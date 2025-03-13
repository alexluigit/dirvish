;;; dirvish-side.el --- Toggle Dirvish in side window like treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.2.7
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Toggle Dirvish in side window like treemacs.

;;; Code:

(require 'dirvish-subtree)

(defcustom dirvish-side-display-alist '((side . left) (slot . -1))
  "Display alist for `dirvish-side' window."
  :group 'dirvish :type 'alist)

(defcustom dirvish-side-width 35
  "Width of the `dirvish-side' buffer."
  :type 'integer :group 'dirvish)

(defcustom dirvish-side-window-parameters
  '((no-delete-other-windows . t) (no-other-window . t))
  "Window parameters for `dirvish-side' window."
  :group 'dirvish :type 'alist)

(defcustom dirvish-side-mode-line-format dirvish-mode-line-format
  "Mode line format used in `dirvish-side' window.
See `dirvish-mode-line-format' for details."
  :group 'dirvish :type 'plist)

(defcustom dirvish-side-header-line-format '(:left (project))
  "Header line format used in `dirvish-side' window.
See `dirvish-mode-line-format' for details."
  :group 'dirvish :type 'plist)

(defcustom dirvish-side-attributes dirvish-attributes
  "File attributes used in `dirvish-side' window.
See `dirvish-attributes' for details."
  :group 'dirvish :type '(repeat (symbol :tag "Dirvish attribute")))

(defcustom dirvish-side-open-file-action 'mru
  "The action of how to open a file in side window.
The value can be one of:

- \\='mru    - open the file in the most-recent-used window.
- \\='split  - open the file below the mru window.
- \\='vsplit - open the file in a vertical split window.
- a function that returns a target window for the file buffer,
  such as `ace-select-window'."
  :group 'dirvish
  :type '(choice (const :tag "open the file in the most-recent-used window" mru)
                 (const :tag "open the file below the mru window" split)
                 (const :tag "open the file in a vertical split window" vsplit)
                 (function :tag "custom function")))

(defcustom dirvish-side-auto-close nil
  "Whether to auto close the side session after opening a file."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-side-auto-expand t
  "Whether to auto expand parent directories of current file.
If non-nil, expand all the parent directories of current buffer's
filename until the project root when opening a side session."
  :group 'dirvish :type 'boolean)

(defun dirvish-side-open-file-fn ()
  "Called before opening a file in side sessions."
  (when (dv-curr-layout (dirvish-curr)) (dirvish-layout-toggle))
  (when dirvish-side-auto-close (quit-window))
  (let* ((mru (get-mru-window nil nil t)))
    (select-window (cond ((functionp dirvish-side-open-file-action)
                          (funcall dirvish-side-open-file-action))
                         ((eq dirvish-side-open-file-action 'mru) mru)
                         ((eq dirvish-side-open-file-action 'split)
                          (with-selected-window mru (split-window-below)))
                         ((eq dirvish-side-open-file-action 'vsplit)
                          (with-selected-window mru (split-window-right)))))))

(defun dirvish-side-root-conf-fn (buffer)
  "Setup BUFFER for side session."
  (let ((name (buffer-name buffer)))
    (unless (dirvish-prop :side-buf-renamed) ; hide it by prefix with " "
      (rename-buffer (format " *SIDE::%s" name))
      (dirvish-prop :side-buf-renamed t))))

(defun dirvish-side-root-window-fn (dv)
  "Create root window of DV according to `dirvish-side-display-alist'."
  (let* ((buf (with-current-buffer (get-buffer-create " *dirvish-temp*")
                ;; set the :dv prop for `dirvish-curr'
                (dirvish-prop :dv (dv-id dv))
                (current-buffer)))
         (win (display-buffer-in-side-window
               buf (append '((dedicated . t)) dirvish-side-display-alist))))
    (cl-loop for (key . value) in dirvish-side-window-parameters
             do (set-window-parameter win key value))
    (with-selected-window win
      (let ((w (max dirvish-side-width window-min-width)) window-size-fixed)
        (cond ((> (window-width) w)
               (shrink-window-horizontally  (- (window-width) w)))
              ((< (window-width) w)
               (enlarge-window-horizontally (- w (window-width)))))))
    (select-window win)))

(defun dirvish-side--session-visible-p ()
  "Return the root window of visible side session."
  (cl-loop
   for w in (window-list)
   for b = (window-buffer w)
   for dv = (with-current-buffer b (dirvish-curr))
   thereis (and dv (eq 'side (dv-type dv)) w)))

(defun dirvish-side--auto-jump ()
  "Select latest buffer file in the visible `dirvish-side' session."
  ;; some commands such as `consult-buffer' that uses minibuffer somehow causes
  ;; a delay on `current-buffer' updating, so we wait for 0.01s here to ensure
  ;; the latest buffer is correctly grabbed. (#264)
  (run-with-timer
   0.01 nil
   (lambda ()
     (when-let* (((not (dirvish-curr)))
                 ((not (active-minibuffer-window)))
                 (win (dirvish-side--session-visible-p))
                 (dv (with-current-buffer (window-buffer win) (dirvish-curr)))
                 (dir (or (dirvish--get-project-root) default-directory))
                 (prev (with-selected-window win (dirvish-prop :index)))
                 (curr buffer-file-name)
                 ((not (string-suffix-p "COMMIT_EDITMSG" curr)))
                 ((not (equal prev curr))))
       (with-selected-window win
         (let (buffer-list-update-hook)
           (dirvish--find-entry 'find-alternate-file dir))
         (if dirvish-side-auto-expand (dirvish-subtree-expand-to curr t)
           (dired-goto-file curr))
         (dirvish--update-display))))))

(defun dirvish-side--new (path)
  "Open a side session in PATH."
  (let* ((bname buffer-file-name)
         (dv (or (dirvish--get-session 'type 'side)
                 (dirvish--new
                  :type 'side
                  :size-fixed 'width
                  :dedicated t
                  :root-conf #'dirvish-side-root-conf-fn
                  :root-window-fn #'dirvish-side-root-window-fn
                  :open-file-fn #'dirvish-side-open-file-fn)))
         (r-win (dv-root-window dv)))
    (setq r-win (dirvish--create-root-window dv))
    (with-selected-window r-win
      (dirvish--find-entry 'find-alternate-file path)
      (cond ((not bname) nil)
            (dirvish-side-auto-expand
             (dirvish-subtree-expand-to bname))
            (t (dired-goto-file bname)))
      (dirvish--update-display))))

;;;###autoload
(define-minor-mode dirvish-side-follow-mode
  "Toggle `dirvish-side-follow-mode'.
When enabled the visible side session will select the current
buffer's filename.  It will also visits the latest `project-root'
after switching to a new project."
  :init-value nil :global t :group 'dirvish
  (if dirvish-side-follow-mode
      (add-hook 'buffer-list-update-hook #'dirvish-side--auto-jump)
    (remove-hook 'buffer-list-update-hook #'dirvish-side--auto-jump)))

;;;###autoload
(defun dirvish-side (&optional path)
  "Toggle a Dirvish session at the side window.

- If the current window is a side session window, hide it.
- If a side session is visible, select it.
- If a side session exists but is not visible, show it.
- If there is no side session exists,create a new one with PATH.

If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to `project-current'."
  (interactive (list (and current-prefix-arg
                          (read-directory-name "Open sidetree: "))))
  (let ((fullframep (when-let* ((dv (dirvish-curr))) (dv-curr-layout dv)))
        (visible (dirvish-side--session-visible-p))
        (path (or path (dirvish--get-project-root) default-directory)))
    (cond (fullframep (user-error "Can not create side session here"))
          ((eq visible (selected-window)) (dirvish-quit))
          (visible (select-window visible))
          (t (dirvish-side--new path)))))

(provide 'dirvish-side)
;;; dirvish-side.el ends here

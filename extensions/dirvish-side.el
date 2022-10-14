;;; dirvish-side.el --- Toggle Dirvish in side window like treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Toggle Dirvish in side window like treemacs.

;;; Code:

(require 'dirvish-subtree)

(defcustom dirvish-side-display-alist
  '((side . left) (slot . -1))
  "Display alist for `dirvish-side' window."
  :group 'dirvish :type 'alist)

(defcustom dirvish-side-width 35
  "Width of the `dirvish-side' buffer."
  :type 'integer :group 'dirvish)

(defcustom dirvish-side-window-parameters
  '((no-delete-other-windows . t) (no-other-window . t))
  "Window parameters for `dirvish-side' window."
  :group 'dirvish :type 'alist)

(define-obsolete-variable-alias 'dirvish-side-open-file-window-function 'dirvish-side-open-file-action "Sep 23, 2022")
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

(define-obsolete-variable-alias 'dirvish-side-follow-buffer-file 'dirvish-side-auto-expand "Sep 15, 2022")
(defcustom dirvish-side-auto-expand t
  "Whether to auto expand parent directories of current file.
If non-nil, expand all the parent directories of current buffer's
filename until the project root when opening a side session."
  :group 'dirvish :type 'boolean)

(defconst dirvish-side-header (dirvish--mode-line-fmt-setter '(project) nil t))

(defun dirvish-side-file-open-fn ()
  "Called before opening a file in side sessions."
  (let* ((dv (dirvish-curr)) (layout (car (dv-layout dv)))
         (mru (get-mru-window nil nil t)))
    (if layout (dirvish-kill dv)
      (when dirvish-side-auto-close
        (dirvish-kill dv)
        (when dirvish-reuse-session (quit-window)))
      (select-window (cond ((functionp dirvish-side-open-file-action)
                            (funcall dirvish-side-open-file-action))
                           ((eq dirvish-side-open-file-action 'mru) mru)
                           ((eq dirvish-side-open-file-action 'split)
                            (with-selected-window mru (split-window-below)))
                           ((eq dirvish-side-open-file-action 'vsplit)
                            (with-selected-window mru (split-window-right))))))))

(defun dirvish-side-root-window-fn ()
  "Create root window according to `dirvish-side-display-alist'."
  (let ((win (display-buffer-in-side-window
              (dirvish--util-buffer "temp") dirvish-side-display-alist)))
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
   thereis (and dv (eq 'side (car (dv-type dv))) w)))

(defun dirvish-side--auto-jump ()
  "Select latest buffer file in the visible `dirvish-side' session."
  (run-with-timer
   0.5 nil
   (lambda ()
     (when-let* (((not dirvish--this))
                 (dir (or (dirvish--get-project-root) default-directory))
                 (win (dirvish-side--session-visible-p))
                 (dv (with-selected-window win (dirvish-curr)))
                 ((not (active-minibuffer-window)))
                 (file buffer-file-name))
       (with-selected-window win
         (when dir
           (setq dirvish--this dv)
           (let (buffer-list-update-hook) (dirvish-find-entry-a dir))
           (if dirvish-side-auto-expand (dirvish-subtree-expand-to file)
             (dired-goto-file file))
           (dirvish-prop :cus-header 'dirvish-side-header)
           (dirvish--setup-mode-line (car (dv-layout dv)))
           (dirvish-update-body-h))
         (setq dirvish--this nil))))))

(defun dirvish-side--new (path)
  "Open a side session in PATH."
  (let* ((bname buffer-file-name)
         (dv (or (car (dirvish--find-reusable 'side))
                 (dirvish-new :type '(side width dedicated
                                      dirvish-side-root-window-fn
                                      dirvish-side-file-open-fn))))
         (r-win (dv-root-window dv)))
    (unless (window-live-p r-win) (setq r-win (dirvish--create-root-window dv)))
    (with-selected-window r-win
      (setq dirvish--this dv)
      (dirvish-find-entry-a path)
      (cond ((not bname) nil)
            (dirvish-side-auto-expand
             (dirvish-subtree-expand-to bname))
            (t (dired-goto-file bname)))
      (dirvish-prop :cus-header 'dirvish-side-header)
      (dirvish-update-body-h))))

(dirvish-define-mode-line project
  "Return a string showing current project."
  (let ((project (dirvish--get-project-root))
        (face (if (dirvish--window-selected-p dv) 'dired-header 'shadow)))
    (if project
        (setq project (file-name-base (directory-file-name project)))
      (setq project "-"))
    (format " %s %s"
            (propertize "Project:" 'face face)
            (propertize project 'face 'font-lock-string-face))))

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
  (let ((fullframep (when-let ((dv (dirvish-curr))) (car (dv-layout dv))))
        (visible (dirvish-side--session-visible-p))
        (path (or path (dirvish--get-project-root) default-directory)))
    (cond (fullframep (user-error "Can not create side session here"))
          ((eq visible (selected-window)) (dirvish-quit))
          (visible (select-window visible))
          (t (dirvish-side--new path)))))

(provide 'dirvish-side)
;;; dirvish-side.el ends here

;;; dirvish-side.el --- Toggle Dirvish in side window like treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.3.0
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

(defcustom dirvish-side-open-file-action nil
  "Action to perform before opening a file in a side window.
The value is a function called before switching to the file buffer.  The
most recent used window is select if it is nil."
  :group 'dirvish
  :type '(choice (const :tag "open the file in the most-recent-used window" nil)
                 (function :tag "custom function")))

(defcustom dirvish-side-auto-expand t
  "Whether to auto expand parent directories of current file.
If non-nil, expand all the parent directories of current buffer's
filename until the project root when opening a side session."
  :group 'dirvish :type 'boolean)

(defun dirvish-side-root-conf (buffer)
  "Setup BUFFER for side session."
  (let ((name (buffer-name buffer)))
    (unless (string-prefix-p " *SIDE :: " name)
      (rename-buffer (format " *SIDE :: %s :: %s" ; hide it by prefix with " "
                             (file-name-base (directory-file-name
                                              default-directory))
                             (dirvish--timestamp))))))

(defun dirvish-side-root-window-fn (dv)
  "Create root window of DV according to `dirvish-side-display-alist'."
  (let* ((buf (with-current-buffer (get-buffer-create " *dirvish-temp*")
                ;; set the :dv prop for `dirvish-curr'
                (setq window-size-fixed 'width)
                (dirvish-prop :dv (dv-id dv))
                (current-buffer)))
         (win (display-buffer-in-side-window
               buf (append '((dedicated . t)) dirvish-side-display-alist))))
    (cl-loop for (key . value) in dirvish-side-window-parameters
             do (set-window-parameter win key value))
    (with-selected-window win ; Set window width to `dirvish-side-width'
      (let ((w (max dirvish-side-width window-min-width))
            window-size-fixed) ; Temporarily unfix size for initial adjustment
        ;; Ignore errors during resizing (eg. already minimum)
        (ignore-errors (enlarge-window-horizontally (- w (window-width))))))
    (select-window win)))

(defun dirvish-side-open-file (dv find-fn file)
  "Open FILE using FIND-FN for default DV sessions."
  (let ((idx (current-buffer)) fbuf)
    (unwind-protect (if (eq find-fn 'find-file-other-window)
                        (funcall find-fn file) ; a new window is split
                      (dirvish-save-dedication (funcall find-fn file)))
      (cond ((eq (setq fbuf (current-buffer)) idx) nil)
            ((eq find-fn 'find-file-other-window) (dirvish--clear-session dv))
            (t (dirvish--clear-session dv)
               (setf (dv-curr-layout dv) nil)
               (if (buffer-live-p idx) ; `find-alternate-file' kills idx
                   (dirvish-save-dedication (switch-to-buffer idx))
                 (delete-window))
               (when (dirvish-curr) (other-window 1))
               (when (functionp dirvish-side-open-file-action)
                 (funcall dirvish-side-open-file-action))
               (dirvish-save-dedication (switch-to-buffer fbuf)))))))

(defun dirvish-side--session-visible-p ()
  "Return the root window of visible side session."
  (cl-loop
   for w in (window-list)
   for b = (window-buffer w)
   for dv = (with-current-buffer b (dirvish-curr))
   thereis (and dv (eq 'side (dv-type dv)) w)))

(defun dirvish-side--auto-jump ()
  "Select latest buffer file in the visible `dirvish-side' session."
  (when-let* (((not (dirvish-curr)))
              ((not (active-minibuffer-window)))
              (win (dirvish-side--session-visible-p))
              (dv (with-current-buffer (window-buffer win) (dirvish-curr)))
              (dir (or (dirvish--vc-root-dir) default-directory))
              (prev (with-selected-window win (dirvish-prop :index)))
              (curr buffer-file-name)
              ((not (string-suffix-p "COMMIT_EDITMSG" curr)))
              ((not (equal prev curr))))
    (with-selected-window win
      (let (buffer-list-update-hook window-buffer-change-functions)
        (or (cl-loop for (d . _) in dired-subdir-alist
                     if (string-prefix-p d (expand-file-name dir))
                     return (dired-goto-subdir d))
            (dirvish--find-entry 'find-alternate-file dir)))
      ;; delay the running of this hook to eliminate race condition
      (dirvish-winbuf-change-h win)
      (unwind-protect (if dirvish-side-auto-expand
                          (dirvish-subtree-expand-to curr)
                        (dired-goto-file curr))
        (dirvish--redisplay)))))

(defun dirvish-side--new (path)
  "Open a side session in PATH."
  (let ((bname buffer-file-name)
        (dv (or (dirvish--get-session 'type 'side)
                (dirvish--new
                 :type 'side
                 :size-fixed 'width
                 :dedicated t
                 :root-conf #'dirvish-side-root-conf
                 :root-window-fn #'dirvish-side-root-window-fn
                 :open-file #'dirvish-side-open-file))))
    (with-selected-window (dirvish--create-root-window dv)
      (dirvish--find-entry 'find-alternate-file path)
      (cond ((not bname) nil)
            (dirvish-side-auto-expand
             (dirvish-subtree-expand-to bname))
            (t (dired-goto-file bname))))))

(defun dirvish-side-increase-width (delta)
  "Increase width of the `dirvish-side' window by DELTA columns.
Interactively, if no argument is given, DELTA is seen as 1."
  (interactive "^p")
  (let ((win (dirvish-side--session-visible-p)))
    (unless win (user-error "No visible dirvish-side window found"))
    (with-selected-window win
      (let ((window-size-fixed nil))
        (ignore-errors (enlarge-window-horizontally delta))))))

(defun dirvish-side-decrease-width (delta)
  "Decrease width of the `dirvish-side' window by DELTA columns.
Interactively, if no argument is given, DELTA is seen as 1."
  (interactive "^p")
  (dirvish-side-increase-width (- delta)))

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
- If there is no side session exists, create a new one with PATH.

If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to `project-current'."
  (interactive (list (and current-prefix-arg
                          (read-directory-name "Open sidetree: "))))
  (let ((fullframep (when-let* ((dv (dirvish-curr))) (dv-curr-layout dv)))
        (visible (dirvish-side--session-visible-p))
        (path (or path (dirvish--vc-root-dir) default-directory)))
    (cond (fullframep (user-error "Can not create side session here"))
          ((eq visible (selected-window)) (dirvish-quit))
          (visible (select-window visible))
          (t (dirvish-side--new path)))))

(provide 'dirvish-side)
;;; dirvish-side.el ends here

;;; dirvish-side.el --- Toggle Dirvish in side window like treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.9.23
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Toggle Dirvish in side window like treemacs.

;;; Code:

(require 'dirvish)

(defcustom dirvish-side-attributes dirvish-attributes
  "Same as `dirvish-attributes', but for side sessions."
  :group 'dirvish :type '(repeat dirvish-attribute))

(defcustom dirvish-side-preview-dispatchers dirvish-preview-dispatchers
  "Same as `dirvish-preview-dispatchers', but for side sessions."
  :group 'dirvish :type 'list)

(defcustom dirvish-side-header-line-format
  '(:left (project) :right ())
  "Same as `dirvish-header-line-format', but for side sessions."
  :group 'dirvish :type 'plist
  :set (lambda (k v) (set k (dirvish--mode-line-fmt-setter v t))))

(defcustom dirvish-side-mode-line-format
  '(:left (sort omit) :right (index))
  "Same as `dirvish-mode-line-format', but for side sessions."
  :group 'dirvish :type 'plist
  :set (lambda (k v) (set k (dirvish--mode-line-fmt-setter v))))

(defcustom dirvish-side-display-alist
  '((side . left) (slot . -1) (window-width . 0.2))
  "Display alist for `dirvish-side' window."
  :group 'dirvish :type 'alist)

(defcustom dirvish-side-window-parameters '((no-delete-other-windows . t))
  "Window parameters for `dirvish-side' window."
  :group 'dirvish :type 'alist)

(defcustom dirvish-side-open-file-window-function
  (lambda () (get-mru-window nil nil t))
  "A function that returns a window for the `find-file' buffer.
This function is called before opening files in a `dirvish-side'
session.  For example, if you have `ace-window' installed, you
can set it to `ace-select-window', which prompts you for a target
window to place the file buffer.  Note that if this value is
`selected-window', the session closes after opening a file."
  :group 'dirvish :type 'function)

(defcustom dirvish-side-follow-buffer-file nil
  "Whether to follow current buffer's filename.
If this variable is non-nil, when the current buffer is visiting
a file, the summoned side sessions updates its index path
according to the filename."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-side-follow-project-switch t
  "Whether visible side session update index on project switch.
If this variable is non-nil, the visible `dirvish-side' session
will visit the latest `project-root' after executing
`project-switch-project' or `projectile-switch-project'."
  :group 'dirvish :type 'boolean
  :set
  (lambda (key enabled)
    (set key enabled)
    (if enabled
        (progn
          (and (fboundp 'project-switch-project)
               (advice-add 'project-switch-project :after #'dirvish-side-find-file))
          (add-hook 'projectile-after-switch-project-hook #'dirvish-side-find-file))
      (and (fboundp 'project-switch-project)
           (advice-remove 'project-switch-project #'dirvish-side-find-file))
      (remove-hook 'projectile-after-switch-project-hook #'dirvish-side-find-file))))

(defun dirvish-side-on-file-open (dv)
  "Called before opening a file in Dirvish-side session DV."
  (unless (dv-layout dv)
    (select-window (funcall dirvish-side-open-file-window-function)))
  (dirvish-focus-change-h)
  (when-let ((dv (dirvish-curr))) (dirvish-kill dv)))

(defun dirvish-side-winconf-change-h ()
  "Adjust width of side window on window configuration change."
  (let ((dv (dirvish-curr))
        window-size-fixed window-configuration-change-hook)
    (unless (dv-layout dv)
      (window--display-buffer (window-buffer) (get-buffer-window)
                              'reuse dirvish-side-display-alist))))

(defun dirvish-side-root-window-fn ()
  "Create root window according to `dirvish-side-display-alist'."
  (let ((win (display-buffer-in-side-window
              (dirvish--util-buffer) dirvish-side-display-alist)))
    (cl-loop for (key . value) in dirvish-side-window-parameters
             do (set-window-parameter win key value))
    (select-window win)))

(defun dirvish-side--session-visible-p ()
  "Return the root window of visible side session."
  (cl-loop
   for w in (window-list)
   for b = (window-buffer w)
   for dv = (with-current-buffer b (dirvish-prop :dv))
   thereis (and dv (eq 'dirvish-side-root-window-fn (dv-root-window-fn dv)) w)))

(defun dirvish-side-find-file (&optional filename)
  "Visit FILENAME in current visible `dirvish-side' session."
  (when-let ((win (dirvish-side--session-visible-p))
             (dirname (or (and filename (file-name-directory filename))
                          (dirvish--get-project-root))))
    (with-selected-window win (dirvish-find-entry-ad dirname))))

;;;###autoload (autoload 'dirvish-project-ml "dirvish-side" nil t)
(dirvish-define-mode-line project
  "Return a string showing current project."
  (let ((project (dirvish--get-project-root)))
    (if project
        (setq project (file-name-base (directory-file-name project)))
      (setq project "-"))
    (format " %s [%s]"
            (propertize "Project:" 'face 'bold)
            (propertize project 'face 'font-lock-string-face))))

;;;###autoload
(cl-defun dirvish-side (&optional path)
  "Toggle a Dirvish session at the side window.
- If the side window is visible hide it.
- If a side session exists but is not visible, show it.
- If there is no session exists within the scope,
  create the session with PATH and display it.

If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to `project-current'."
  (interactive (list (and current-prefix-arg (read-file-name "Dirvish side: "))))
  (let* ((dv (dirvish-prop :dv))
         (visible-side-win (dirvish-side--session-visible-p))
         (followed (buffer-file-name)))
    (cond ((and dv (dv-layout dv))
           (user-error "Can not create side session here"))
          (visible-side-win
           (with-selected-window visible-side-win
             (let ((dirvish-reuse-session t))
               (dirvish-quit)
               (cl-return-from dirvish-side)))))
    (dirvish--reuse-session)
    (when (and (dirvish-prop :dv) dirvish-side-follow-buffer-file followed)
      (dirvish-find-entry-ad (file-name-directory followed))
      (dired-goto-file followed))
    (unless (dirvish-prop :dv)
      (dirvish-new
        :path (or (and path (file-name-directory path))
                  (dirvish--get-project-root)
                  default-directory)
        :attributes dirvish-side-attributes
        :preview-dispatchers dirvish-side-preview-dispatchers
        :mode-line-format dirvish-side-mode-line-format
        :header-line-format dirvish-side-header-line-format
        :root-window-fn #'dirvish-side-root-window-fn
        :on-file-open #'dirvish-side-on-file-open
        :on-winconf-change #'dirvish-side-winconf-change-h))))

(provide 'dirvish-side)
;;; dirvish-side.el ends here

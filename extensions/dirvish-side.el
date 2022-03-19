;;; dirvish-side.el --- Toggle Dirvish in side window like treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.0.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (dirvish "1.0.0"))

;;; Commentary:

;; Toggle Dirvish in side window like treemacs.

;;; Code:

(require 'dirvish)
(declare-function get-current-persp "persp-mode")
(declare-function persp-curr "perspective")

(defvar dirvish-side--state-alist '())
(defvar dirvish-side-scope-fn nil)

(defcustom dirvish-side-scope 'tab
  "SCOPE for Dirvish side window.
Every SCOPE only have one (toggleable) side Dirvish session.
SCOPE can be `emacs', `tab', `frame', `persp', or `perspective'."
  :group 'dirvish :type 'symbol
  :options '(emacs tab frame persp)
  :set
  (lambda (k v)
    (set k v)
    (cl-case v
      ('tab
       (add-hook 'tab-bar-tab-pre-close-functions #'dirvish-side--remove-state)
       (setq dirvish-side-scope-fn #'tab-bar--current-tab-index))
      ('frame
       (add-hook 'delete-frame-functions #'dirvish-side--remove-state)
       (setq dirvish-side-scope-fn #'selected-frame))
      ('persp
       (if (require 'persp-mode nil t)
           (progn
             (add-hook 'persp-before-kill-functions #'dirvish-side--remove-state)
             (add-hook 'persp-activated-functions
                       (lambda (_scope) (when (car (dirvish-side--get-state)) (dotimes (_ 2) (dirvish-side)))))
             (setq dirvish-side-scope-fn (lambda () (or (get-current-persp) 'none))))
         (set k 'tab)
         (user-error "Unable to find package `persp-mode'")))
      ('perspective
       (if (require 'perspective nil t)
           (progn
             (add-hook 'persp-killed-hook #'dirvish-side--remove-state)
             (setq dirvish-side-scope-fn #'persp-curr))
         (set k 'tab)
         (user-error "Unable to find package `perspective'"))))
    (cl-loop for (_scope . (dv . state)) in dirvish-side--state-alist
             do (when dv (dirvish-deactivate dv)))
    (setq dirvish-side--state-alist '())))

(defcustom dirvish-side-display-alist
  '((side . left)
    (slot . -1)
    (window-width . 0.2)
    (window-parameters . ((no-delete-other-windows . t))))
  "Display alist for `dirvish-side' window."
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

(defun dirvish-side--get-state ()
  "Get state of side session for current scope."
  (or (alist-get (funcall dirvish-side-scope-fn) dirvish-side--state-alist)
      (cons nil 'uninitialized)))

(defun dirvish-side--set-state (dv state)
  "Set state of current scope to DV and its STATE."
  (setf (alist-get (funcall dirvish-side-scope-fn) dirvish-side--state-alist) (cons dv state)))

(defun dirvish-side--remove-state (scope &rest _)
  "Remove invalid state info within SCOPE."
  (let* ((dv-w/state (alist-get scope dirvish-side--state-alist))
         (dv (car-safe dv-w/state)))
    (when dv (dirvish-deactivate dv))
    (setq dirvish-side--state-alist
          (delq (assoc scope dirvish-side--state-alist) dirvish-side--state-alist))))

(defun dirvish-side-find-file-window-fn ()
  "Return a window for opening files in `dirvish-side'."
  (if (window-parameter (selected-window) 'window-side)
      (funcall dirvish-side-open-file-window-function)
    (selected-window)))

(defun dirvish-side-quit-window-fn (_dv)
  "Quit window action for `dirvish-side'."
  (dirvish-side--set-state nil 'uninitialized)
  (when (window-parameter (selected-window) 'window-side) (delete-window)))

(defun dirvish-side-root-window-fn ()
  "Display a window according to `dirvish-side-display-alist'."
  (select-window
   (display-buffer-in-side-window (dirvish--ensure-temp-buffer) dirvish-side-display-alist)))

(defun dirvish-side-header-string-fn ()
  "Return a string showing current project."
  (when-let ((dv (dirvish-curr)))
    (with-current-buffer (window-buffer (dv-root-window dv))
      (let ((project (dirvish--get-project-root)))
        (if project
            (setq project (file-name-base (directory-file-name project)))
          (setq project "-"))
        (format " %s [%s]"
                (propertize "Project:" 'face 'bold)
                (propertize project 'face 'font-lock-string-face))))))

;;;###autoload
(defun dirvish-side (&optional path)
  "Toggle a Dirvish session at the side window.
- If the side window is visible hide it.
- If a side session within the current `dirvish-side-scope'
  exists but is not visible, show it.
- If there is no session exists within the scope,
  create the session with PATH and display it.

If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to `project-current'."
  (interactive (list (and current-prefix-arg (read-file-name "Dirvish side: "))))
  (cl-destructuring-bind (dv . state) (dirvish-side--get-state)
    (cl-case state
      ('visible
       (unless (dirvish-dired-p dv) (dirvish-toggle-fullscreen))
       (delete-window (get-buffer-window (cdar (dv-root-dir-buf-alist dv))))
       (dirvish-side--set-state dv 'exists))
      ('exists
       (with-selected-window (dirvish--create-root-window dv)
         (switch-to-buffer (cdar (dv-root-dir-buf-alist dv)))
         (dirvish-reclaim)
         (dirvish-build))
       (dirvish-side--set-state dv 'visible))
      ('uninitialized
       (dirvish-activate
        (dirvish-new
          :path (or (and path (file-name-directory path))
                    (dirvish--get-project-root)
                    (dirvish--ensure-path))
          :depth -1
          :type 'side))
       (dirvish-side--set-state (dirvish-curr) 'visible)))))

(provide 'dirvish-side)
;;; dirvish-side.el ends here

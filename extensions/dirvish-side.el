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
      ('tab (add-hook 'tab-bar-tab-pre-close-functions #'dirvish-side--remove-state))
      ('frame (add-hook 'delete-frame-functions #'dirvish-side--remove-state))
      ('persp
       (if (require 'persp-mode nil t)
           (progn
             (add-hook 'persp-before-kill-functions #'dirvish-side--remove-state)
             (add-hook 'persp-activated-functions
                       (lambda (_scope) (when (car (dirvish-side--get-state)) (dotimes (_ 2) (dirvish-side))))))
         (set k 'tab)
         (user-error "Unable to find package `persp-mode'")))
      ('perspective
       (if (require 'perspective nil t)
           (add-hook 'persp-killed-hook #'dirvish-side--remove-state)
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

(defun dirvish-side--scope ()
  (cl-case dirvish-side-scope
    ('emacs 'emacs)
    ('tab (tab-bar--current-tab-index))
    ('frame (selected-frame))
    ('persp (or (get-current-persp) 'none))
    ('perspective (persp-curr))))

(defun dirvish-side--get-state ()
  (or (alist-get (dirvish-side--scope) dirvish-side--state-alist)
      (cons nil 'uninitialized)))

(defun dirvish-side--set-state (dv state)
  (setf (alist-get (dirvish-side--scope) dirvish-side--state-alist) (cons dv state)))

(defun dirvish-side--remove-state (scope &rest _)
  (let* ((dv-w/state (alist-get scope dirvish-side--state-alist))
         (dv (car-safe dv-w/state)))
    (when dv (dirvish-deactivate dv))
    (setq dirvish-side--state-alist
          (delq (assoc scope dirvish-side--state-alist) dirvish-side--state-alist))))

(defun dirvish-side-find-file-window-fn ()
  (if (window-parameter (selected-window) 'window-side)
      (funcall dirvish-side-open-file-window-function)
    (selected-window)))

(defun dirvish-side-quit-window-fn (_dv)
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
  "Open Dirvish in side window with optional PATH.
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
          :path (or path (dirvish--get-project-root) (dirvish--ensure-path))
          :depth -1
          :type 'side))
       (dirvish-side--set-state (dirvish-curr) 'visible)))))

(provide 'dirvish-side)
;;; dirvish-side.el ends here

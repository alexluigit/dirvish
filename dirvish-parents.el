;;; dirvish-parents.el --- Parent windows for Dirvish. -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Creating parent windows for dirvish.  A parent window is a window that holds a dirvish buffer,
;;; which exhibit information of parent directory for window on the right side.

;;; Code:

(declare-function dired-hide-details-mode "dired")
(declare-function dired-goto-file "dired")
(declare-function dirvish-mode "dirvish")
(declare-function all-the-icons-dired-mode "all-the-icons-dired")
(require 'dirvish-structs)
(require 'dirvish-vars)
(require 'dirvish-header)
(require 'dirvish-body)
(require 'dirvish-helpers)

(defun dirvish-parent-build ()
  "Create all dirvish parent windows."
  (cl-flet ((setup (child win buf one-w)
              (when child (dired-goto-file child))
              (push win (dv-parent-windows (dirvish-curr)))
              (push buf (dv-parent-buffers (dirvish-curr)))
              (dirvish-mode)
              (when (bound-and-true-p all-the-icons-dired-mode)
                (all-the-icons-dired-mode -1)
                (setq-local tab-width 2))
              (dirvish--parent-default-config win)
              (dirvish--header-setup (if one-w 'one-window 'posframe))))
    (let* ((current (expand-file-name default-directory))
           (parent (dirvish--get-parent current))
           (parent-dirs ())
           (one-window-p (dv-one-window-p (dirvish-curr)))
           (depth dirvish-depth)
           (i 0))
      (if one-window-p (setq depth 0) (delete-other-windows))
      (setup dirvish-child-entry (dv-root-window (dirvish-curr)) (current-buffer) one-window-p)
      (while (and (< i depth) (not (string= current parent)))
        (setq i (+ i 1))
        (push (cons current parent) parent-dirs)
        (setq current (dirvish--get-parent current))
        (setq parent (dirvish--get-parent parent)))
      (when (> depth 0)
        (let* ((remain (- 1 dirvish-preview-width dirvish-parent-max-width))
               (width (min (/ remain depth) dirvish-parent-max-width))
               dired-after-readin-hook)
          (cl-dolist (parent-dir parent-dirs)
            (let* ((current (car parent-dir))
                   (parent (cdr parent-dir))
                   (win-alist `((side . left)
                                (inhibit-same-window . t)
                                (window-width . ,width)))
                   (buffer (dired-noselect parent))
                   (window (display-buffer buffer `(dirvish--display-buffer . ,win-alist))))
              (with-selected-window window
                (setup current window buffer one-window-p)
                (dired-hide-details-mode t)
                (dirvish-body-update))))))
      (when dirvish-enable-preview (dired-hide-details-mode t)))))

(defun dirvish--parent-default-config (win)
  "Apply default config for dirvish parent window WIN."
  (setq cursor-type nil)
  (setq mode-line-format nil)
  (setq-local face-font-rescale-alist nil)
  (set-window-fringes win 1 1))

(provide 'dirvish-parents)

;;; dirvish-parents.el ends here

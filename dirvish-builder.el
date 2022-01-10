;;; dirvish-builder.el ---  Build a Dirvish layout in a window or frame -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; This library provides functions for building a dirvish layout.

;;; Code:

(declare-function all-the-icons-dired-mode "all-the-icons-dired")
(require 'dirvish-updater)
(require 'dirvish-preview)
(require 'dirvish-structs)
(require 'dirvish-options)
(require 'dirvish-helpers)

(defun dirvish-revert (&optional _arg _noconfirm)
  "Reread the Dirvish buffer.
Dirvish sets `revert-buffer-function' to this function.  See
`dired-revert'."
  (dirvish-with-update t
    (dired-revert)
    (dirvish-setup-dired-buffer)))

(defun dirvish-setup ()
  "Default config for dirvish parent windows."
  (dirvish-mode)
  (dirvish-setup-dired-buffer)
  (setq-local revert-buffer-function #'dirvish-revert)
  (set (make-local-variable 'face-remapping-alist)
       dirvish-parent-face-remap-alist)
  (setq-local face-font-rescale-alist nil)
  (setq cursor-type nil)
  (set-window-fringes nil 1 1)
  (when (bound-and-true-p all-the-icons-dired-mode)
    (all-the-icons-dired-mode -1)
    (setq-local tab-width 2))
  (when dirvish-child-entry (dired-goto-file dirvish-child-entry))
  (dirvish-body-update)
  (let* ((dv (dirvish-curr))
         (owp (dv-one-window-p dv)))
    (push (selected-window) (dv-parent-windows dv))
    (push (current-buffer) (dv-parent-buffers dv))
    (setq-local dirvish--curr-name (dv-name dv))
    (setq mode-line-format (and owp dirvish-mode-line-format
                                '((:eval (dirvish-format-mode-line)))))
    (setq header-line-format (and owp dirvish-header-line-format
                                  '((:eval (format-mode-line dirvish-header-line-format)))))
    (if (eq (dv-root-window dv) (selected-window))
        (and dirvish-enable-preview (dired-hide-details-mode t))
      (dired-hide-details-mode t))))

(defun dirvish-build-parents ()
  "Create all dirvish parent windows."
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish--get-parent current))
         (parent-dirs ())
         (dv (dirvish-curr))
         (one-window-p (dv-one-window-p dv))
         (depth dirvish-depth)
         (i 0))
    (when one-window-p (setq depth 0))
    (dirvish-setup)
    (while (and (< i depth) (not (string= current parent)))
      (setq i (+ i 1))
      (push (cons current parent) parent-dirs)
      (setq current (dirvish--get-parent current))
      (setq parent (dirvish--get-parent parent)))
    (when (> depth 0)
      (let* ((remain (- 1 dirvish-preview-width dirvish-parent-max-width))
             (width (min (/ remain depth) dirvish-parent-max-width))
             (dired-after-readin-hook nil))
        (cl-dolist (parent-dir parent-dirs)
          (let* ((current (car parent-dir))
                 (parent (cdr parent-dir))
                 (win-alist `((side . left)
                              (inhibit-same-window . t)
                              (window-width . ,width)))
                 (buffer (dired-noselect parent))
                 (window (display-buffer buffer `(dirvish--display-buffer . ,win-alist))))
            (with-selected-window window
              (setq-local dirvish-child-entry current)
              (dirvish-setup))))))))

(defun dirvish-build-preview ()
  "Build dirvish preview window."
  (when-let* (dirvish-enable-preview
              (dv (dirvish-curr))
              (one-window-p (not (dv-one-window-p dv))))
    (let* ((inhibit-modification-hooks t)
           (buf (dv-preview-buffer dv))
           (win-alist `((side . right) (window-width . ,dirvish-preview-width)))
           (fringe 30)
           (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
      (set-window-fringes new-window fringe fringe nil t)
      (setf (dv-preview-pixel-width (dirvish-curr)) (window-width new-window t))
      (setf (dv-preview-window (dirvish-curr)) new-window))))

(defun dirvish-build-header ()
  "Create a window showing dirvish header."
  (when (and dirvish-header-style
             dirvish-header-line-format
             (not (dv-one-window-p (dirvish-curr))))
    (let* ((inhibit-modification-hooks t)
           (buf (dv-header-buffer (dirvish-curr)))
           (win-alist `((side . above) (window-height . -2)))
           (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
      (setf (dv-header-window (dirvish-curr)) new-window)
      (set-window-buffer new-window buf))))

(defun dirvish-build-footer ()
  "Create a window showing dirvish footer."
  (when (and dirvish-mode-line-format
             (not (dv-one-window-p (dirvish-curr))))
    (let* ((inhibit-modification-hooks t)
           (buf (dv-footer-buffer (dirvish-curr)))
           (win-alist `((side . below) (window-height . -2)))
           (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
      (setf (dv-footer-window (dirvish-curr)) new-window)
      (set-window-buffer new-window buf))))

(defun dirvish-build ()
  "Build dirvish layout."
  (dirvish-with-update nil
    (unless (dv-one-window-p (dirvish-curr))
      (delete-other-windows))
    (dirvish-build-preview)
    (dirvish-build-header)
    (dirvish-build-footer)
    (dirvish-build-parents)))

(define-derived-mode dirvish-mode dired-mode "Dirvish"
  "Convert Dired buffer to a Dirvish buffer."
  :group 'dirvish
  :interactive nil)

(provide 'dirvish-builder)
;;; dirvish-builder.el ends here

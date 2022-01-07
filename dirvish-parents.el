;;; dirvish-parents.el --- Parent windows for Dirvish -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Creating parent windows for dirvish.  A parent window is a window that holds
;;; a dirvish buffer, which exhibit information of parent directory for window
;;; on the right side.  The `body' refers to all visible lines in a dirvish
;;; parent buffer, which is also the target of `dirvish-body-update'.  Updating
;;; only visible part of a buffer gives us better performance especially for
;;; those big directories.

;;; Code:

(declare-function dirvish-mode "dirvish")
(declare-function all-the-icons-dired-mode "all-the-icons-dired")
(declare-function all-the-icons-icon-for-file "all-the-icons")
(declare-function all-the-icons-icon-for-dir "all-the-icons")
(require 'dirvish-structs)
(require 'dirvish-vars)
(require 'dirvish-helpers)

(defun dirvish-parent-build ()
  "Create all dirvish parent windows."
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish--get-parent current))
         (parent-dirs ())
         (one-window-p (dv-one-window-p (dirvish-curr)))
         (depth dirvish-depth)
         (i 0))
    (and one-window-p (setq depth 0))
    (dirvish-mode)
    (dirvish-parent-config)
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
              (dirvish-mode)
              (dirvish-parent-config))))))))

(defun dirvish-parent-config ()
  "Default config for dirvish parent windows."
  (when (bound-and-true-p all-the-icons-dired-mode)
    (all-the-icons-dired-mode -1)
    (setq-local tab-width 2))
  (setq mode-line-format nil))

(defun dirvish-body-update (&optional skip-icons skip-padding)
  "Update attributes in dirvish body.

By default update icons, padding, and current line.  If
SKIP-ICONS is non-nil, do not update icons.  If SKIP-PADDING is
non-nil, do not update padding."
  (unless skip-icons
    (dirvish--body-update-icons))
  (unless skip-padding
    (dirvish--body-update-fontsize))
  (dirvish--body-update-line))

(defun dirvish--body-update-line ()
  "Update highlighting in current dirvish line."
  (remove-overlays (point-min) (point-max) 'dirvish-body t)
  (when-let* ((pos (dired-move-to-filename nil))
              (beg (line-beginning-position))
              (end (line-beginning-position 2))
              (ol (make-overlay beg end)))
    (when dirvish-show-icons
      (remove-overlays beg end 'dirvish-icons t)
      (dirvish--body-render-icon pos 'highlight))
    (overlay-put ol 'dirvish-body t)
    (overlay-put ol 'face 'highlight)))

(defun dirvish--body-update-icons ()
  "Update icon in current dirvish line."
  (when dirvish-show-icons
    (remove-overlays (point-min) (point-max) 'dirvish-icons t)
    (dirvish-body-render 'dirvish--body-render-icon)))

(defun dirvish--body-update-fontsize ()
  "Update paddings in current dirvish buffer."
  (when (> dirvish-body-fontsize-increment 0)
    (save-excursion
      (let ((o (make-overlay (point-min) (point-max))))
        (setq line-spacing dirvish-body-fontsize-increment)
        (overlay-put o 'display `(height ,(1+ dirvish-body-fontsize-increment)))))))

(defun dirvish--body-render-icon (pos &optional face)
  "Render icon in POS with optional FACE."
  (let* ((entry (dired-get-filename 'relative 'noerror))
         (offset `(:v-adjust ,dirvish-icon-v-offset))
         (icon-face (or (when face `(:face ,face))
                        (when dirvish-icon-monochrome `(:face ,(face-at-point)))))
         (icon-attrs (append icon-face offset))
         (icon (if (file-directory-p entry)
                   (apply #'all-the-icons-icon-for-dir entry icon-attrs)
                 (apply #'all-the-icons-icon-for-file entry icon-attrs)))
         (icon-w/-offset (concat icon dirvish-icon-delimiter))
         (icon-str (propertize icon-w/-offset 'font-lock-face face))
         (ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'dirvish-icons t)
    (overlay-put ov 'after-string icon-str)))

(defun dirvish-body-render (render-func &optional range)
  "Call RENDER-FUNC on every line in dirvish body with optional RANGE.

Where RENDER-FUNC is a function takes a position (point in
current line) and optional face as args, range"
  (save-excursion
    (let ((beg (or (car range) (- 0 (frame-height))))
          (end (or (cdr range) (+ (line-number-at-pos) (frame-height)))))
      (forward-line beg)
      (while (and (not (eobp)) (< (line-number-at-pos) end))
        (when-let ((pos (dired-move-to-filename nil)))
          (funcall render-func pos))
        (forward-line 1)))))

(provide 'dirvish-parents)

;;; dirvish-parents.el ends here

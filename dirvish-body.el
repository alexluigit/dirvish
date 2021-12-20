;;; dirvish-body.el --- Setup/update body for Dirvish. -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Setup/update body of dirvish buffer.  For icons, the `body' means every
;;; visible line in dirvish buffer rather than the whole buffer, we do this for
;;; performance reason, since we don't want to render icons for every
;;; file/directory in the dirvish buffer, what we really care is the visible
;;; part.

;;; Code:

(declare-function all-the-icons-icon-for-file "all-the-icons")
(declare-function all-the-icons-icon-for-dir "all-the-icons")
(declare-function dired-move-to-filename "dired")
(declare-function dired-get-filename "dired")
(require 'dirvish-vars)

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
      (dirvish--body-render-icon pos 'dirvish-body-face))
    (overlay-put ol 'dirvish-body t)
    (overlay-put ol 'face 'dirvish-body-face)))

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
         (offset `(:v-adjust ,dirvish-icons-v-offset))
         (icon-face (or (when face `(:face ,face))
                        (when dirvish-icons-monochrome `(:face ,(face-at-point)))))
         (icon-attrs (append icon-face offset))
         (icon (if (file-directory-p entry)
                   (apply #'all-the-icons-icon-for-dir entry icon-attrs)
                 (apply #'all-the-icons-icon-for-file entry icon-attrs)))
         (icon-w/-offset (concat icon "\t"))
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

(provide 'dirvish-body)

;;; dirvish-body.el ends here

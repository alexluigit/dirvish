;;; dirvish-updater.el ---  Update a Dirvish instance -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; This library provides functions for updating a Dirvish layout.

;;; Code:

(declare-function all-the-icons-icon-for-file "all-the-icons")
(declare-function all-the-icons-icon-for-dir "all-the-icons")
(require 'dirvish-structs)
(require 'dirvish-options)
(require 'dirvish-helpers)
(require 'dirvish-preview)
(require 'format-spec)
(require 'cl-lib)
(eval-when-compile (require 'subr-x))

(defun dirvish--header-text ()
  "Compose header string."
  (let* ((index (dv-index-path (dirvish-curr)))
         (file-path (file-name-directory index))
         (path-prefix-home (string-prefix-p (getenv "HOME") file-path))
         (path-regex (concat (getenv "HOME") "/\\|\\/$"))
         (path-tail (replace-regexp-in-string path-regex "" file-path))
         (file-name (file-name-nondirectory index)))
    (format "  %s %s %s" (propertize (if path-prefix-home "~" ":"))
            (propertize path-tail 'face 'dired-mark)
            (propertize file-name 'face 'font-lock-constant-face))))

(defun dirvish--render-icon (pos &optional face)
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

(defun dirvish--footer-slot (slot-fn entry)
  "Call SLOT-FN with ENTRY, return the result string.
If SLOT-FN is nil or its return value is nil, return an empty
string."
  (or (and slot-fn (funcall slot-fn entry)) ""))

(defun dirvish--footer-spec ()
  "File specs for current file that will be sent to `format-spec'."
  (let* ((entry (dired-get-filename nil t))
         (fattr (file-attributes entry))
         (file-size (format "%6s" (file-size-human-readable (or (nth 7 fattr) 0))))
         (user (nth 2 fattr))
         (file-date (propertize (format-time-string "%Y-%m-%d %H:%m" (nth 5 fattr))
                                'face 'font-lock-warning-face))
         (file-perm (nth 8 fattr))
         (cur-pos (- (line-number-at-pos (point)) 2))
         (final-pos (- (line-number-at-pos (point-max)) 3))
         (index (format "%3d/%-3d" cur-pos final-pos))
         (sorting (car (dv-sort-criteria (dirvish-curr))))
         (slot-x (dirvish--footer-slot dirvish-footer-slot-x-fn entry))
         (slot-y (dirvish--footer-slot dirvish-footer-slot-y-fn entry))
         (slot-z (dirvish--footer-slot dirvish-footer-slot-z-fn entry))
         (filter (if dired-omit-mode "ON" "OFF"))
         (space "&&&"))
    `((?u . ,user) (?d . ,file-date) (?p . ,file-perm) (?i . ,index) (?f . ,filter)
      (?s . ,file-size) (?S . ,sorting) (?w . ,space) (?x . ,slot-x) (?y . ,slot-y) (?z . ,slot-z))))

(defun dirvish-header-update ()
  "Update header string.

This function trims header string to avoid vertical wobbling."
  (cond
   ((not dirvish-header-style) nil)
   ((dv-one-window-p (dirvish-curr))
    (setq header-line-format (funcall dirvish-header-text-fn)))
   (t
    (with-current-buffer (dv-header-buffer (dirvish-curr))
      (erase-buffer)
      (let* ((str (funcall dirvish-header-text-fn))
             (incr (if (eq dirvish-header-style 'large) 0.25 0.0))
             (max-width (floor (/ (if dirvish-enable-preview
                                      (1- (* (frame-width) (- 1 dirvish-preview-width)))
                                    (- (frame-width) dirvish-header-wobbling-offset))
                                  (1+ incr)))))
        (while (>= (+ (length str) (/ (- (string-bytes str) (length str)) 2)) max-width)
          (setq str (substring str 0 -1)))
        (insert str (if (eq dirvish-header-style 'large) "\n" ""))
      (add-text-properties
       (point-min) (point-max)
       `(display '(height ,(1+ incr)) line-height ,(1+ (* 2 incr)))))))))

(defun dirvish-body-update (&optional skip-icons skip-padding)
  "Update attributes in dirvish body.

By default update icons, padding, and current line.  If
SKIP-ICONS is non-nil, do not update icons.  If SKIP-PADDING is
non-nil, do not update padding."
  (when (and dirvish-show-icons (not skip-icons))
    (remove-overlays (point-min) (point-max) 'dirvish-icons t)
    (dirvish-render 'dirvish--render-icon))
  (when (and (> dirvish-body-fontsize-increment 0) (not skip-padding))
    (save-excursion
      (let ((o (make-overlay (point-min) (point-max))))
        (setq line-spacing dirvish-body-fontsize-increment)
        (overlay-put o 'display `(height ,(1+ dirvish-body-fontsize-increment))))))
  (remove-overlays (point-min) (point-max) 'dirvish-body t)
  (when-let* ((pos (dired-move-to-filename nil))
              (beg (line-beginning-position))
              (end (line-beginning-position 2))
              (ol (make-overlay beg end)))
    (when dirvish-show-icons
      (remove-overlays beg end 'dirvish-icons t)
      (dirvish--render-icon pos 'highlight))
    (overlay-put ol 'dirvish-body t)
    (overlay-put ol 'face 'highlight)))

(cl-defun dirvish-footer-update ()
  "Show file details in echo area."
  (when-let ((one-window-p (and (dirvish-curr)
                                (dv-one-window-p (dirvish-curr)))))
    (cl-return-from dirvish-footer-update))
  (when (and (dired-get-filename nil t)
             (dirvish-live-p))
    (let* ((fwidth (frame-width))
           (footer (format-spec dirvish-footer-format (dirvish--footer-spec)))
           (parts (split-string footer "&&&"))
           (lhs (nth 0 parts))
           (rhs (and (> (length parts) 1) (nth 1 parts)))
           (fringe-gap (if (eq fringe-mode 0) 4 2))
           (space (- fwidth fringe-gap (length lhs)))
           (message-log-max nil)
           (msg (format (format "%%s%%%ds" space) lhs rhs)))
      (message "%s" msg))))

(provide 'dirvish-updater)
;;; dirvish-updater.el ends here

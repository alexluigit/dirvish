;;; dirvish-footer.el --- Footer for Dirvish. -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Setup dirvish footer.

;;; Code:

(require 'format-spec)
(require 'dirvish-vars)
(require 'dirvish-helpers)
(require 'cl-lib)
(eval-when-compile (require 'subr-x))

(cl-defun dirvish-footer-update ()
  "Show file details in echo area."
  (when-let ((one-window-p (dirvish-one-window-p (dirvish-meta))))
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

(defun dirvish--footer-spec ()
  "File specs for current file that will be sent to `format-spec'."
  (let* ((entry (dired-get-filename nil t))
         (fattr (file-attributes entry))
         (file-size (format "%6s" (file-size-human-readable (nth 7 fattr))))
         (user (nth 2 fattr))
         (file-date (propertize (format-time-string "%Y-%m-%d %H:%m" (nth 5 fattr))
                                'face 'font-lock-warning-face))
         (file-perm (nth 8 fattr))
         (cur-pos (- (line-number-at-pos (point)) 2))
         (final-pos (- (line-number-at-pos (point-max)) 3))
         (index (format "%3d/%-3d" cur-pos final-pos))
         (sorting (car (dirvish-sort-criteria (dirvish-meta))))
         (i/o-task (or (dirvish--get-IO-status) ""))
         (filter (if dired-omit-mode "ON" "OFF"))
         (space "&&&"))
    `((?u . ,user) (?d . ,file-date) (?p . ,file-perm) (?i . ,index) (?f . ,filter)
      (?s . ,file-size) (?S . ,sorting) (?w . ,space) (?t . ,i/o-task))))

(provide 'dirvish-footer)

;;; dirvish-footer.el ends here

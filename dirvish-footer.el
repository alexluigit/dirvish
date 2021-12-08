;;; dirvish-footer.el --- footer line for Dirvish. -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Setup dirvish footer.

;;; Code:

(autoload 'format-spec "format-spec")
(require 'dirvish-vars)
(require 'dirvish-helpers)
(require 'cl-lib)
(eval-when-compile (require 'subr-x))

(cl-defun dirvish-footer-update ()
  "Show file details in echo area."
  (when-let ((one-window (frame-parameter nil 'dirvish-one-window)))
    (cl-return-from dirvish-footer-update))
  (when (and (dired-get-filename nil t)
             (dirvish-live-p))
    (let* ((fwidth (frame-width))
           (footer (format-spec dirvish-footer-format (dirvish-footer--spec)))
           (parts (split-string footer "&&&"))
           (lhs (nth 0 parts))
           (rhs (and (> (length parts) 1) (nth 1 parts)))
           (fringe-gap (if (eq fringe-mode 0) 4 2))
           (space (- fwidth fringe-gap (length lhs)))
           (message-log-max nil)
           (msg (format (format "%%s%%%ds" space) lhs rhs)))
      (message "%s" msg))))

(defun dirvish-footer--spec ()
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
         (sorting (if (string= "" dirvish-sort-criteria) "name" dirvish-sort-criteria))
         (i/o-task (or (dirvish-get--i/o-status) ""))
         (filter (format "%s" dirvish-show-hidden))
         (space "&&&"))
    `((?u . ,user) (?d . ,file-date) (?p . ,file-perm) (?i . ,index) (?f . ,filter)
      (?s . ,file-size) (?S . ,sorting) (?w . ,space) (?t . ,i/o-task))))

(provide 'dirvish-footer)

;;; dirvish-footer.el ends here

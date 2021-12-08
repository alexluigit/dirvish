;;; dirvish-parents.el --- Parent windows for Dirvish. -*- lexical-binding: t -*-

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

;;; Creating parent windows for dirvish. A parent window is a window that holds a dirvish buffer,
;;; which exhibit information of parent directory for window on the right side.

;;; Code:

(declare-function dired-hide-details-mode "dired")
(declare-function dired-goto-file "dired")
(require 'dirvish-vars)
(require 'dirvish-header)
(require 'dirvish-body)
(require 'dirvish-helpers)

(defun dirvish-parent-build ()
  "Create all dirvish parent windows."
  (cl-flet ((setup (child win buf one-w)
              (when child (dired-goto-file child))
              (add-to-list 'dirvish-parent-windows win)
              (add-to-list 'dirvish-parent-buffers buf)
              (dirvish-mode)
              (dirvish-parent--default-config)
              (dirvish-header--setup (if one-w 'one-window 'posframe))))
    (let* ((current (expand-file-name default-directory))
           (parent (dirvish-get--parent current))
           (parent-dirs ())
           (one-window (frame-parameter nil 'dirvish-one-window))
           (depth dirvish-depth)
           (i 0))
      (setq dirvish-window (frame-selected-window))
      (if one-window (setq depth 0) (delete-other-windows))
      (setup dirvish-child-entry dirvish-window (current-buffer) one-window)
      (while (and (< i depth) (not (string= current parent)))
        (setq i (+ i 1))
        (push (cons current parent) parent-dirs)
        (setq current (dirvish-get--parent current))
        (setq parent (dirvish-get--parent parent)))
      (when (> depth 0)
        (let ((width (min (/ dirvish-max-parent-width depth) dirvish-width-parents)))
          (cl-dolist (parent-dir parent-dirs)
            (let* ((current (car parent-dir))
                   (parent (cdr parent-dir))
                   (win-alist `((side . left)
                                (inhibit-same-window . t)
                                (window-width . ,width)))
                   (buffer (dired-noselect parent))
                   (window (display-buffer buffer `(dirvish-display--buffer . ,win-alist))))
              (with-selected-window window
                (setup current window buffer one-window)
                (dired-hide-details-mode t)
                (dirvish-body-update))))))
      (when dirvish-enable-preview (dired-hide-details-mode t)))))

(defun dirvish-parent--default-config ()
  (when dirvish-use-default-setup
    (setq cursor-type nil)
    (setq mode-line-format nil)))

(define-derived-mode dirvish-mode dired-mode "Dirvish"
  "Convert `dired' buffer to a `dirvish' buffer."
  :group 'dirvish
  :interactive nil)

(provide 'dirvish-parents)

;;; dirvish-parents.el ends here

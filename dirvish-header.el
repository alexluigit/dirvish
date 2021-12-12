;;; dirvish-header.el --- Header for Dirvish. -*- lexical-binding: t -*-

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

;;; Setup dirvish header.  When posframe is available, use it to create a child frame, otherwise
;;; just change the `header-line-format'.

;;; Code:

(require 'posframe)
(require 'dirvish-vars)
(eval-when-compile (require 'subr-x))

(defvar recentf-list)

(defun dirvish-header-width ()
  "Calculate header frame width.  Default to frame width when disable preview."
  (* (frame-width) (if dirvish-enable-preview (- 1 dirvish-width-preview) 1)))

(cl-defun dirvish-header-build ()
  "Create a posframe showing dirvish header."
  (when-let ((one-window (frame-parameter nil 'dirvish-one-window)))
    (cl-return-from dirvish-header-build))
  (let* ((buf (frame-parameter nil 'dirvish-header-buffer))
         (min-w (1+ (ceiling (dirvish-header-width))))
         (f-props `(:background-color
                    ,(face-attribute 'region :background)
                    :poshandler ,dirvish-header-position
                    :min-width ,min-w
                    :min-height 2))
         (h-frame (frame-parameter nil 'dirvish-header--frame))
         (size `(:posframe ,h-frame :height 2 :max-height 2 :min-height 2
                           :width: ,min-w :min-width ,min-w :max-width ,min-w)))
    (setq dirvish-header-width min-w)
    (if h-frame
        (posframe--set-frame-size size)
      (let ((fr (apply #'posframe-show buf f-props)))
        (set-frame-parameter nil 'dirvish-header--frame fr)))))

(defun dirvish-header-update ()
  "Update header string.
Make sure the length of header string
is less then `(variable) dirvish-header-width'."
  (if-let ((one-window (frame-parameter nil 'dirvish-one-window)))
      (dirvish-header--setup 'one-window)
    (with-current-buffer (frame-parameter nil 'dirvish-header-buffer)
      (erase-buffer)
      (let ((str (funcall dirvish-header-string-fn))
            (max-width (1- (floor (/ dirvish-header-width dirvish-header-scale)))))
        (while (>= (+ (length str) (/ (- (string-bytes str) (length str)) 2)) max-width)
          (setq str (substring str 0 -1)))
        (insert str "\n"))
      (add-text-properties (point-min) (point-max)
                           `(display '(height ,dirvish-header-scale) line-spacing 0.5 line-height 1.5)))))

(defun dirvish-header--setup (type)
  "Apply default setup for dirvish header TYPE.

Where TYPE is either `posframe' or `one-window'."
  ;; FIXME: use face-remapping-alist
  (setq tab-line-format nil)
  (cl-case type
    ('posframe
     (setq header-line-format (propertize " " 'display `(height ,(* 2 (1+ dirvish-body-padding)))))
     (set-face-attribute 'header-line nil :box nil))
    ('one-window
     (setq header-line-format (propertize (dirvish-header--string) 'display `(height ,dirvish-header-scale)))
     (set-face-attribute 'header-line nil :box '(:line-width 4 :color "#353644")))))

(defun dirvish-header--string ()
  "Compose header string."
  (let* ((index (frame-parameter nil 'dirvish-index-path))
         (file-path (file-name-directory index))
         (path-prefix-home (string-prefix-p (getenv "HOME") file-path))
         (path-regex (concat (getenv "HOME") "/\\|\\/$"))
         (path-tail (replace-regexp-in-string path-regex "" file-path))
         (file-name (file-name-nondirectory index)))
    (format "   %s %s %s" (propertize (if path-prefix-home "~" ":"))
            (propertize path-tail 'face 'dired-mark)
            (propertize file-name 'face 'font-lock-constant-face))))

(provide 'dirvish-header)

;;; dirvish-header.el ends here

;;; dirvish-header.el --- Header for Dirvish. -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Setup dirvish header.  When posframe is available, use it to create a child frame, otherwise
;;; just change the `header-line-format'.

;;; Code:

(require 'posframe)
(require 'dirvish-vars)
(require 'dirvish-structs)
(require 'dirvish-helpers)
(eval-when-compile (require 'subr-x))

(defun dirvish--get-header-width ()
  "Calculate header frame width.  Default to frame width when disable preview."
  (* (frame-width) (if dirvish-enable-preview (- 1 dirvish-preview-width) 1)))

(defun dirvish--header-poshandler (_info)
  "Calculate dirvish header coordinate.

Used as `:poshandler' for `posframe-show'."
  (let* ((tab-h (tab-bar-height nil t))
         (frame-border (or (frame-parameter nil 'internal-border-width) 0))
         (x-offset frame-border)
         (y-offset (+ frame-border tab-h)))
    (cons x-offset y-offset)))

(defun dirvish--header-fontsize-increment ()
  "Get dirvish header font size increment in percentage.
If `dirvish-use-large-header' is non-nil, return 0.25, meaning
increase header font size by 25%.  Otherwise return 0."
  (if dirvish-use-large-header 0.25 0.0))

(cl-defun dirvish-header-build ()
  "Create a posframe showing dirvish header."
  (when-let ((one-window-p (dv-one-window-p (dirvish-curr))))
    (cl-return-from dirvish-header-build))
  (let* ((buf (dv-header-buffer (dirvish-curr)))
         (min-w (floor (dirvish--get-header-width)))
         (height (if dirvish-use-large-header 2 1))
         (f-props `(:background-color
                    ,(face-attribute 'region :background)
                    :poshandler dirvish--header-poshandler
                    :min-width ,min-w
                    :min-height ,height)))
    (setf (dv-header-width (dirvish-curr)) min-w)
    (apply #'posframe-show buf f-props)))

(defun dirvish-header-update ()
  "Update header string.

Make header string shorter than variable `dv-header-width'."
  (if-let ((one-window (dv-one-window-p (dirvish-curr))))
      (dirvish--header-setup 'one-window)
    (with-current-buffer (dv-header-buffer (dirvish-curr))
      (erase-buffer)
      (let ((str (funcall dirvish-header-text-fn))
            (max-width (1- (floor (/ (dv-header-width (dirvish-curr))
                                     (1+ (dirvish--header-fontsize-increment)))))))
        (while (>= (+ (length str) (/ (- (string-bytes str) (length str)) 2)) max-width)
          (setq str (substring str 0 -1)))
        (insert str (if dirvish-use-large-header "\n" "")))
      (add-text-properties (point-min) (point-max)
                           `(display '(height ,(1+ (dirvish--header-fontsize-increment)))
                                     line-height ,(1+ (* 2 (dirvish--header-fontsize-increment))))))))

(defun dirvish--header-setup (type)
  "Apply default setup for dirvish header TYPE.

Where TYPE is either `posframe' or `one-window'."
  ;; FIXME: use face-remapping-alist
  (setq tab-line-format nil)
  (cl-case type
    ('posframe
     (setq header-line-format
           (propertize
            " " 'display `(height ,(+ (if dirvish-use-large-header 2 1)
                                      dirvish-header-margin))))
     (set-face-attribute 'header-line nil :box nil))
    ('one-window
     (setq header-line-format
           (propertize
            (funcall dirvish-header-text-fn)
            'display `(height ,(1+ (dirvish--header-fontsize-increment)))))
     (set-face-attribute 'header-line nil :box '(:line-width 4 :color "#353644")))))

(defun dirvish-header-text ()
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

(provide 'dirvish-header)

;;; dirvish-header.el ends here

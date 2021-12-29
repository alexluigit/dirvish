;;; dirvish-header.el --- Header for Dirvish. -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Setup dirvish header.  When using a full-frame dirvish, create a pair of
;;; window and buffer to display the header, otherwise just change the
;;; `header-line-format'.

;;; Code:

(require 'dirvish-vars)
(require 'dirvish-structs)
(require 'dirvish-helpers)
(eval-when-compile (require 'subr-x))

(defun dirvish--header-width ()
  "Calculate dirvish header width for header text display.
Default to frame width when dirvish preview is disabled."
  (floor (/ (if dirvish-enable-preview
                (1- (* (frame-width) (- 1 dirvish-preview-width)))
              (- (frame-width) dirvish-header-wobbling-offset))
            (1+ (dirvish--header-fontsize-increment)))))

(defun dirvish--header-fontsize-increment ()
  "Get dirvish header font size increment in percentage.
If `dirvish-header-style' is `large', return 0.25, meaning
increase header font size by 25%.  Otherwise return 0."
  (if (eq dirvish-header-style 'large) 0.25 0.0))

(defun dirvish-header-build ()
  "Create a window showing dirvish header."
  (when (and dirvish-header-style
             (not (dv-one-window-p (dirvish-curr))))
    (let* ((inhibit-modification-hooks t)
           (buf (dv-header-buffer (dirvish-curr)))
           (win-alist `((side . above)
                        (window-height . ,(if (eq dirvish-header-style 'large) -2 -1))))
           (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
      (with-selected-window new-window
        (set (make-local-variable 'face-remapping-alist)
             dirvish-header-face-remap-alist))
      (setf (dv-header-window (dirvish-curr)) new-window)
      (set-window-buffer new-window buf))))

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
      (let ((str (funcall dirvish-header-text-fn))
            (max-width (dirvish--header-width)))
        (while (>= (+ (length str) (/ (- (string-bytes str) (length str)) 2)) max-width)
          (setq str (substring str 0 -1)))
        (insert str (if (eq dirvish-header-style 'large) "\n" "")))
      (add-text-properties
       (point-min) (point-max)
       `(display '(height ,(1+ (dirvish--header-fontsize-increment)))
                 line-height ,(1+ (* 2 (dirvish--header-fontsize-increment)))))))))

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

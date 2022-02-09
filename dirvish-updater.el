;;; dirvish-updater.el ---  Update a Dirvish instance -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides functions for updating a Dirvish layout.

;;; Code:

(declare-function dired-filter--describe-filters "dired-filter")
(defvar dired-filter-mode)
(defvar dired-filter-show-filters)
(defvar dired-filter-revert)
(defvar fd-dired-input-fd-args)
(require 'dirvish-structs)
(require 'dirvish-helpers)
(require 'dirvish-options)
(eval-when-compile
  (require 'subr-x)
  (require 'find-dired))

(when (require 'dired-filter nil t)
  (setq dired-filter-show-filters nil)
  (setq dired-filter-revert 'always))

(defun dirvish--header-line-path ()
  "Compose header string."
  (when-let ((dv (dirvish-curr)))
    (let* ((index (dv-index-path dv))
           (file-path (or (file-name-directory index) ""))
           (path-prefix-home (string-prefix-p (getenv "HOME") file-path))
           (path-regex (concat (getenv "HOME") "/\\|\\/$"))
           (path-tail (replace-regexp-in-string path-regex "" file-path))
           (file-name (file-name-nondirectory index)))
      (format "  %s %s %s"
              (propertize (if path-prefix-home "~" ":"))
              (propertize path-tail 'face 'dired-mark)
              (propertize file-name 'face 'font-lock-constant-face)))))

(defun dirvish--render-hl-line (_pos &optional hl-face)
  "Highlight current line with HL-FACE."
  (when hl-face
    (let* ((beg (line-beginning-position))
           (end (line-beginning-position 2))
           (ol (make-overlay beg end)))
      (overlay-put ol 'dirvish-hl-line t)
      (overlay-put ol 'face 'highlight))))

(defun dirvish--render-zoom (_pos render)
  "Zoom in current Dirvish buffer when RENDER is non-nil."
  (when render
    (let ((o (make-overlay (point-min) (point-max))))
      (setq line-spacing dirvish-body-zoom)
      (overlay-put o 'dirvish-zoom t)
      (overlay-put o 'display `(height ,(1+ dirvish-body-zoom)))
      (overlay-put o 'priority -999))))

(defun dirvish--render-symlink-target (_pos _face)
  "Hide symlink target using overlay."
  ;; This hack solves 2 issues:
  ;; 1. Hide " -> " arrow of symlink files as well.
  ;; 2. A `dired-subtree' bug (https://github.com/Fuco1/dired-hacks/issues/125).
	(dired-move-to-end-of-filename)
	(when (and dired-hide-details-mode
             (default-value 'dired-hide-details-hide-symlink-targets)
             (< (+ (point) 4) (line-end-position)))
    (let ((o (make-overlay (point) (line-end-position))))
      (overlay-put o 'dirvish-symlink-target t)
      (overlay-put o 'invisible t))))

(defun dirvish--mode-line-sorter ()
  "Return a string showing current Dired file sort criteria."
  (with-current-buffer (window-buffer (dv-root-window (dirvish-curr)))
    (format " %s %s "
            (propertize "Sort:" 'face 'bold)
            (propertize (car (dv-sort-criteria (dirvish-curr))) 'face 'font-lock-type-face))))

(defun dirvish--mode-line-filter ()
  "Return a string showing active Dired file filter."
  (with-current-buffer (window-buffer (dv-root-window (dirvish-curr)))
    (cond ((bound-and-true-p dired-filter-mode)
           (format " %s %s " (propertize "Filters:" 'face 'bold)
                   (dired-filter--describe-filters)))
          (dired-omit-mode (propertize "[Omit]" 'face 'bold)))))

(defun dirvish--mode-line-fd-args ()
  "Return a string showing current `find/fd' command args."
  (with-current-buffer (window-buffer (dv-root-window (dirvish-curr)))
    (when (string-match "^\\*F\\(?:d\\|ind\\)\\*$" (buffer-name))
      (format " %s [%s] "
              (propertize "FD:" 'face 'bold)
              (propertize (or (bound-and-true-p fd-dired-input-fd-args) find-args)
                          'face 'font-lock-string-face)))))

(defun dirvish--mode-line-index ()
  "Return a string showing index in a Dirvish buffer."
  (with-current-buffer (window-buffer (dv-root-window (dirvish-curr)))
    (let ((cur-pos (- (line-number-at-pos (point)) 1))
          (fin-pos (number-to-string (- (line-number-at-pos (point-max)) 2))))
      (format " %d / %s " cur-pos (propertize fin-pos 'face 'bold)))))

(defun dirvish-body-update ()
  "Update attributes in dirvish body."
  (let* ((curr-pos (point))
         (p-min (point-min))
         (p-max (point-max))
         (fr-h (frame-height))
         (beg (- 0 fr-h))
         (end (+ (line-number-at-pos) fr-h))
         (dv (dirvish-curr))
         (attrs (dv-attributes-alist dv))
         (ov-names (mapcar #'car attrs))
         (renderers (mapcar #'cdr attrs)))
    (mapc (lambda (ov) (remove-overlays p-min p-max ov t)) ov-names)
    (save-excursion
      (forward-line beg)
      (while (and (not (eobp)) (< (line-number-at-pos) end))
        (when-let ((pos (and (not (invisible-p (point)))
                             (dired-move-to-filename nil))))
          (mapc (lambda (rd) (funcall rd pos (and (eq pos curr-pos) 'highlight))) renderers))
        (forward-line 1)))))

(defun dirvish-mode-line-update ()
  "Show file details in mode line."
  (when-let ((dv (dirvish-curr)))
    (with-current-buffer (dirvish--get-buffer 'footer) (force-mode-line-update))
    (with-current-buffer (dirvish--get-buffer 'header) (force-mode-line-update))))

(provide 'dirvish-updater)
;;; dirvish-updater.el ends here

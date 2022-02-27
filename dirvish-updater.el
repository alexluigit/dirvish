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
(require 'dirvish-core)
(require 'dirvish-helpers)
(require 'dirvish-options)
(eval-when-compile
  (require 'subr-x)
  (require 'find-dired))

(when (require 'dired-filter nil t)
  (setq dired-filter-show-filters nil)
  (setq dired-filter-revert 'always))

(dirvish-define-attribute hl-line (line-beg line-end hl-face) :lineform
  (when hl-face
    (let ((ol (make-overlay line-beg (1+ line-end))))
      (overlay-put ol 'dirvish-hl-line t)
      (overlay-put ol 'face 'highlight))))

;; This hack solves 2 issues:
;; 1. Hide " -> " arrow of symlink files as well.
;; 2. A `dired-subtree' bug (https://github.com/Fuco1/dired-hacks/issues/125).
(dirvish-define-attribute symlink-target (file-end line-end) :lineform
	(when (and dired-hide-details-mode
             (default-value 'dired-hide-details-hide-symlink-targets)
             (< (+ file-end 4) line-end))
    (let ((o (make-overlay file-end line-end)))
      (overlay-put o 'dirvish-symlink-target t)
      (overlay-put o 'invisible t))))

(defun dirvish-default-header-string-fn ()
  "Compose header string."
  (when-let ((dv (dirvish-curr)))
    (let* ((index (dv-index-path dv))
           (file-path (or (file-name-directory index) ""))
           (path-prefix-home (string-prefix-p (getenv "HOME") file-path))
           (path-regex (concat (getenv "HOME") "/\\|\\/$"))
           (path-tail (replace-regexp-in-string path-regex "" file-path))
           (file-name (file-name-nondirectory index)))
      (format " %s %s %s"
              (propertize (if path-prefix-home "~" ":"))
              (propertize path-tail 'face 'dired-mark)
              (propertize file-name 'face 'font-lock-constant-face)))))

(defun dirvish-find-dired-header-string-fn ()
  "Return a string showing current `find/fd' command args."
  (with-current-buffer (window-buffer (dv-root-window (dirvish-curr)))
    (when-let ((args (or (bound-and-true-p fd-dired-input-fd-args) find-args)))
      (format " %s [%s] at %s"
              (propertize "FD:" 'face 'bold)
              (propertize args 'face 'font-lock-string-face)
              (propertize default-directory 'face 'dired-header)))))

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

(defun dirvish--mode-line-index ()
  "Return a string showing index in a Dirvish buffer."
  (with-current-buffer (window-buffer (dv-root-window (dirvish-curr)))
    (let ((cur-pos (- (line-number-at-pos (point)) 1))
          (fin-pos (number-to-string (- (line-number-at-pos (point-max)) 2))))
      (format " %d / %s " cur-pos (propertize fin-pos 'face 'bold)))))

(defun dirvish-body-update ()
  "Update attributes in dirvish body."
  (let* ((curr-pos (point))
         (fr-h (frame-height))
         (beg (- 0 fr-h))
         (end (+ (line-number-at-pos) fr-h))
         (dv (dirvish-curr))
         (attrs (dv-attributes-alist dv))
         (body-renderers (mapcar #'car attrs))
         (line-renderers (mapcar #'cdr attrs)))
    (mapc #'funcall body-renderers)
    (save-excursion
      (forward-line beg)
      (while (and (not (eobp)) (< (line-number-at-pos) end))
        (when-let ((line-beg (line-beginning-position))
                   (line-end (line-end-position))
                   (file-beg (and (not (invisible-p (point)))
                             (dired-move-to-filename nil)))
                   (file-end (dired-move-to-end-of-filename t)))
          (mapc (lambda (rd) (funcall rd file-beg file-end line-beg line-end
                                 (and (eq file-beg curr-pos) 'highlight)))
                line-renderers))
        (forward-line 1)))))

(defun dirvish-mode-line-update ()
  "Show file details in mode line."
  (when-let ((dv (dirvish-curr)))
    (with-current-buffer (dirvish--get-util-buffer dv 'footer) (force-mode-line-update))
    (with-current-buffer (dirvish--get-util-buffer dv 'header) (force-mode-line-update))))

(provide 'dirvish-updater)
;;; dirvish-updater.el ends here

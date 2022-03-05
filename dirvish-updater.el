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

(dirvish-define-attribute hl-line
  :form
  (when hl-face
    (let ((ov (make-overlay l-beg (1+ l-end)))) (overlay-put ov 'face 'highlight) ov)))

;; This hack solves 2 issues:
;; 1. Hide " -> " arrow of symlink files as well.
;; 2. A `dired-subtree' bug (https://github.com/Fuco1/dired-hacks/issues/125).
(dirvish-define-attribute symlink-target
  :if (and dired-hide-details-mode (default-value 'dired-hide-details-hide-symlink-targets))
  :form
  (when (< (+ f-end 4) l-end)
    (let ((ov (make-overlay f-end l-end))) (overlay-put ov 'invisible t) ov)))

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
            (or (and dired-sort-inhibit (propertize "inhibited" 'face 'font-lock-warning-face))
                (propertize (car (dv-sort-criteria (dirvish-curr))) 'face 'font-lock-type-face)))))

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

(defun dirvish-body-update (dv)
  "Update attributes in Dirvish session DV's body."
  (when (> (length dirvish--attrs-alist) 500)
    (setq-local dirvish--attrs-alist nil))
  (let* ((attrs (dv-attributes-alist dv))
         (curr-pos (point))
         (fr-h (frame-height))
         (beg (- 0 fr-h))
         (end (+ (line-number-at-pos) fr-h))
         (fns (cl-loop with (left-w . right-w) = (cons dirvish-prefix-spaces 0)
                       for (ov pred fn left right) in attrs
                       do (remove-overlays (point-min) (point-max) ov t)
                       for valid = (funcall pred dv)
                       when valid do (progn (setq left-w (+ left-w (or (eval left) 0)))
                                            (setq right-w (+ right-w (or (eval right) 0))))
                       when valid collect (prog1 fn (setq-local dirvish--attrs-width (cons left-w right-w))))))
    (save-excursion
      (forward-line beg)
      (while (and (not (eobp)) (< (line-number-at-pos) end))
        (when-let ((f-name (dired-get-filename nil t))
                   (f-beg (and (not (invisible-p (point)))
                               (dired-move-to-filename nil)))
                   (f-end (dired-move-to-end-of-filename t)))
          (let ((f-attrs (file-attributes f-name))
                (l-beg (line-beginning-position))
                (l-end (line-end-position))
                (hl-face (and (eq f-beg curr-pos) 'highlight)))
            (dolist (fn fns) (funcall fn f-name f-attrs f-beg f-end l-beg l-end hl-face))))
        (forward-line 1)))))

(defun dirvish-mode-line-update ()
  "Show file details in mode line."
  (when-let ((dv (dirvish-curr)))
    (with-current-buffer (dirvish--get-util-buffer dv 'footer) (force-mode-line-update))
    (with-current-buffer (dirvish--get-util-buffer dv 'header) (force-mode-line-update))))

(provide 'dirvish-updater)
;;; dirvish-updater.el ends here

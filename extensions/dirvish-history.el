;;; dirvish-history.el --- History navigation commands in Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.9.23
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; History navigation commands in Dirvish.

;;; Code:

(require 'ring)
(require 'dirvish)

(define-obsolete-function-alias 'dirvish-show-history #'dirvish-history-jump "Jun 08, 2022")
(define-obsolete-function-alias 'dirvish-other-buffer #'dirvish-history-last "Jun 08, 2022")
(define-obsolete-function-alias 'dirvish-go-forward-history #'dirvish-history-go-forward "Jun 08, 2022")
(define-obsolete-function-alias 'dirvish-go-backward-history #'dirvish-history-go-backward "Jun 08, 2022")

(defvar dirvish-history--ring nil)
(defun dirvish-history-insert-entry-h (_dv entry buffer)
  "Add ENTRY name for BUFFER to `dirvish-history--ring'."
  (let ((entry (if (string-prefix-p "FD####" entry)
                   (buffer-name buffer) entry)))
    (ring-insert dirvish-history--ring entry)))
(defcustom dirvish-history-length 50
  "Length of directory visiting history Dirvish will track."
  :group 'dirvish :type 'integer
  :set (lambda (k v)
         (set k v)
         (if (> dirvish-history-length 0)
             (progn
               (setq dirvish-history--ring (make-ring v))
               (add-hook 'dirvish-find-entry-hook #'dirvish-history-insert-entry-h))
           (setq dirvish-history--ring nil)
           (remove-hook 'dirvish-find-entry-hook #'dirvish-history-insert-entry-h))))

;;;###autoload
(defun dirvish-history-jump ()
  "Open a target directory from `dirvish-history--ring'."
  (interactive)
  (unless (ring-p dirvish-history--ring)
    (user-error "Dirvish[error]: global history tracking has been disabled"))
  (let* ((history-w/metadata
          (dirvish--append-metadata
           'file (ring-elements dirvish-history--ring)))
         (result (completing-read "Recently visited: " history-w/metadata)))
      (when result (dirvish-find-entry-ad result))))

;;;###autoload
(defun dirvish-history-last ()
  "Switch to the most recently visited dirvish buffer."
  (interactive)
  (unless (ring-p dirvish-history--ring)
    (user-error "Dirvish[error]: global history tracking has been disabled"))
  (let ((match
         (cl-loop
          with local-entries = (mapcar #'car (dv-roots (dirvish-curr)))
          with entries = (ring-elements dirvish-history--ring)
          for entry in entries
          thereis (and (member entry local-entries)
                       (not (equal entry (dired-current-directory))) entry))))
    (and match (dirvish-find-entry-ad match))))

;;;###autoload
(defun dirvish-history-go-forward (arg)
  "Navigate to next ARG directory in history.
ARG defaults to 1."
  (interactive "^p")
  (let* ((dirs (reverse
                (mapcar #'car (dv-roots (dirvish-curr)))))
         (len (length dirs))
         (idx (cl-position
               (car (dv-index-dir (dirvish-curr))) dirs :test #'equal))
         (new-idx (+ idx arg)))
    (cond ((>= new-idx len)
           (dirvish-find-entry-ad (nth (- len 1) dirs))
           (message "Dirvish: reached the end of history"))
          ((< new-idx 0)
           (dirvish-find-entry-ad (nth 0 dirs))
           (message "Dirvish: reached the beginning of history"))
          (t (dirvish-find-entry-ad (nth new-idx dirs))))))

;;;###autoload
(defun dirvish-history-go-backward (arg)
  "Navigate to previous ARG directory in history.
ARG defaults to 1."
  (interactive "^p")
  (dirvish-history-go-forward (- 0 arg)))

(provide 'dirvish-history)
;;; dirvish-history.el ends here

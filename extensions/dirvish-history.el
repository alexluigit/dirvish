;;; dirvish-history.el --- History navigation commands in Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; History navigation commands in Dirvish.

;;; Code:

(require 'dirvish)

(define-obsolete-function-alias 'dirvish-show-history #'dirvish-history-jump "Jun 08, 2022")
(define-obsolete-function-alias 'dirvish-other-buffer #'dirvish-history-last "Jun 08, 2022")
(define-obsolete-function-alias 'dirvish-go-forward-history #'dirvish-history-go-forward "Jun 08, 2022")
(define-obsolete-function-alias 'dirvish-go-backward-history #'dirvish-history-go-backward "Jun 08, 2022")

;;;###autoload
(defun dirvish-history-jump ()
  "Open a target directory from `dirvish--history'."
  (interactive)
  (unless dirvish--history (user-error "Dirvish[error]: no history entries"))
  (let* ((entries (dirvish--append-metadata 'file dirvish--history))
         (result (completing-read "Recently visited: " entries)))
      (when result (dirvish-find-entry-a result))))

;;;###autoload
(defun dirvish-history-last ()
  "Switch to the most recently visited dirvish buffer."
  (interactive)
  (unless dirvish--history (user-error "Dirvish[error]: no history entries"))
  (let ((match
         (cl-loop
          with local-entries = (mapcar #'car (dv-roots (dirvish-curr)))
          for entry in dirvish--history
          thereis (and (member entry local-entries)
                       (not (equal entry (dired-current-directory))) entry))))
    (and match (dirvish-find-entry-a match))))

;;;###autoload
(defun dirvish-history-go-forward (arg)
  "Navigate to next ARG directory in history.
ARG defaults to 1."
  (interactive "^p")
  (let* ((dv (or (dirvish-curr) (user-error "Not in a dirvish session")))
         (dirs (reverse (mapcar #'car (dv-roots dv))))
         (len (length dirs))
         (idx (cl-position (car (dv-index dv)) dirs :test #'equal))
         (new-idx (+ idx arg)))
    (cond ((>= new-idx len)
           (dirvish-find-entry-a (nth (- len 1) dirs))
           (message "Dirvish: reached the end of history"))
          ((< new-idx 0)
           (dirvish-find-entry-a (nth 0 dirs))
           (message "Dirvish: reached the beginning of history"))
          (t (dirvish-find-entry-a (nth new-idx dirs))))))

;;;###autoload
(defun dirvish-history-go-backward (arg)
  "Navigate to previous ARG directory in history.
ARG defaults to 1."
  (interactive "^p")
  (dirvish-history-go-forward (- 0 arg)))

;;;###autoload (autoload 'dirvish-history-menu "dirvish-history" nil t)
(transient-define-prefix dirvish-history-menu ()
  "Help menu for `dirvish-history-*' commands."
  [:description
   (lambda () (dirvish--format-menu-heading "Go to history entries"))
   ("f" "Forward history"        dirvish-history-go-forward :transient t)
   ("b" "Backward history"       dirvish-history-go-backward :transient t)
   ("l" "Go to most recent used" dirvish-history-last)
   ("a" "Access history entries" dirvish-history-jump)])

(provide 'dirvish-history)
;;; dirvish-history.el ends here

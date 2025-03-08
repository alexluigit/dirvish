;;; dirvish-history.el --- History navigation commands in Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.2.3
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; History navigation commands in Dirvish.

;;; Code:

(require 'dirvish)
(require 'transient)

(defcustom dirvish-history-sort-function #'dirvish-history--sort-by-atime
  "Function used to sort history entries for `dirvish-history-jump'."
  :group 'dirvish :type 'function)

(defun dirvish-history--sort-by-atime (file-list)
  "Sort the FILE-LIST by access time, from most recent to least recent."
  (thread-last
    file-list
    ;; Use modification time, since getting file access time seems to count as
    ;; accessing the file, ruining future uses.
    (mapcar (lambda (f) (cons f (file-attribute-access-time (file-attributes f)))))
    (seq-sort (pcase-lambda (`(,f1 . ,t1) `(,f2 . ,t2))
                ;; Want existing, most recent, local files first.
                (cond ((or (not (file-exists-p f1)) (file-remote-p f1)) nil)
                      ((or (not (file-exists-p f2)) (file-remote-p f2)) t)
                      (t (time-less-p t2 t1)))))
    (mapcar #'car)))

;;;###autoload
(defun dirvish-history-jump ()
  "Open a target directory from `dirvish--history'."
  (interactive)
  (unless dirvish--history (user-error "Dirvish[error]: no history entries"))
  (when-let* ((result
               (completing-read
                "Recently visited: "
                (dirvish--completion-table-with-metadata
                 dirvish--history
                 `((category . file)
                   (display-sort-function . ,dirvish-history-sort-function))))))
    (dirvish--find-entry 'find-file result)))

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
    (and match (dirvish--find-entry 'find-file match))))

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
           (dirvish--find-entry 'find-file (nth (- len 1) dirs))
           (message "Dirvish: reached the end of history"))
          ((< new-idx 0)
           (dirvish--find-entry 'find-file (nth 0 dirs))
           (message "Dirvish: reached the beginning of history"))
          (t (dirvish--find-entry 'find-file (nth new-idx dirs))))))

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

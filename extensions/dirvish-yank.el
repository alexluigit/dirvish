;;; dirvish-yank.el --- Async file copy/paste in Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.0.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (dirvish "1.0.0"))

;;; Commentary:

;;; This package is a Dirvish extension which provides command `dirvish-yank'
;;; that enables async file copy/paste in dirvish.

;;; Code:

(require 'dirvish)

(defvar dirvish-yank--queue '())

(defun dirvish--yank (&optional mode)
  "Paste marked files/directory to current directory according to MODE.

MODE can be `'copy', `'move', `symlink', or `relalink'."
  (interactive)
  (let* ((regexp (dired-marker-regexp))
         (yanked-files ())
         (mode (or mode 'copy))
         case-fold-search)
    (cl-dolist (buf (seq-filter #'buffer-live-p (dirvish-get-all 'dired-buffers t t)))
      (with-current-buffer buf
        (when (save-excursion (goto-char (point-min))
                              (re-search-forward regexp nil t))
          (setq yanked-files
                (append yanked-files (dired-map-over-marks (dired-get-filename) nil))))))
    (unless yanked-files (user-error "No files marked for pasting"))
    (dirvish--do-yank yanked-files mode)))

(defun dirvish--do-yank (fileset mode)
  "Run paste-mode MODE on FILESET.
This function is a helper for `dirvish--yank'."
  (let* ((target (dired-current-directory))
         (process-connection-type nil)
         (io-buffer (generate-new-buffer " *Dirvish I/O*"))
         (paste-func
          (cl-case mode
            ('copy (lambda (fr to) (start-process "" io-buffer "cp" "-f" "-r" "-v" fr to)))
            ('move (lambda (fr to) (start-process "" io-buffer "mv" "-f" "-v" fr to)))
            ('symlink (lambda (fr to) (make-symbolic-link fr to)))
            ('relalink (lambda (fr to) (dired-make-relative-symlink fr to)))))
         (new-fileset ())
         overwrite abort)
    (cl-dolist (file fileset)
      (when (and (not abort) (file-exists-p file))
        (let* ((base-name (file-name-nondirectory file))
               (paste-name (concat target base-name))
               (prompt (concat base-name " exists, overwrite?: (y)es (n)o (a)ll (q)uit"))
               choice)
          (if overwrite
              (setq new-fileset (dirvish--yank-push-task file target paste-name new-fileset))
            (if (file-exists-p paste-name)
                (let ((name~ paste-name)
                      (idx 1))
                  (setq choice (read-char-choice prompt '(?y ?n ?a ?q)))
                  (when (eq choice ?n)
                    (while (file-exists-p name~)
                      (setq name~ (concat paste-name (number-to-string idx) "~"))
                      (setq idx (1+ idx))))
                  (cl-case choice
                    ((?y ?n) (setq new-fileset (dirvish--yank-push-task file target name~ new-fileset)))
                    (?a (setq overwrite t)
                        (setq new-fileset (dirvish--yank-push-task file target name~ new-fileset)))
                    (?q (setq abort t) (setq new-fileset ()))))
              (setq new-fileset (dirvish--yank-push-task file target paste-name new-fileset)))))))
    (and abort (user-error "Dirvish: yank aborted"))
    (let ((size (dirvish--get-filesize (mapcar #'car new-fileset)))
          (leng (length new-fileset)))
      (add-to-list 'dirvish-yank--queue `(nil ,io-buffer ,size ,(cons 0 leng) ,mode)))
    (dirvish-repeat dirvish--yank-status-update 0 dirvish--repeat-interval)
    (cl-dolist (file new-fileset)
      (funcall paste-func (car file) (cdr file)))
    (cl-dolist (buf (dirvish-get-all 'dired-buffers t t))
      (with-current-buffer buf (dired-unmark-all-marks)))
    (message "Dirvish: task started.")))

(defun dirvish--yank-push-task (file dir name place)
  "Push (FILE . DIR or NAME) cons to PLACE.
If FILE is a directory, push (FILE . DIR), otherwise push (FILE
. NAME).  In either case, return PLACE."
  (if (file-directory-p file)
      (push (cons file dir) place)
    (push (cons file name) place))
  place)

(defun dirvish--yank-status-update ()
  "Update current yank task progress."
  (when-let* ((task (car-safe dirvish-yank--queue)))
    (let ((io-buf (nth 1 task))
          (progress (car (nth 3 task)))
          (length (cdr (nth 3 task)))
          (mode (nth 4 task))
          (proc-exit "Process \\(<[0-9]+>\\)? \\(exited\\|finished\\).*"))
      (if (or (eq mode 'copy) (eq mode 'move))
          (setq progress (with-current-buffer io-buf
                           (how-many proc-exit (point-min) (point-max))))
        (setq progress length))
      (when (eq progress length)
        (when (dirvish-curr) (revert-buffer))
        (setf (nth 0 (car-safe dirvish-yank--queue)) t)
        (when (eq (length dirvish-yank--queue) 1)
          (cancel-timer (symbol-value 'dirvish--yank-status-update-timer))))
      (setcar (nth 3 (car-safe dirvish-yank--queue)) progress))))

;;;###autoload (autoload 'dirvish-yank-ml "dirvish-yank" nil t)
(dirvish-define-mode-line yank "Current move/paste task progress."
  (when-let* ((task (car-safe dirvish-yank--queue)))
    (let ((finished (car task))
          (size (nth 2 task))
          (index (car (nth 3 task)))
          (length (cdr (nth 3 task))))
      (when finished (setq dirvish-yank--queue (cdr dirvish-yank--queue)))
      (format " %s: %s total size: %s "
              (if finished "Success" "Progress")
              (propertize (format "%s / %s" index length) 'face 'font-lock-keyword-face)
              (propertize size 'face 'font-lock-builtin-face)))))

;;;###autoload
(defun dirvish-yank-retrive-progress-h ()
  "Retrieve progress of current paste/move task."
  (when (dirvish-yank-ml) (dirvish-repeat dirvish--yank-status-update 0 0.5)))

;;;###autoload
(defun dirvish-yank (&optional arg)
  "Paste marked files/directory to current directory.
With optional prefix ARG, delete source files/directories."
  (interactive "P")
  (if arg (dirvish--yank 'move) (dirvish--yank)))

(provide 'dirvish-yank)
;;; dirvish-yank.el ends here

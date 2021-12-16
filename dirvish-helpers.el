;;; dirvish-helpers.el --- Helper functions for Dirvish. -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Helper functions for dirvish.

;;; Code:

(declare-function dirvish "dirvish")
(declare-function dirvish-refresh "dirvish")
(require 'dirvish-vars)
(require 'dired-x)

(defmacro dirvish-repeat (func delay interval &rest args)
  "Execute FUNC with ARGS in every INTERVAL after DELAY."
  (let ((timer (intern (format "%s-timer" func))))
    `(progn
       (defvar ,timer nil)
       (add-to-list 'dirvish-repeat-timers ',timer)
       (setq ,timer (run-with-timer ,delay ,interval ',func ,@args)))))

(defmacro dirvish-debounce (func delay &rest args)
  "Execute a delayed version of FUNC with delay time DELAY.

When called, the FUNC only runs after the idle time
specified by DELAY.  Multiple calls to the same function before
the idle timer fires are ignored.  ARGS is arguments for FUNC."
  (let* ((timer (intern (format "%s-timer" func)))
         (do-once `(lambda (&rest args)
                     (unwind-protect (apply #',func args) (setq ,timer nil)))))
    `(progn
       (unless (boundp ',timer) (defvar ,timer nil))
       (unless (timerp ,timer)
         (setq ,timer (run-with-idle-timer ,delay nil ,do-once ,@args))))))

(defun dirvish-meta (&optional frame)
  "Get dirvish metadata of FRAME.

FRAME defaults to current frame."
  (frame-parameter frame 'dirvish-meta))

(defun dirvish--header-buffer-default ()
  "Return a buffer for dirvish header with sensible settings."
  (with-current-buffer
      (get-buffer-create
       (format " *Dirvish Header-%s*"
               (number-to-string (length dirvish-frame-list))))
    (setq-local face-font-rescale-alist nil)
    (current-buffer)))

(defun dirvish--preview-buffer-default ()
  "Return a buffer for dirvish preview with sensible settings."
  (with-current-buffer
      (get-buffer-create
       (format " *Dirvish Preview-%s*"
               (number-to-string (length dirvish-frame-list))))
    (setq-local mode-line-format nil)
    (current-buffer)))

(defun dirvish--clean-buffers ()
  "Cleanup all dirvish buffers."
  (cl-dolist (buf (buffer-list))
    (let ((name (buffer-name buf))
          (mode (buffer-local-value 'major-mode buf)))
      (when (or (eq 'dired-mode mode) (eq 'dirvish-mode mode)
                (and (not (string-equal name ""))
                     (string-match " \\*Dirvish .*" name)
                     (not (get-buffer-process buf))))
        (kill-buffer buf)))))

;;;###autoload
(defun dirvish-live-p (&optional win)
  "Helper function for detecting if WIN is in dirvish mode.

If WIN is nil, defaults to `\\(selected-window\\)'."
  (memq (or win (selected-window)) dirvish-parent-windows))

(defun dirvish--update-filter ()
  "Update file list in dirvish buffer.

According to `dirvish-hidden-regexp'."
  (save-excursion
    (let* ((all-re '("^\\.?#\\|^\\.$\\|^\\.\\.$"))
           (dot-re '("^\\."))
           (method (cl-case dirvish-show-hidden ('dirvish dirvish-hidden-regexp)
                            ('dot dot-re)))
           (omit-re (mapconcat #'concat (append all-re method) "\\|"))
           buffer-read-only)
      (dired-mark-unmarked-files omit-re nil nil 'no-dir)
      (goto-char (point-min))
      (let ((regexp (dired-marker-regexp)))
        (while (and (not (eobp)) (re-search-forward regexp nil t))
          (delete-region (line-beginning-position) (progn (forward-line 1) (point))))))))

(defun dirvish--display-buffer (buffer alist)
  "Try displaying BUFFER at one side of the selected frame.

 This splits the window at the designated side of the
 frame.  ALIST is window arguments for the new-window, it has the
 same format with `display-buffer-alist'."
  (let* ((side (cdr (assq 'side alist)))
         (window-configuration-change-hook nil)
         (window-width (or (cdr (assq 'window-width alist)) 0.5))
         (size (ceiling (* (frame-width) window-width)))
         (split-width-threshold 0)
         (new-window (split-window-no-error dirvish-window size side)))
    (window--display-buffer buffer new-window 'window alist)))

(defun dirvish--paste (fileset mode)
  "Run paste-mode MODE on FILESET.
This function is a helper for `dirvish-paste'."
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
              (push (cons file paste-name) new-fileset)
            (if (file-exists-p paste-name)
                (let ((name~ paste-name)
                      (idx 1))
                  (setq choice (read-char-choice prompt '(?y ?n ?a ?q)))
                  (when (eq choice ?n)
                    (while (file-exists-p name~)
                      (setq name~ (concat paste-name (number-to-string idx) "~"))
                      (setq idx (1+ idx))))
                  (cl-case choice
                    ((?y ?n) (push (cons file name~) new-fileset))
                    (?a (setq overwrite t) (push (cons file name~) new-fileset))
                    (?q (setq abort t) (setq new-fileset ()))))
              (push (cons file paste-name) new-fileset))))))
    (let ((size (dirvish--get-filesize (mapcar #'car new-fileset)))
          (leng (length new-fileset)))
      (add-to-list 'dirvish-IO-queue `(nil ,io-buffer ,size ,(cons 0 leng) ,mode)))
    (dirvish-repeat dirvish-footer-update 0 0.1)
    (dirvish-repeat dirvish--set-IO-status 0 0.1)
    (cl-dolist (file new-fileset)
      (funcall paste-func (car file) (cdr file)))
    (cl-dolist (buf dirvish-parent-buffers)
      (with-current-buffer buf (dired-unmark-all-marks)))))

(defun dirvish--get-parent (path)
  "Get parent directory of PATH."
  (file-name-directory (directory-file-name (expand-file-name path))))

(defun dirvish--get-filesize (fileset)
  "Determine file size of provided list of files in FILESET."
  (unless (executable-find "du") (user-error "`du' executable not found"))
  (with-temp-buffer
    (apply #'call-process "du" nil t nil "-sch" fileset)
    (format "%s" (progn (re-search-backward "\\(^[0-9.,]+[a-zA-Z]*\\).*total$")
                        (match-string 1)))))

(defun dirvish--get-trash-dir ()
  "Get trash directory for current disk."
  (cl-dolist (dir dirvish-trash-dir-alist)
    (when (string-prefix-p (car dir) (dired-current-directory))
      (cl-return (concat (car dir) (cdr dir))))))

(defun dirvish--get-IO-status ()
  "Get current disk I/O task."
  (when-let* ((task (car-safe dirvish-IO-queue)))
    (let ((finished (car task))
          (size (nth 2 task))
          (index (car (nth 3 task)))
          (length (cdr (nth 3 task))))
      (when finished
        (setq dirvish-IO-queue (cdr dirvish-IO-queue))
        (unless dirvish-IO-queue
          (cancel-timer (symbol-value 'dirvish-footer-update-timer))))
      (format "%s: %s total size: %s"
              (if finished "Success" "Progress")
              (propertize (format "%s / %s" index length) 'face 'font-lock-keyword-face)
              (propertize size 'face 'font-lock-builtin-face)))))

(defun dirvish--set-IO-status ()
  "Set current disk I/O task."
  (when-let* ((task (car-safe dirvish-IO-queue)))
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
        (when (dirvish-live-p) (dirvish-refresh))
        (setf (nth 0 (car-safe dirvish-IO-queue)) t)
        (when (eq (length dirvish-IO-queue) 1)
          (cancel-timer (symbol-value 'dirvish--set-IO-status-timer))))
      (setcar (nth 3 (car-safe dirvish-IO-queue)) progress))))

(defun dirvish-override-dired (&rest _)
  "Helper func for `dirvish-override-dired-mode'."
  (dirvish nil (or (not window-system)
                   (not (= (length (window-list)) 1)))))

(provide 'dirvish-helpers)

;;; dirvish-helpers.el ends here

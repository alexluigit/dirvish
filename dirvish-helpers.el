;;; dirvish-helpers.el --- Helper functions for Dirvish. -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Helper functions for dirvish.

;;; Code:

(declare-function dirvish "dirvish")
(declare-function dirvish-dired "dirvish")
(declare-function dirvish-reset "dirvish")
(declare-function dirvish-quit "dirvish")
(declare-function dirvish-body-update "dirvish-body")
(declare-function dirvish--body-render-icon "dirvish-body")
(declare-function dirvish--body-render-icon "dirvish-body")
(declare-function dirvish--add-advices "dirvish-advices")
(declare-function dirvish--clean-advices "dirvish-advices")
(require 'dirvish-structs)
(require 'dirvish-vars)
(require 'dired-x)

(defmacro dirvish-with-update (full-update &rest body)
  "Do necessary cleanup, execute BODY, update current dirvish.

If FULL-UPDATE is non-nil, redraw icons and reset line height.

Some overlays such as icons, line highlighting, need to be
removed or updated before update current dirvish instance."
  (declare (indent 1))
  `(progn
     (remove-overlays (point-min) (point-max) 'dirvish-body t)
     (when-let ((pos (dired-move-to-filename nil))
                dirvish-show-icons)
       (remove-overlays (1- pos) pos 'dirvish-icons t)
       (dirvish--body-render-icon pos))
     ,@body
     (let ((skip (not ,full-update)))
       (dirvish-body-update skip skip))
     (when-let ((curr-dv (dirvish-curr))
                (filename (dired-get-filename nil t)))
       (setf (dv-index-path curr-dv) filename)
       (dirvish-header-update)
       (dirvish-footer-update)
       (dirvish-debounce dirvish-preview-update dirvish-preview-delay))))

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

(defun dirvish--update-sorter ()
  "Sort files under the dirvish window.
The sort flag is accessed from `dv-sort-criteria'."
  (let ((sort-flag (cdr (dv-sort-criteria (dirvish-curr)))))
    (dired-sort-other (string-join (list dired-listing-switches sort-flag) " "))))

(defun dirvish--display-buffer (buffer alist)
  "Try displaying BUFFER at one side of the selected frame.

 This splits the window at the designated side of the
 frame.  ALIST is window arguments for the new-window, it has the
 same format with `display-buffer-alist'."
  (let* ((side (cdr (assq 'side alist)))
         (window-configuration-change-hook nil)
         (width (or (cdr (assq 'window-width alist)) 0.5))
         (height (cdr (assq 'window-height alist)))
         (size (or height (ceiling (* (frame-width) width))))
         (split-width-threshold 0)
         (root-win (dv-root-window (dirvish-curr)))
         (new-window (split-window-no-error root-win size side)))
    (window--display-buffer buffer new-window 'window alist)))

(defun dirvish--yank (&optional mode)
  "Paste marked files/directory to current directory according to MODE.

MODE can be `'copy', `'move', `symlink', or `relalink'."
  (interactive)
  (let* ((regexp (dired-marker-regexp))
         (yanked-files ())
         (mode (or mode 'copy))
         case-fold-search)
    (cl-dolist (buf (seq-filter #'buffer-live-p (dirvish-all-parent-buffers)))
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
    (dirvish-repeat dirvish-footer-update 0 dirvish-footer-repeat)
    (dirvish-repeat dirvish--set-IO-status 0 dirvish-footer-repeat)
    (cl-dolist (file new-fileset)
      (funcall paste-func (car file) (cdr file)))
    (cl-dolist (buf (dirvish-all-parent-buffers))
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
        (when (dirvish-live-p) (dirvish-reset))
        (setf (nth 0 (car-safe dirvish-IO-queue)) t)
        (when (eq (length dirvish-IO-queue) 1)
          (cancel-timer (symbol-value 'dirvish--set-IO-status-timer))))
      (setcar (nth 3 (car-safe dirvish-IO-queue)) progress))))

;;;###autoload
(defun dirvish-live-p (&optional win)
  "Detecting if WIN is in dirvish mode.

If WIN is nil, defaults to `\\(selected-window\\)'."
  (and
   (dirvish-curr)
   (memq (or win (selected-window)) (dv-parent-windows (dirvish-curr)))))

(provide 'dirvish-helpers)

;;; dirvish-helpers.el ends here

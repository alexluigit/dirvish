;;; dirvish-helpers.el --- Helper functions for Dirvish. -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Helper functions for dirvish.

;;; Code:

(declare-function dirvish "dirvish")
(declare-function dirvish-reset "dirvish")
(declare-function dirvish-quit "dirvish")
(declare-function dirvish-header-update "dirvish-header")
(declare-function dirvish-footer-update "dirvish-footer")
(declare-function dirvish-preview-update "dirvish-preview")
(declare-function dirvish--body-render-icon "dirvish-body")
(declare-function dirvish--body-render-icon "dirvish-body")
(declare-function dirvish--add-advices "dirvish-advices")
(declare-function dirvish--clean-advices "dirvish-advices")
(require 'dirvish-structs)
(require 'dirvish-vars)
(require 'dired-x)
(require 'posframe)

(defmacro dirvish-with-update (full-update &rest body)
  "Do necessary cleanup, execute BODY, update current dirvish.
If FULL-UPDATE is non-nil, redraw icons and reset line height."
  (declare (indent 1))
  `(progn
     (dirvish--update-frame-prepare)
     ,@body
     (dirvish--update-frame-execute ,full-update)))

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

(defun dirvish--update-frame-prepare ()
  "Make current line ready for `dirvish--update-frame-execute'.

Some overlays such as icons, line highlighting, need to be
removed or updated before update current dirvish instance."
  (remove-overlays (point-min) (point-max) 'dirvish-body t)
  (when-let ((pos (dired-move-to-filename nil))
             dirvish-show-icons)
    (remove-overlays (1- pos) pos 'dirvish-icons t)
    (dirvish--body-render-icon pos)))

(defun dirvish--update-frame-execute (&optional lazy)
  "Update current dirvish."
  (let ((skip (not lazy)))
    (dirvish-body-update skip skip)
    (when (dired-move-to-filename nil)
      (setf (dirvish-index-path (dirvish-meta)) (dired-get-filename nil t))
      (when (or (dirvish-header-width (dirvish-meta))
                (dirvish-one-window-p (dirvish-meta)))
        (dirvish-header-update))
      (dirvish-footer-update)
      (dirvish-debounce dirvish-preview-update dirvish-preview-delay))))

(defun dirvish-meta (&optional frame)
  "Get dirvish metadata of FRAME.

FRAME defaults to current frame."
  (frame-parameter frame 'dirvish-meta))

(defun dirvish-posframe-guard (one-window)
  "Make sure posframe workable under current env."
  (unless (or (posframe-workable-p) one-window)
    (user-error "Dirvish: posframe unable to initialize under current Emacs")))

(defun dirvish--clean-buffers ()
  "Cleanup all dirvish buffers."
  (cl-dolist (buf (buffer-list))
    (let ((name (buffer-name buf))
          (mode (buffer-local-value 'major-mode buf)))
      (when (or (eq 'dired-mode mode)
                (eq 'dirvish-mode mode)
                (and (not (string-equal name ""))
                     (string-match " \\*Dirvish I/O.*" name)
                     (not (get-buffer-process buf))))
        (kill-buffer buf)))))

;;;###autoload
(defun dirvish-live-p (&optional win)
  "Helper function for detecting if WIN is in dirvish mode.

If WIN is nil, defaults to `\\(selected-window\\)'."
  (memq (or win (selected-window)) dirvish-parent-windows))

(defun dirvish--update-sorter ()
  "Sort files under the dirvish window.
The sort flag is accessed from `dirvish-sort-criteria'."
  (let ((sort-flag (cdr (dirvish-sort-criteria (dirvish-meta)))))
    (dired-sort-other (string-join (list dired-listing-switches sort-flag) " "))))

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
         (root-win (dirvish-root-window (dirvish-meta)))
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
    (cl-dolist (buf (seq-filter #'buffer-live-p dirvish-parent-buffers))
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
        (when (dirvish-live-p) (dirvish-reset))
        (setf (nth 0 (car-safe dirvish-IO-queue)) t)
        (when (eq (length dirvish-IO-queue) 1)
          (cancel-timer (symbol-value 'dirvish--set-IO-status-timer))))
      (setcar (nth 3 (car-safe dirvish-IO-queue)) progress))))

(defun dirvish-init (&optional one-window)
  "Save previous window config and initialize dirvish.

If ONE-WINDOW is not-nil, initialize dirvish only in current
window, not the whole frame."
  (dirvish-posframe-guard one-window)
  (when (eq major-mode 'dirvish-mode) (dirvish-quit))
  (set-frame-parameter nil 'dirvish-meta (make--dirvish))
  (setf (dirvish-one-window-p (dirvish-meta)) one-window)
  (setf (dirvish-window-conf (dirvish-meta)) (current-window-configuration))
  (add-to-list 'dirvish-frame-list (window-frame))
  (when (window-parameter nil 'window-side) (delete-window)) ;; side window can not be split
  (setf (dirvish-root-window (dirvish-meta)) (frame-selected-window))
  (unless dirvish-initialized
    (dirvish--add-advices)
    (when (dirvish--get-IO-status)
      (dirvish-repeat dirvish-footer-update 0 dirvish-footer-repeat)
      (dirvish-repeat dirvish--set-IO-status 0 dirvish-footer-repeat))
    (setq dirvish-initialized t)))

(defun dirvish-deinit ()
  "Revert previous window config and deinit dirvish."
  (setq dirvish-initialized nil)
  (setq recentf-list (dirvish-saved-recentf (dirvish-meta)))
  (mapc #'kill-buffer dirvish-preview-buffers)
  (let ((one-window-p (dirvish-one-window-p (dirvish-meta)))
        (config (dirvish-window-conf (dirvish-meta))))
    (posframe-delete (dirvish-header-buffer (dirvish-meta)))
    (setq dirvish-frame-list (delq (window-frame) dirvish-frame-list))
    (when (window-configuration-p config)
      (set-window-configuration config))
    (unless
        (or (and one-window-p (> (length dirvish-parent-windows) 1))
            (> (length dirvish-frame-list) 1))
      (dirvish--clean-buffers)
      (dirvish--clean-advices)
      (dolist (tm dirvish-repeat-timers) (cancel-timer (symbol-value tm))))
    (setq dirvish-parent-windows ())
    (setq dirvish-preview-buffers ())
    (setq dirvish-parent-buffers ())))

(defun dirvish-override-dired (_fn &optional _other-win path)
  "Helper func for `dirvish-override-dired-mode'.

PATH is passed from `dired-jump' if called with `prefix-arg'."
  (dirvish path (or (not window-system)
                    (not (= (length (window-list)) 1)))))

(provide 'dirvish-helpers)

;;; dirvish-helpers.el ends here

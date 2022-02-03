;;; dirvish-helpers.el --- Helper functions for Dirvish -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Helper functions for dirvish.

;;; Code:

(require 'dirvish-structs)
(require 'dirvish-options)
(require 'dired-x)

(defmacro dirvish-here (&optional path &rest keywords)
  "Open Dirvish with PATH and KEYWORDS.

PATH defaults to variable `buffer-file-name'.
KEYWORDS are slot key-values for `dirvish-new'."
  (declare (indent defun))
  (interactive)
  `(let* ((file (or ,path buffer-file-name))
          (dir (if file (expand-file-name (file-name-directory file))
                 (expand-file-name default-directory))))
     (dirvish-activate (dirvish-new ,@keywords))
     (dirvish-find-file dir)))

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
       (unless (timerp ,timer)
         (setq ,timer (run-with-idle-timer ,delay nil ,do-once ,@args))))))

(defun dirvish-setup-dired-buffer (&rest _)
  "Setup Dired buffer for dirvish.
This function removes the header line in a Dired buffer."
  (save-excursion
    (let ((o (make-overlay
              (point-min)
              (progn (goto-char (point-min)) (forward-line 1) (point)))))
      (overlay-put o 'invisible t))))

(defun dirvish--shell-to-string (program &rest args)
  "Execute PROGRAM with arguments ARGS and return output string.

If program returns non zero exit code return nil."
  (let* ((exit-code nil)
         (output
          (with-output-to-string
            (with-current-buffer standard-output
              (setq exit-code (apply #'process-file program nil t nil args))))))
    (when (eq exit-code 0) output)))

(defun dirvish-format-header-line ()
  "Format Dirvish header line."
  (when (dirvish-curr)
    (let* ((str (format-mode-line dirvish-header-line-format))
           (ht (if (eq dirvish-header-style 'large) 1.2 1))
           (win-width (1- (* (frame-width) (- 1 dirvish-preview-width))))
           (max-width (floor (/ win-width ht))))
      (while (>= (+ (length str) (/ (- (string-bytes str) (length str)) 2)) (1- max-width))
        (setq str (substring str 0 -1)))
      (propertize str 'display `((height ,ht) (raise ,(/ (- dirvish-header-line-height ht) 2)))))))

(defun dirvish-format-mode-line ()
  "Generate Dirvish mode line string."
  (when (dirvish-curr)
    (cl-destructuring-bind (left . right) dirvish-mode-line-format
      (let ((fmt-right (format-mode-line right)))
        (concat (format-mode-line left)
                (propertize " " 'display
                            `((space :align-to (- (+ right right-fringe right-margin)
                                                  ,(string-width fmt-right)))))
                fmt-right)))))

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

(defun dirvish--enlarge (&rest _)
  "Kill all dirvish parent windows except the root one."
  (when (dirvish-live-p)
    (cl-dolist (win (dv-parent-windows (dirvish-curr)))
      (and (not (eq win (dv-root-window (dirvish-curr))))
           (window-live-p win)
           (delete-window win)))))

(defun dirvish--buffer-for-dir (dv entry switches)
  "Return the dirvish buffer in DV for ENTRY.
If the buffer is not available, create it with `ls' SWITCHES."
  (let* ((root-dir-buf (dv-root-dir-buf-alist dv))
         (buffer (alist-get entry root-dir-buf nil nil #'equal)))
    (unless buffer
      (setq buffer (dired-noselect entry switches))
      (push (cons entry buffer) (dv-root-dir-buf-alist dv)))
    buffer))

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

(defun dirvish--append-metadata (metadata completions)
  "Append METADATA for minibuffer COMPLETIONS."
  (let ((entry (if (functionp metadata)
                   `(metadata (annotation-function . ,metadata))
                 `(metadata (category . ,metadata)))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          entry
        (complete-with-action action completions string pred)))))

(provide 'dirvish-helpers)
;;; dirvish-helpers.el ends here

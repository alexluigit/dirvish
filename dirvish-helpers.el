;;; dirvish-helpers.el --- Helper functions for Dirvish -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Helper functions for dirvish.

;;; Code:

(require 'dirvish-structs)
(require 'dirvish-options)
(require 'dired-x)

(defmacro dirvish-with-update (full-update &rest body)
  "Do necessary cleanup, execute BODY, update current dirvish.

If FULL-UPDATE is non-nil, redraw icons and reset line height.

Some overlays such as icons, line highlighting, need to be
removed or updated before update current dirvish instance."
  (declare (indent 1))
  `(progn
     (when-let ((dv (dirvish-curr)))
       (remove-overlays (point-min) (point-max) 'dirvish-body t)
       (when-let ((pos (dired-move-to-filename nil))
                  dirvish-show-icons)
         (remove-overlays (1- pos) pos 'dirvish-icons t)
         (dirvish--render-icon pos))
       ,@body
       (let ((skip (not ,full-update)))
         (dirvish-body-update skip skip))
       (when-let ((filename (dired-get-filename nil t)))
         (setf (dv-index-path dv) filename)
         (dirvish-debounce dirvish-mode-line-update dirvish-debouncing-delay)
         (dirvish-debounce dirvish-preview-update dirvish-debouncing-delay)))))

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

(defun dirvish-render (renderer &optional range)
  "Call RENDERER on RANGE in viewport.

Where RENDERER is a function which takes a position (point in
current line) and optional face as args.  RANGE is a beginning
and end position cons, default to buffer start and end position
within the viewport."
  (save-excursion
    (let ((beg (or (car range) (- 0 (frame-height))))
          (end (or (cdr range) (+ (line-number-at-pos) (frame-height)))))
      (forward-line beg)
      (while (and (not (eobp)) (< (line-number-at-pos) end))
        (when-let ((pos (dired-move-to-filename nil)))
          (funcall renderer pos))
        (forward-line 1)))))

(defun dirvish-format-header-line ()
  "Generate Dirvish header line string."
  (let* ((str (format-mode-line dirvish-header-line-format))
         (ht (1+ (if (eq dirvish-header-style 'large) 0.25 dirvish-body-fontsize-increment)))
         (win-width (if dirvish-enable-preview
                    (1- (* (frame-width) (- 1 dirvish-preview-width)))
                  (frame-width)))
         (max-width (floor (/ win-width ht))))
    (while (>= (+ (length str) (/ (- (string-bytes str) (length str)) 2)) (1- max-width))
      (setq str (substring str 0 -1)))
    (propertize str 'display `((height ,ht) (raise ,(/ 0.16 ht))))))

(defun dirvish-format-mode-line ()
  "Generate Dirvish mode line string."
  (cl-destructuring-bind (left . right) dirvish-mode-line-format
    (let ((fmt-right (format-mode-line right)))
      (concat (format-mode-line left)
              (propertize " " 'display
                          `((space :align-to (- (+ right right-fringe right-margin)
                                                ,(string-width fmt-right)))))
              fmt-right))))

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

(provide 'dirvish-helpers)
;;; dirvish-helpers.el ends here

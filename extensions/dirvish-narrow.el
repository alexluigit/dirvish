;;; dirvish-narrow.el --- Live-narrowing of search results for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides live filtering of files in Dirvish buffers.  It is a
;; stripped-down version of `dired-narrow'.

;;; Code:

(require 'dirvish)
(declare-function dirvish-subtree--revert "dirvish-subtree")

(defcustom dirvish-narrow-regex-builder
  (if (functionp 'orderless-pattern-compiler)
      #'orderless-pattern-compiler
    #'split-string)
  "Function used to compose the regex list for narrowing.
The function takes the input string as its sole argument and
should return a list of regular expressions."
  :group 'dirvish :type 'function)

(defcustom dirvish-narrow-debounce 0.2
  "Like `dirvish-redisplay-debounce', but used for narrowing."
  :group 'dirvish :type 'float)

(defvar dirvish-narrow-debounce-timer nil)
(defvar-local dirvish-narrow--subdir-alist '())

(defun dirvish-narrow--build-indices ()
  "Update the Dirvish buffer based on the input of the minibuffer."
  (setq dirvish-narrow--subdir-alist '())
  (when (bound-and-true-p dirvish-subtree--overlays)
    (dirvish-subtree--revert t))
  (save-excursion
    (with-current-buffer (window-buffer (minibuffer-selected-window))
      (cl-loop for (dir . beg) in dired-subdir-alist do
               (dirvish-narrow--index-subdir dir beg)))))

(defun dirvish-narrow-update-h ()
  "Update the Dirvish buffer based on the input of the minibuffer."
  (dirvish-debounce narrow
    (let* ((input (minibuffer-contents-no-properties))
           (regex-list (funcall dirvish-narrow-regex-builder input)))
      (with-current-buffer (window-buffer (minibuffer-selected-window))
        (save-excursion
          (cl-loop for idx from 0
                   for (dir . pos) in dired-subdir-alist
                   do (dirvish-narrow--filter-subdir dir pos regex-list idx)))
        (dirvish-update-body-h)))))

(defun dirvish-narrow--revert ()
  "Revert Dirvish buffer with empty narrowing filter."
  (cl-loop for idx from 0
           for (dir . pos) in dired-subdir-alist
           do (dirvish-narrow--filter-subdir dir pos nil idx)))

(cl-defun dirvish-narrow--index-subdir (subdir beg)
  "Filter the SUBDIR from BEG to END."
  (goto-char beg)
  (let ((end (dired-subdir-max)) files)
    (while (< (point) end)
      (when-let* ((f-beg (dired-move-to-filename))
                  (f-end (dired-move-to-end-of-filename))
                  (f-name (buffer-substring-no-properties f-beg f-end))
                  (l-beg (line-beginning-position))
                  (l-end (1+ (line-end-position)))
                  (l-str (buffer-substring l-beg l-end)))
        (push (cons f-name l-str) files))
      (forward-line 1))
    (push (cons subdir (reverse files)) dirvish-narrow--subdir-alist)))

(defun dirvish-narrow--filter-subdir (dir pos regexs idx)
  "Filter the subdir DIR in POS with REGEXS.
IDX the index of DIR in `dired-subdir-alist'."
  (goto-char pos)
  (let* ((files (alist-get dir dirvish-narrow--subdir-alist nil nil #'equal))
         (end (- (dired-subdir-max) (if (eq idx 0) 0 1)))
         (offset (1- (line-number-at-pos (dirvish-prop :content-begin))))
         (beg (progn (forward-line offset) (point)))
         buffer-read-only)
    (delete-region beg end)
    (if (not regexs)
        (cl-loop for (_ . line) in files do (insert line))
      (cl-loop for (file . line) in files
               unless (cl-loop for regex in regexs
                               thereis (not (string-match regex file)))
               do (insert line)))))

(defun dirvish-narrow-minibuffer-setup-h ()
  "Minibuffer setup function for `dirvish-narrow'."
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (goto-char (dirvish-prop :content-begin))
    (dirvish-update-body-h))
  (add-hook 'post-command-hook #'dirvish-narrow-update-h nil t))

;;;###autoload
(defun dirvish-narrow ()
  "Narrow a Dirvish buffer to the files matching a regex."
  (interactive)
  (dirvish-narrow--build-indices)
  (when (minibufferp) (user-error "`%s' called inside the minibuffer" this-command))
  (let ((old-f (dirvish-prop :index)) final-input)
    (minibuffer-with-setup-hook #'dirvish-narrow-minibuffer-setup-h
      (unwind-protect
          (setq final-input (read-from-minibuffer "Focus on files: "))
        (when (= (length final-input) 0) (dirvish-narrow--revert))
        (dired-goto-file old-f)))))

(provide 'dirvish-narrow)
;;; dirvish-narrow.el ends here

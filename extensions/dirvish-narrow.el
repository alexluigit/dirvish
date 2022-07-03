;;; dirvish-narrow.el --- Live-narrowing of search results for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.8.14
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides live filtering of files in Dirvish buffers.  It is a
;; stripped-down version of `dired-narrow'.

;;; Code:

(require 'dirvish)
(declare-function dirvish-subtree--revert "dirvish-subtree")

(defvar-local dirvish-narrow--subdir-alist '())

(defun dirvish-narrow--build-indices ()
  "Update the Dirvish buffer based on the input of the minibuffer."
  (setq dirvish-narrow--subdir-alist '())
  (when (bound-and-true-p dirvish-subtree--overlays)
    (dirvish-subtree--revert t))
  (with-current-buffer (window-buffer (minibuffer-selected-window))
      (cl-loop for (dir . beg) in dired-subdir-alist do
               (progn (goto-char beg)
                      (forward-line (if dirvish--dired-free-space 2 1))
                      (dirvish-narrow--index-subdir dir (dired-subdir-max))))))

(defun dirvish-narrow-dirvish-update-h ()
  "Update the Dirvish buffer based on the input of the minibuffer."
  (dirvish-debounce 'narrow
    (let ((input (minibuffer-contents-no-properties)))
      (with-current-buffer (window-buffer (minibuffer-selected-window))
        (cl-loop for idx from 0
                 for (dir . pos) in dired-subdir-alist
                 do (progn (goto-char pos)
                           (forward-line (if dirvish--dired-free-space 2 1))
                           (dirvish-narrow--filter-subdir dir input idx)))
        (goto-char (window-start))
        (dired-goto-file (dirvish-prop :child))
        (dirvish-update-body-h)))))

(cl-defun dirvish-narrow--index-subdir (subdir end)
  "Filter the SUBDIR from BEG to END."
  (let (files)
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

(defun dirvish-narrow--filter-subdir (dir regex idx)
  "Filter the subdir DIR with REGEX.
IDX the index of DIR in `dired-subdir-alist'."
  (let ((files (alist-get dir dirvish-narrow--subdir-alist nil nil #'equal))
        (p-max (- (dired-subdir-max) (if (eq idx 0) 0 1)))
        buffer-read-only)
    (delete-region (point) p-max)
    (cl-loop for (file . line) in files do
             (when (string-match regex file)
               (insert line)))))

(defun dirvish-narrow-minibuffer-setup-h ()
  "Minibuffer setup function for `dirvish-narrow'."
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (if (>= (line-number-at-pos (point-max)) (frame-height))
        (goto-char (window-start))
      (dired-goto-file (dirvish-prop :child)))
    (dirvish-update-body-h))
  (add-hook 'post-command-hook #'dirvish-narrow-dirvish-update-h nil t))

;;;###autoload
(defun dirvish-narrow ()
  "Narrow a Dirvish buffer to the files matching a regex."
  (interactive)
  (dirvish-narrow--build-indices)
  (when (minibufferp) (user-error "`%s' called inside the minibuffer" this-command))
  (let ((old-f (dirvish-prop :child)))
    (minibuffer-with-setup-hook #'dirvish-narrow-minibuffer-setup-h
      (unwind-protect
          (read-from-minibuffer "Focus on files: ")
        (dired-goto-file old-f)))))

(provide 'dirvish-narrow)
;;; dirvish-narrow.el ends here

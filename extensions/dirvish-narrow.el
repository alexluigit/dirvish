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

(defun dirvish-narrow-dirvish-update-h ()
  "Update the Dirvish buffer based on the input of the minibuffer."
  (dirvish-debounce 'narrow
    (let ((input (minibuffer-contents-no-properties)))
      (with-current-buffer (window-buffer (minibuffer-selected-window))
        (cl-loop for (_dir . pos) in dired-subdir-alist do
                 (progn (goto-char pos)
                        (forward-line (if dirvish--dired-free-space 2 1))
                        (dirvish-narrow-subdir input pos (dired-subdir-max))))
        (dirvish-update-body-h)))))

(cl-defun dirvish-narrow-subdir (regex beg end)
  "Filter the subdir from BEG to END with REGEX."
  (remove-overlays beg end 'dirvish-narrow-ov t)
  (when (equal regex "") (cl-return-from dirvish-narrow-subdir))
  (while (< (point) end)
    (when-let ((f-beg (dired-move-to-filename))
               (f-end (dired-move-to-end-of-filename)))
      (let* ((f-name (buffer-substring-no-properties f-beg f-end))
             (match (string-match regex f-name)))
        (unless match
          (let* ((l-beg (line-beginning-position))
                 (l-end (line-end-position))
                 (ov (make-overlay l-beg (1+ l-end))))
            (overlay-put ov 'dirvish-narrow-ov t)
            (overlay-put ov 'invisible t)
            (overlay-put ov 'evaporate t)))))
    (forward-line 1)))

;;;###autoload
(defun dirvish-narrow (&optional show)
  "Narrow a Dirvish buffer to the files matching a regex.
With optional prefix argument SHOW reveal the hidden lines."
  (interactive "P")
  (if show
      (progn (remove-overlays (point-min) (point-max) 'dirvish-narrow-ov t)
             (message "All files revealed"))
    (when (minibufferp) (user-error "`%s' called inside the minibuffer" this-command))
    (minibuffer-with-setup-hook
        (lambda () (add-hook 'post-command-hook #'dirvish-narrow-dirvish-update-h nil t))
      (read-from-minibuffer "Focus on files: "))))

(provide 'dirvish-narrow)
;;; dirvish-narrow.el ends here

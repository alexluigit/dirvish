;;; dirvish-narrow.el --- Live-narrowing of search results for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.2.7
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides live filtering of files in Dirvish buffers.  It is a
;; stripped-down version of `dired-narrow'.

;;; Code:

(require 'dirvish)

;; Credit: copied from `orderless.el'
(defcustom dirvish-narrow-match-faces
  [dirvish-narrow-match-face-0
   dirvish-narrow-match-face-1
   dirvish-narrow-match-face-2
   dirvish-narrow-match-face-3]
  "Vector of faces used (cyclically) for component matches."
  :group 'dirvish :type '(vector face))

(defface dirvish-narrow-match-face-0
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#72a4ff")
    (((class color) (min-colors 88) (background light)) :foreground "#223fbf")
    (t :foreground "blue"))
  "Face for matches of components numbered 0 mod 4."
  :group 'dirvish)

(defface dirvish-narrow-match-face-1
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#ed92f8")
    (((class color) (min-colors 88) (background light)) :foreground "#8f0075")
    (t :foreground "magenta"))
  "Face for matches of components numbered 1 mod 4."
  :group 'dirvish)

(defface dirvish-narrow-match-face-2
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#90d800")
    (((class color) (min-colors 88) (background light)) :foreground "#145a00")
    (t :foreground "green"))
  "Face for matches of components numbered 2 mod 4."
  :group 'dirvish)

(defface dirvish-narrow-match-face-3
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "#f0ce43")
    (((class color) (min-colors 88) (background light)) :foreground "#804000")
    (t :foreground "yellow"))
  "Face for matches of components numbered 3 mod 4."
  :group 'dirvish)

(defun dirvish-narrow--compile-regex (string)
  "Compile `completion-regexp-list' from STRING."
  (if (fboundp 'orderless-compile) (cdr (orderless-compile string))
    (split-string string)))

(defun dirvish-narrow--highlight (regexps ignore-case string)
  "Destructively propertize STRING to highlight a match of each of the REGEXPS.
The search is case insensitive if IGNORE-CASE is non-nil."
  (cl-loop with case-fold-search = ignore-case
           with n = (length dirvish-narrow-match-faces)
           for regexp in regexps and i from 0
           when (string-match regexp string) do
           (cl-loop
            for (x y) on (let ((m (match-data))) (or (cddr m) m)) by #'cddr
            when x do (add-face-text-property
                       x y (aref dirvish-narrow-match-faces (mod i n))
                       nil string)))
  string)

(defun dirvish-narrow--build-indices ()
  "Update the Dirvish buffer based on the input of the minibuffer."
  (declare-function dirvish-subtree--revert "dirvish-subtree")
  (when (bound-and-true-p dirvish-subtree--overlays)
    (dirvish-subtree--revert t))
  (save-excursion
    (cl-loop
     for (dir . beg) in dired-subdir-alist
     if (and (equal dir (expand-file-name default-directory))
             (dirvish-prop :fd-arglist))
     do (puthash (md5 dir) (dirvish-prop :fd-cache) dirvish--dir-data)
     else do (goto-char beg)
     (let ((end (dired-subdir-max)) (files (dirvish--ht)))
       (while (< (point) end)
         (when-let* ((f-beg (dired-move-to-filename))
                     (f-end (dired-move-to-end-of-filename))
                     (f-name (buffer-substring-no-properties f-beg f-end))
                     (l-beg (line-beginning-position))
                     (l-end (1+ (line-end-position)))
                     (l-str (buffer-substring l-beg l-end)))
           (puthash f-name l-str files))
         (forward-line 1))
       (puthash (md5 dir) files dirvish--dir-data)))))

;; use a separate timer here, otherwise it would be overrided by the default one
(defvar dirvish-narrow--delay-timer `(,(timer-create) ,(float-time) nil))

(defun dirvish-narrow-update-h ()
  "Update the Dirvish buffer based on the input of the minibuffer."
  (dirvish--run-with-delay
    (minibuffer-contents-no-properties) dirvish-narrow--delay-timer
    (lambda (action)
      (with-current-buffer (window-buffer (minibuffer-selected-window))
        (save-excursion
          (cl-loop with regs = (dirvish-narrow--compile-regex action)
                   for (dir . pos) in dired-subdir-alist and idx from 0
                   do (dirvish-narrow--subdir dir pos regs idx)))))))

(defun dirvish-narrow--subdir (dir pos regexs idx &optional all)
  "Narrow subdir DIR at index IDX in POS with REGEXS."
  (delete-region
   (progn (goto-char pos) (forward-line (dirvish--subdir-offset)) (point))
   (- (dired-subdir-max) (if (eq idx 0) 0 1)))
  (cl-loop with completion-regexp-list = regexs
           with completion-ignore-case =
           (cl-loop for re in (ensure-list regexs)
                    always (isearch-no-upper-case-p re t))
           with files = (gethash (md5 dir) dirvish--dir-data)
           and fr-h = (+ (frame-height) 5) and count = 0
           with pred = (if all #'always (lambda (&rest _) (<= (cl-incf count) fr-h)))
           for f in (all-completions "" files pred)
           for l = (concat (gethash f files)) ; use copy, not reference
           do (insert (if all l (dirvish-narrow--highlight ; lazy highlighting
                                 regexs completion-ignore-case l)))
           finally do (dirvish-prop :count count)))

;;;###autoload
(defun dirvish-narrow ()
  "Narrow a Dirvish buffer to the files matching a regex."
  (interactive nil dired-mode)
  (when (get-buffer-process (current-buffer))
    (user-error "Current buffer has unfinished jobs"))
  (require 'orderless nil t)
  (dirvish-narrow--build-indices)
  (let ((dv (dirvish-prop :dv)) (restore (dirvish-prop :index))
        input buffer-read-only)
    (font-lock-mode -1) (buffer-disable-undo)
    (minibuffer-with-setup-hook
        (lambda ()
          (dirvish-prop :dv dv)
          (add-hook 'post-command-hook #'dirvish-narrow-update-h nil t))
      (unwind-protect
          (setq input (read-from-minibuffer "Focus on files: "))
        (save-excursion
          (cl-loop with re = (dirvish-narrow--compile-regex (or input ""))
                   for (d . p) in dired-subdir-alist and i from 0
                   do (dirvish-narrow--subdir d p re i (or input ""))))
        (dirvish-prop :count nil)
        (when restore (dired-goto-file restore))
        (dirvish--run-with-delay 'reset)
        (dirvish--run-with-delay 'reset dirvish-narrow--delay-timer)
        (font-lock-mode 1) (buffer-enable-undo)))))

(provide 'dirvish-narrow)
;;; dirvish-narrow.el ends here

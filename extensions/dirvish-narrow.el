;;; dirvish-narrow.el --- Live-narrowing of search results for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.3.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides live filtering of files in Dirvish buffers.  It is a
;; stripped-down version of `dired-narrow'.

;;; Code:

(require 'dirvish-fd)

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

(defface dirvish-narrow-split
  '((t :inherit font-lock-negation-char-face))
  "Face used to highlight punctuation character."
  :group 'dirvish)

(defun dirvish-narrow--build-indices ()
  "Update the Dirvish buffer based on the input of the minibuffer."
  (save-excursion
    (cl-loop
     for (dir . beg) in dired-subdir-alist and idx from 0
     unless (and (eq idx 0) (dirvish-prop :fd-info))
     do (goto-char beg)
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

(defun dirvish-narrow--compiler (s)
  "Compile `completion-regexp-list' from string S."
  (if (fboundp 'orderless-compile) (cdr (orderless-compile s)) (split-string s)))

(defun dirvish-narrow-update-h ()
  "Update the Dirvish buffer based on the input of the minibuffer."
  (let* ((mc (minibuffer-contents-no-properties))
         (filter mc) async rel igc)
    (save-match-data
      (when-let* (((string-match "^#\\([^ #]*\\)\\(.*\\)" mc))
                  (beg (minibuffer-prompt-end)))
        (add-text-properties beg (1+ beg) '(rear-nonsticky t))
        (add-face-text-property beg (1+ beg) 'dirvish-narrow-split)
        (setq async (match-string 1 mc) filter (match-string 2 mc))))
    (with-current-buffer (cdr (dv-index (dirvish-curr)))
      (when (and async (dirvish-prop :fd-info))
        (dirvish-fd--argparser (mapcan (lambda (x) `(,(format "--and=%s" x)))
                                       (split-string async "," t))
                               (cddr (dirvish-prop :fd-info))))
      (setq rel (dirvish-narrow--compiler filter)
            igc (cl-loop for re in (ensure-list rel)
                         always (isearch-no-upper-case-p re t)))
      (dirvish-prop :narrow-info (list async rel igc)))
    (dirvish--run-with-delay mc :narrow
      (lambda (_action)
        (with-current-buffer (cdr (dv-index (dirvish-curr)))
          (when (dirvish-prop :fd-info) (dirvish-fd--start-proc))
          (save-excursion
            (cl-loop for (dir . pos) in dired-subdir-alist and idx from 0
                     do (delete-region
                         (progn (goto-char pos)
                                (forward-line (dirvish--subdir-offset)) (point))
                         (- (dired-subdir-max) (if (eq idx 0) 0 1)))
                     unless (and (eq idx 0) (dirvish-prop :fd-info))
                     do (cl-loop with files = (gethash (md5 dir) dirvish--dir-data)
                                 with completion-regexp-list = rel
                                 with completion-ignore-case = igc
                                 for f in (all-completions "" files)
                                 do (insert (gethash f files))))))
        (when (dv-curr-layout (dirvish-curr)) (force-mode-line-update t))))))

(dirvish-define-attribute narrow-match
  "Highlight matched part of narrowed files."
  (cl-loop with (_ regexps case-fold-search) = (dirvish-prop :narrow-info)
           with n = (length dirvish-narrow-match-faces) with ovs = nil
           for regexp in regexps and i from 0
           when (string-match regexp f-str) do
           (cl-loop
            for (x y) on (let ((m (match-data))) (or (cddr m) m)) by #'cddr
            when x do (let ((ov (make-overlay (+ f-beg x) (+ f-beg y)))
                            (face (aref dirvish-narrow-match-faces (mod i n))))
                        (overlay-put ov 'face face)
                        (push ov ovs)))
           finally return `(ovs . ,ovs)))

;;;###autoload
(defun dirvish-narrow ()
  "Narrow a Dirvish buffer to the files matching a regex."
  (interactive nil dired-mode)
  (when (bound-and-true-p dirvish-subtree--overlays)
    (declare-function dirvish-subtree--revert "dirvish-subtree")
    (dirvish-subtree--revert t))
  (require 'orderless nil t)
  (dirvish-narrow--build-indices)
  (let ((dv (dirvish-prop :dv))
        (idx (dirvish-prop :index))
        (fd (dirvish-prop :fd-info))
        (attrs (mapcar #'car (dirvish-prop :attrs)))
        buffer-read-only)
    (when fd
      (setq dired-subdir-alist (list (car (reverse dired-subdir-alist))))
      (delete-region (goto-char (dirvish-prop :content-begin)) (point-max)))
    (dirvish-prop :attrs
      (dirvish--attrs-expand (append '(narrow-match) attrs)))
    (minibuffer-with-setup-hook
        (lambda ()
          (dirvish-prop :dv dv)
          (add-hook 'post-command-hook #'dirvish-narrow-update-h nil t))
      (unwind-protect
          (read-from-minibuffer "Focus on files: " (if fd "#" ""))
        (when idx (dired-goto-file idx))
        (dirvish-prop :attrs (dirvish--attrs-expand attrs))
        (when-let* (((not (eq (dv-type (dirvish-curr)) 'side)))
                    (query (caar (dirvish-prop :fd-info)))
                    (key (file-name-nondirectory
                          (directory-file-name default-directory))))
          (rename-buffer (concat key "🔍" query "🔍" (dv-id (dirvish-curr)))))
        (dirvish--run-with-delay 'reset)
        (dirvish--run-with-delay 'reset :narrow)))))

(provide 'dirvish-narrow)
;;; dirvish-narrow.el ends here

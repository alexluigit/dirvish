;;; dirvish-peek.el --- Minibuffer file preview powered by Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `dirvish-peek-mode' gives you file preview when narrowing candidates using minibuffer.

;;; Code:

(declare-function vertico--candidate "vertico")
(declare-function selectrum--get-candidate "selectrum")
(declare-function selectrum--get-full "selectrum")
(defvar selectrum--current-candidate-index)
(declare-function ivy-state-current "ivy")
(defvar ivy-last)
(require 'dirvish)
(require 'find-func)

(defcustom dirvish-peek-candidate-fetcher nil
  "Function to get current candidate in minibuffer.
If this value is nil, a candidate fetcher function is
automatically choosed according to your completion framework
being used at runtime."
  :group 'dirvish :type '(choice function nil))

(defcustom dirvish-peek-categories '(file project-file library)
  "Minibuffer metadata categories to show file preview."
  :group 'dirvish :type 'list)

(defvar dirvish-peek--cand-fetcher nil)
(defun dirvish-peek--prepare-cand-fetcher ()
  "Set candidate fetcher according to current completion framework."
  (setq dirvish-peek--cand-fetcher
        (cond (dirvish-peek-candidate-fetcher
               dirvish-peek-candidate-fetcher)
              ((bound-and-true-p vertico-mode) #'vertico--candidate)
              ((bound-and-true-p selectrum-mode)
               (lambda ()
                 (selectrum--get-full
                  (selectrum--get-candidate
                   selectrum--current-candidate-index))))
              ((bound-and-true-p ivy-mode) (lambda () (ivy-state-current ivy-last)))
              ((bound-and-true-p icomplete-mode)
               (lambda () (car completion-all-sorted-completions))))))

(defvar dirvish-peek--curr-category nil)
(defun dirvish-peek-setup-h ()
  "Create dirvish minibuffer preview window.
The window is created only when metadata in current minibuffer is
one of categories in `dirvish-peek-categories'."
  (let* ((meta (ignore-errors
                 (completion-metadata
                  (buffer-substring-no-properties (field-beginning) (point))
                  minibuffer-completion-table
                  minibuffer-completion-predicate)))
         (category (completion-metadata-get meta 'category))
         (p-category (and (memq category dirvish-peek-categories) category))
         new-dv)
    (setq dirvish-peek--curr-category p-category)
    (when p-category
      (dirvish-peek--prepare-cand-fetcher)
      (add-hook 'post-command-hook #'dirvish-peek-update-h 90 t)
      (add-hook 'minibuffer-exit-hook #'dirvish-peek-exit-h nil t)
      (unless (and dirvish--this (dv-preview-window dirvish--this))
        (setq new-dv (dirvish-new :type '(peek)))
        (setf (dv-preview-window new-dv)
              (or (minibuffer-selected-window) (next-window)))))))

(defun dirvish-peek-update-h ()
  "Hook for `post-command-hook' to update peek window."
  (when-let* ((dirvish-peek--curr-category)
              (cand (funcall dirvish-peek--cand-fetcher)))
    (pcase dirvish-peek--curr-category
      ('file
       (setq cand (expand-file-name cand)))
      ('project-file
       (setq cand (expand-file-name
                   cand (or (dirvish--get-project-root)
                            (car (minibuffer-history-value))))))
      ('library
       (setq cand (file-truename
                   (or (ignore-errors (find-library-name cand)) "")))))
    (unless (file-remote-p cand)
      (dirvish-debounce nil (dirvish--preview-update dirvish--this cand)))))

(defun dirvish-peek-exit-h ()
  "Hook for `minibuffer-exit-hook' to destroy peek session."
  (dolist (dv (hash-table-values dirvish--session-hash))
    (when (eq (car (dv-type dv)) 'peek)
      (dirvish-kill dv)
      (remhash (dv-name dv) dirvish--session-hash))))

;;;###autoload
(define-minor-mode dirvish-peek-mode
  "Show file preview when narrowing candidates using minibuffer."
  :group 'dirvish :global t
  (if dirvish-peek-mode
      (add-hook 'minibuffer-setup-hook #'dirvish-peek-setup-h)
    (remove-hook 'minibuffer-setup-hook #'dirvish-peek-setup-h)))

(provide 'dirvish-peek)
;;; dirvish-peek.el ends here

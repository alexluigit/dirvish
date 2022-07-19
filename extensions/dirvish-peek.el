;;; dirvish-peek.el --- Minibuffer file preview powered by Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.9.23
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

(defcustom dirvish-peek-display-alist
  '((side . right)
    (slot . -1)
    (window-width . 0.5))
  "Display alist for preview window of `dirvish-peek'."
  :group 'dirvish :type 'alist)

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
              ((bound-and-true-p icomplete-mode) (lambda () (car completion-all-sorted-completions))))))

(defun dirvish-peek--create ()
  "Create dirvish minibuffer preview window.
The window is created only when metadata in current minibuffer is
one of categories in `dirvish-peek-categories'."
  (let* ((old-dv (dirvish-curr))
         (meta (ignore-errors
                 (completion-metadata
                  (buffer-substring-no-properties (field-beginning) (point))
                  minibuffer-completion-table
                  minibuffer-completion-predicate)))
         (category (completion-metadata-get meta 'category))
         (preview-category (and (memq category dirvish-peek-categories) category))
         new-dv)
    (when preview-category
      (dirvish-peek--prepare-cand-fetcher)
      (add-hook 'post-command-hook #'dirvish-peek-update-h 99 t)
      (unless (and old-dv (dv-preview-window old-dv))
        (setq new-dv (dirvish-new nil))
        (setf (dv-preview-window new-dv)
              (display-buffer-in-side-window
               (dirvish--util-buffer)
               dirvish-peek-display-alist))))
    (set-frame-parameter nil 'dirvish--peek
                         `(:category ,preview-category :old ,old-dv))))

(defun dirvish-peek--teardown ()
  "Teardown dirvish minibuffer preview window."
  (let* ((dv-mini (frame-parameter nil 'dirvish--peek))
         (old-dv (plist-get dv-mini :old)))
    (set-frame-parameter nil 'dirvish--curr old-dv)))

(defun dirvish-peek-update-h ()
  "Hook for `post-command-hook' to update peek window."
  (when-let* ((category
               (plist-get (frame-parameter nil 'dirvish--peek) :category))
              (cand (funcall dirvish-peek--cand-fetcher)))
    (pcase category
      ('file
       (setq cand (expand-file-name cand)))
      ('project-file
       (setq cand (expand-file-name cand (or (dirvish--get-project-root)
                                             (car (minibuffer-history-value))))))
      ('library
       (setq cand (file-truename (or (ignore-errors (find-library-name cand)) "")))))
    (dirvish-prop :child cand)
    (dirvish-debounce layout (dirvish-preview-update))))

;;;###autoload
(define-minor-mode dirvish-peek-mode
  "Show file preview when narrowing candidates using minibuffer."
  :group 'dirvish :global t
  (if dirvish-peek-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'dirvish-peek--create)
        (add-hook 'minibuffer-exit-hook #'dirvish-peek--teardown))
    (remove-hook 'minibuffer-setup-hook #'dirvish-peek--create)
    (remove-hook 'minibuffer-exit-hook #'dirvish-peek--teardown)))

(provide 'dirvish-peek)
;;; dirvish-peek.el ends here

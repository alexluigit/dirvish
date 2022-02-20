;;; dirvish-peek.el --- Minibuffer file preview powered by dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 0.9.7
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (dirvish "0.9.9"))

;;; Commentary:

;; This package is a Dirvish extension, which provides minibuffer file preview
;; in a `dirvish' style.

;;; Code:

(declare-function selectrum--get-candidate "selectrum")
(declare-function selectrum--get-full "selectrum")
(declare-function selectrum--update "selectrum")
(declare-function vertico--candidate "vertico")
(declare-function vertico--exhibit "vertico")

(defvar dirvish--peek-ad-sym 'vertico--exhibit)
(defvar dirvish--peek-cand-fn #'vertico--candidate)
(defvar selectrum--current-candidate-index)

(require 'dirvish)
(require 'find-func)

(defcustom dirvish-peek-categories '(file project-file library)
  "Minibuffer metadata categories to show file preview."
  :group 'dirvish :type 'list)

(defun dirvish--peek-create ()
  "Create dirvish minibuffer preview window.
The window is created only when metadata in current minibuffer is
one of categories in `dirvish-peek-categories'."
  (let* ((old-dv (dirvish-curr))
         (meta (completion-metadata
                (buffer-substring-no-properties (field-beginning) (point))
                minibuffer-completion-table
                minibuffer-completion-predicate))
         (category (completion-metadata-get meta 'category))
         (preview-category (and (memq category dirvish-peek-categories) category))
         new-dv)
    (when (and preview-category
               (not (and old-dv (dv-preview-window old-dv))))
      (setq new-dv (dirvish-activate (dirvish-new :depth -1)))
      (push (selected-window) (dv-dired-windows new-dv))
      (let ((next-win (next-window)))
        (setf (dv-preview-window new-dv) next-win)))
    (set-frame-parameter nil 'dirvish--peek
                         `(:category ,preview-category :old ,old-dv :new ,new-dv))))

(defun dirvish--peek-teardown ()
  "Teardown dirvish minibuffer preview window."
  (let* ((dv-mini (frame-parameter nil 'dirvish--peek))
         (old-dv (plist-get dv-mini :old))
         (new-dv (plist-get dv-mini :new)))
    (when new-dv (dirvish-deactivate new-dv))
    (set-frame-parameter nil 'dirvish--curr old-dv)))

(defun dirvish--peek-update-advice (&rest _)
  "Apply FN with ARGS, then update dirvish minibuffer preview window.

Used as an advice for `vertico--exhibit' or `selectrum--update',
invoked when file name under cursor in minibuffer changed."
  (when-let* ((category (plist-get
                         (frame-parameter nil 'dirvish--peek) :category))
              (cand (funcall dirvish--peek-cand-fn)))
    (pcase category
      ('file
       (setq cand (expand-file-name cand)))
      ('project-file
       (setq cand (expand-file-name cand (or (cdr-safe (project-current))
                                             (car (minibuffer-history-value))))))
      ('library
       (setq cand (file-truename (or (ignore-errors (find-library-name cand)) "")))))
    (setf (dv-index-path (dirvish-curr)) cand)
    (dirvish-debounce dirvish-preview-update dirvish-debouncing-delay)))

(define-obsolete-function-alias 'dirvish-minibuf-preview-mode 'dirvish-peek-mode "0.9.9")

;;;###autoload
(define-minor-mode dirvish-peek-mode
  "Show dirvish preview when minibuffer candidates are files/dirs."
  :group 'dirvish :global t
  (if dirvish-peek-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'dirvish--peek-create)
        (add-hook 'minibuffer-exit-hook #'dirvish--peek-teardown)
        (advice-add dirvish--peek-ad-sym :after #'dirvish--peek-update-advice))
    (remove-hook 'minibuffer-setup-hook #'dirvish--peek-create)
    (remove-hook 'minibuffer-exit-hook #'dirvish--peek-teardown)
    (advice-remove dirvish--peek-ad-sym #'dirvish--peek-update-advice)))

(with-eval-after-load 'selectrum
  (let ((enabled dirvish-peek-mode))
    (and enabled (dirvish-peek-mode -1))
    (setq dirvish--peek-ad-sym 'selectrum--update)
    (setq dirvish--peek-cand-fn
          (lambda () (selectrum--get-full (selectrum--get-candidate
                                      selectrum--current-candidate-index))))
    (and enabled (dirvish-peek-mode +1))))

(provide 'dirvish-peek)
;;; dirvish-peek.el ends here

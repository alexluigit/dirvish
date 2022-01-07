;;; dirvish-minibuffer-preview.el --- Minibuffer file preview powered by dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 0.9.7
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (dirvish "0.9.7"))

;;; Commentary:

;; This package is a Dirvish extension, which provides minibuffer file preview in a `dirvish' style.

;;; Code:

(declare-function selectrum--get-candidate "selectrum")
(declare-function selectrum--get-full "selectrum")
(declare-function vertico--candidate "vertico")

(require 'dirvish)
(require 'find-func)

(defvar dirvish-minibuf-preview-categories '(file project-file library))
(defvar dirvish-minibuf-preview--category nil)
(defvar selectrum--current-candidate-index)

(defun dirvish-minibuf-preview-create ()
  "Create dirvish minibuffer preview window.
The window is created only when metadata in current minibuffer is
one of categories in `dirvish-minibuf-preview-categories'."
  (when-let* ((meta (completion-metadata
                     (buffer-substring-no-properties (field-beginning) (point))
                     minibuffer-completion-table
                     minibuffer-completion-predicate))
              (category (completion-metadata-get meta 'category))
              (show-preview (memq category dirvish-minibuf-preview-categories)))
    (setq dirvish-minibuf-preview--category category)
    (unless (and (dirvish-curr) (dv-preview-window (dirvish-curr)))
      (set-frame-parameter nil 'dirvish--minibuf (dirvish-activate t))
      (let ((next-win (next-window)))
        (setf (dv-preview-window (dirvish-curr)) next-win)
        (setf (dv-preview-pixel-width (dirvish-curr)) (window-width next-win t))))))

(defun dirvish-minibuf-preview-teardown ()
  "Teardown dirvish minibuffer preview window."
  (when-let (dv (frame-parameter nil 'dirvish--minibuf))
    (dirvish-deactivate dv)
    (with-selected-window (minibuffer-selected-window)
      (dirvish-reclaim)))
  (setq dirvish-minibuf-preview--category nil))

(defun dirvish--minibuf-update-advice (fn &rest args)
  "Apply FN with ARGS, then update dirvish minibuffer preview window.

Used as an advice for `vertico--exhibit' or `selectrum--update',
invoked when file name under cursor in minibuffer changed."
  (apply fn args)
  (when-let* ((category dirvish-minibuf-preview--category)
              (cand (cond ((bound-and-true-p vertico-mode)
                           (vertico--candidate))
                          ((bound-and-true-p selectrum-mode)
                           (selectrum--get-full
                            (selectrum--get-candidate
                             selectrum--current-candidate-index))))))
    (pcase category
      ('file
       (setq cand (expand-file-name cand)))
      ('project-file
       (setq cand (expand-file-name cand (or (cdr-safe (project-current))
                                             (car (minibuffer-history-value))))))
      ('library
       (setq cand (file-truename (or (ignore-errors (find-library-name cand)) "")))))
    (setf (dv-index-path (dirvish-curr)) cand)
    (dirvish-debounce dirvish-preview-update dirvish-preview-delay)))

;;;###autoload
(define-minor-mode dirvish-minibuf-preview-mode
  "Show dirvish preview when minibuffer candidates are files/dirs."
  :group 'dirvish :global t
  (if dirvish-minibuf-preview-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'dirvish-minibuf-preview-create)
        (add-hook 'minibuffer-exit-hook #'dirvish-minibuf-preview-teardown)
        (advice-add 'vertico--exhibit :around #'dirvish--minibuf-update-advice)
        (advice-add 'selectrum--update :around #'dirvish--minibuf-update-advice))
    (remove-hook 'minibuffer-setup-hook #'dirvish-minibuf-preview-create)
    (remove-hook 'minibuffer-exit-hook #'dirvish-minibuf-preview-teardown)
    (advice-remove 'vertico--exhibit #'dirvish--minibuf-update-advice)
    (advice-remove 'selectrum--update #'dirvish--minibuf-update-advice)))

(provide 'dirvish-minibuffer-preview)

;;; dirvish-minibuffer-preview.el ends here

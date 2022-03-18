;;; dirvish-peek.el --- Minibuffer file preview powered by Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.0.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (dirvish "1.0.0"))

;;; Commentary:

;; `dirvish-peek-mode' gives you file preview when narrowing candidates using minibuffer.

;;; Code:

(declare-function vertico--candidate "vertico")
(declare-function selectrum--get-candidate "selectrum")
(declare-function selectrum--get-full "selectrum")
(defvar selectrum--current-candidate-index)
(declare-function ivy-state-current "vertico")
(defvar ivy-last)
(require 'dirvish)
(require 'find-func)

(defvar dirvish-peek--cand-fn nil)
(defcustom dirvish-peek-backend
  (or (require 'vertico nil t) (require 'selectrum nil t) (require 'ivy nil t) 'icomplete)
  "Completion framework for `dirvish-peek-mode'.
These framework are supported: `vertico', `selectrum', `ivy', or
the inbuilt `icomplete\[-vertical-mode\]'."
  :group 'dirvish :type 'symbol
  :set
  (lambda (k v)
    (set k v)
    (setq dirvish-peek--cand-fn
          (pcase v
            ('vertico #'vertico--candidate)
            ('selectrum (lambda () (selectrum--get-full (selectrum--get-candidate selectrum--current-candidate-index))))
            ('ivy (lambda () (ivy-state-current ivy-last)))
            ('icomplete (lambda () (car completion-all-sorted-completions)))))))

(defcustom dirvish-peek-categories '(file project-file library)
  "Minibuffer metadata categories to show file preview."
  :group 'dirvish :type 'list)

(defcustom dirvish-peek-display-alist
  '((side . right)
    (slot . -1)
    (window-width . 0.5))
  "Display alist for preview window of `dirvish-peek'."
  :group 'dirvish :type 'alist)

(defun dirvish-peek--create ()
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
    (when preview-category
      (add-hook 'post-command-hook #'dirvish-peek-update-h 99 t)
      (unless (and old-dv (dv-preview-window old-dv))
        (setq new-dv (dirvish-activate (dirvish-new :depth -1)))
        (push (selected-window) (dv-dired-windows new-dv))
        (setf (dv-preview-window new-dv)
              (display-buffer-in-side-window (dirvish--ensure-temp-buffer) dirvish-peek-display-alist))))
    (set-frame-parameter nil 'dirvish--peek `(:category ,preview-category :old ,old-dv :new ,new-dv))))

(defun dirvish-peek--teardown ()
  "Teardown dirvish minibuffer preview window."
  (let* ((dv-mini (frame-parameter nil 'dirvish--peek))
         (old-dv (plist-get dv-mini :old))
         (new-dv (plist-get dv-mini :new)))
    (when new-dv (dirvish-deactivate new-dv))
    (set-frame-parameter nil 'dirvish--curr old-dv)))

(defun dirvish-peek-update-h ()
  "Hook for `post-command-hook' to update peek window."
  (when-let* ((category
               (plist-get (frame-parameter nil 'dirvish--peek) :category))
              (cand (funcall dirvish-peek--cand-fn)))
    (pcase category
      ('file
       (setq cand (expand-file-name cand)))
      ('project-file
       (setq cand (expand-file-name cand (or (dirvish--get-project-root)
                                             (car (minibuffer-history-value))))))
      ('library
       (setq cand (file-truename (or (ignore-errors (find-library-name cand)) "")))))
    (setf (dv-index-path (dirvish-curr)) cand)
    (dirvish-debounce layout (dirvish-preview-update))))

(define-obsolete-function-alias 'dirvish-minibuf-preview-mode 'dirvish-peek-mode "0.9.9")

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

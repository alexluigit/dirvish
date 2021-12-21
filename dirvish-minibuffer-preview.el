;;; dirvish-minibuffer-preview.el --- Minibuffer file preview powered by dirvish -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package is a Dirvish extension, which provides minibuffer file preview in a `dirvish' style.

;;; Code:

(declare-function selectrum--get-candidate "selectrum")
(declare-function selectrum--get-full "selectrum")
(declare-function vertico--candidate "vertico")

(require 'dirvish)
(require 'find-func)

(defcustom dirvish-minibuf-preview-position
  (lambda (info)
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width)) 2)
          (or (* (frame-parameter nil 'internal-border-width) 2) 60)))
  "A function determines position of dirvish minibuffer preview window.

Used as `:poshandler' for `posframe-show'."
  :group 'dirvish :type 'function)

(defvar dirvish-minibuf-preview-categories '(file project-file library))
(defvar dirvish-minibuf-preview--height (- 1 (* max-mini-window-height 1.5)))
(defvar dirvish-minibuf-preview--width nil)
(defvar dirvish-minibuf-preview-window nil)
(defvar dirvish-minibuf-preview--category nil)
(defvar selectrum--current-candidate-index)
(defvar dirvish-preview-update-timer)

(defun dirvish-minibuf-preview-create ()
  "Create dirvish minibuffer preview window using `posframe'."
  (when-let* ((meta (completion-metadata
                     (buffer-substring-no-properties (field-beginning) (point))
                     minibuffer-completion-table
                     minibuffer-completion-predicate))
              (category (completion-metadata-get meta 'category))
              (show-preview (memq category dirvish-minibuf-preview-categories)))
    (setq dirvish-minibuf-preview--category category)
    (walk-window-tree (lambda (w)
                        (when (window-live-p w)
                          (with-current-buffer (window-buffer w)
                            (let ((ov (make-overlay (point-min) (point-max))))
                              (overlay-put ov 'temp-inactive-ov t)
                              (overlay-put ov 'font-lock-face 'font-lock-doc-face))))))
    (or (frame-parameter nil 'dirvish-meta)
        (set-frame-parameter nil 'dirvish-meta (make--dirvish)))
    (setq dirvish-minibuf-preview-window (dirvish-preview-window (dirvish-meta)))
    (unless (window-live-p dirvish-minibuf-preview-window)
      (let* ((min-w (ceiling (* (frame-width) dirvish-preview-width)))
             (min-h (ceiling (* (frame-height) dirvish-minibuf-preview--height)))
             (b-color (face-attribute 'font-lock-doc-face :foreground))
             (pos-f (or dirvish-minibuf-preview-position #'posframe-poshandler-frame-top-center))
             (override `((minibuffer . ,(active-minibuffer-window))))
             (f-props `(:min-width ,min-w :min-height ,min-h :poshandler ,pos-f
                                   :override-parameters ,override
                                   :border-width 5 :border-color ,b-color))
             (frame (apply #'posframe-show "*candidate preview*" f-props)))
        (setq dirvish-minibuf-preview-window (frame-root-window frame))
        (set-window-fringes dirvish-minibuf-preview-window 30 30 nil t)))
    (setq dirvish-minibuf-preview--width (window-width dirvish-minibuf-preview-window t))
    (set-window-dedicated-p dirvish-minibuf-preview-window nil)))

(defun dirvish-minibuf-preview-teardown ()
  "Teardown dirvish minibuffer preview window."
  (posframe-delete "*candidate preview*")
  (walk-window-tree (lambda (w)
                      (with-current-buffer (window-buffer w)
                        (remove-overlays (point-min) (point-max) 'temp-inactive-ov t))))
  (setq dirvish-minibuf-preview--category nil)
  (setq dirvish-minibuf-preview--width nil)
  (mapc #'kill-buffer dirvish-preview-buffers))

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
       (setq cand (find-library-name cand))))
    (setf (dirvish-index-path (dirvish-meta)) cand)
    (dirvish-debounce dirvish-preview-update
                      dirvish-preview-delay
                      dirvish-minibuf-preview-window)))

;;;###autoload
(define-minor-mode dirvish-minibuf-preview-mode
  "Show dirvish preview when minibuffer candidates are files/dirs."
  :group 'dirvish :global t
  (if dirvish-minibuf-preview-mode
      (when window-system
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

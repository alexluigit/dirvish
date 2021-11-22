;;; danger-minibuffer.el --- minibuffer preview window -*- lexical-binding: t -*-

;; Author: Alex Lu <alexluigit@gmail.com>
;; Maintainer: Alex Lu <alexluigit@gmail.com>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (danger "0.7.0"))
;; Homepage: https://github.com/alexluigit/danger

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a Danger extension, which provides minibuffer file preview in a `danger' style.

;;; Code:

(declare-function selectrum--get-candidate "selectrum")
(declare-function selectrum--get-full "selectrum")
(declare-function vertico--candidate "vertico")
(defvar danger-update--preview-timer)

(require 'danger)

(defcustom danger-minibuf-preview-position
  (lambda (info)
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width)) 2)
          (or (* (frame-parameter nil 'internal-border-width) 2) 60)))
  "doc"
  :group 'danger :type 'function)

(defvar danger-minibuf-preview-categories '(file project-file)
  "doc")

(defvar danger-minibuf-preview--height (- 1 (* max-mini-window-height 1.5))
  "doc")

(defvar danger-minibuf-preview--width nil
  "doc")

(defvar danger-minibuf-preview-window nil
  "doc")

(defvar danger-minibuf-preview--category nil
  "doc")

(defvar selectrum--current-candidate-index)

(defun danger-minibuf-preview-create ()
  "doc"
  (when-let* ((meta (completion-metadata
                     (buffer-substring-no-properties (field-beginning) (point))
                     minibuffer-completion-table
                     minibuffer-completion-predicate))
              (category (completion-metadata-get meta 'category))
              (show-preview (memq category danger-minibuf-preview-categories)))
    (setq danger-minibuf-preview--category category)
    (walk-window-tree (lambda (w)
                        (when (window-live-p w)
                          (with-current-buffer (window-buffer w)
                            (let ((ov (make-overlay (point-min) (point-max))))
                              (overlay-put ov 'temp-inactive-ov t)
                              (overlay-put ov 'font-lock-face 'font-lock-doc-face))))))
    (setq danger-minibuf-preview-window (frame-parameter nil 'danger-preview-window))
    (unless danger-minibuf-preview-window
      (let* ((min-w (ceiling (* (frame-width) danger-width-preview)))
             (min-h (ceiling (* (frame-height) danger-minibuf-preview--height)))
             (b-color (face-attribute 'font-lock-doc-face :foreground))
             (pos-f (or danger-minibuf-preview-position #'posframe-poshandler-frame-top-center))
             (override `((minibuffer . ,(active-minibuffer-window))))
             (f-props `(:min-width ,min-w :min-height ,min-h :poshandler ,pos-f
                                   :override-parameters ,override
                                   :border-width 5 :border-color ,b-color))
             (frame (apply #'posframe-show "*candidate preview*" f-props)))
        (setq danger-minibuf-preview-window (frame-root-window frame))
        (set-window-fringes danger-minibuf-preview-window 30 30 nil t)))
    (danger-init--buffer)
    (setq danger-minibuf-preview--width (window-width danger-minibuf-preview-window t))
    (set-window-dedicated-p danger-minibuf-preview-window nil)))

(defun danger-minibuf-preview-teardown ()
  "doc"
  (posframe-delete "*candidate preview*")
  (walk-window-tree (lambda (w)
                      (with-current-buffer (window-buffer w)
                        (remove-overlays (point-min) (point-max) 'temp-inactive-ov t))))
  (setq danger-minibuf-preview--category nil)
  (setq danger-minibuf-preview--width nil)
  (mapc 'kill-buffer danger-preview-buffers))

(defun danger-minibuf--update-advice (fn &rest args)
  "doc"
  (apply fn args)
  (when-let* ((category danger-minibuf-preview--category)
              (cand (cond ((bound-and-true-p vertico-mode)
                           (vertico--candidate))
                          ((bound-and-true-p selectrum-mode)
                           (selectrum--get-full
                            (selectrum--get-candidate selectrum--current-candidate-index))))))
    (if (eq category 'project-file)
        (setq cand (expand-file-name cand (or (cdr-safe (project-current))
                                              (car (minibuffer-history-value)))))
      (setq cand (expand-file-name cand)))
    (set-frame-parameter nil 'danger-index-path cand)
    (danger-debounce danger-update--preview danger-preview-delay danger-minibuf-preview-window)))

;;;###autoload
(define-minor-mode danger-minibuf-preview-mode
  "Show danger preview when minibuf."
  :group 'danger :global t
  (if danger-minibuf-preview-mode
      (when window-system
        (add-hook 'minibuffer-setup-hook #'danger-minibuf-preview-create)
        (add-hook 'minibuffer-exit-hook #'danger-minibuf-preview-teardown)
        (advice-add 'vertico--exhibit :around #'danger-minibuf--update-advice)
        (advice-add 'selectrum--update :around #'danger-minibuf--update-advice))
    (remove-hook 'minibuffer-setup-hook #'danger-minibuf-preview-create)
    (remove-hook 'minibuffer-exit-hook #'danger-minibuf-preview-teardown)
    (advice-remove 'vertico--exhibit #'danger-minibuf--update-advice)
    (advice-remove 'selectrum--update #'danger-minibuf--update-advice)))

(provide 'danger-minibuffer)

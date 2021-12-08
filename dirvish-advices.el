;;; dirvish-advices.el --- Shims for other packages. -*- lexical-binding: t -*-

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

;;; Shims for other packages.

;;; Code:

(declare-function dirvish-quit "dirvish-init")
(declare-function dirvish-refresh "dirvish-init")
(declare-function dirvish-new-frame "dirvish-commands")
(declare-function dirvish-next-file "dirvish-commands")
(require 'cl-lib)
(require 'dirvish-helpers)
(require 'dirvish-header)
(require 'dirvish-body)
(require 'dirvish-vars)

(defun dirvish-redisplay--frame ()
  "Refresh dirvish frame, added to `after-focus-change-functions'."
  (if (eq major-mode 'dirvish-mode)
      (dirvish-refresh t)
    (when (memq (previous-frame) (mapcar 'car dirvish-frame-alist))
      (with-selected-frame (previous-frame)
        (dirvish-header-build)
        (dirvish-header-update)))))

(defun dirvish-setup-dired-buffer--advice (fn &rest args)
  "Remove the header line in dired buffer."
  (apply fn args)
  (save-excursion
    (let ((o (make-overlay (point-min) (progn (forward-line 1) (point)))))
      (overlay-put o 'invisible t))))

(defun dirvish-refresh--advice (fn &rest args)
  "Apply FN with ARGS, rebuild dirvish frame when necessary."
  (apply fn args)
  (let ((rebuild (not (eq major-mode 'dirvish-mode))))
    (dirvish-refresh rebuild nil 'no-revert)))

(defun dirvish-revert--advice (fn &rest args)
  "Apply FN with ARGS then revert buffer."
  (apply fn args) (dirvish-refresh))

(defun dirvish-refresh-cursor--advice (fn &rest args)
  "Only apply FN with ARGS when editing."
  (unless (and (not (eq major-mode 'wdired-mode)) (dirvish-live-p))
    (apply fn args)))

(defun dirvish-update-line--advice (fn &rest args)
  "Apply FN with ARGS then update current line in dirvish."
  (remove-overlays (point-min) (point-max) 'dirvish-body t)
  (when-let ((pos (dired-move-to-filename nil))
             dirvish-show-icons)
    (remove-overlays (1- pos) pos 'dirvish-icons t)
    (dirvish-body--render-icon pos))
  (apply fn args)
  (dirvish-body-update t t))

(defun dirvish-deletion--advice (fn &rest args)
  "Advice function for FN with ARGS."
  (let ((trash-directory (dirvish-get--trash-dir))) (apply fn args))
  (unless (dired-get-filename nil t) (dirvish-next-file 1))
  (dirvish-refresh))

(defun dirvish-file-open--advice (fn &rest args)
  "Quit dirvish when open a file."
  (when (dirvish-live-p) (dirvish-quit :keep-alive))
  (let ((default-directory "")) (apply fn args)))

;; FIXME: it should support window when current instance is launched by `(dirvish nil t)'
(defun dirvish-other-window--advice (fn &rest args)
  "Open current file/dir in new frame."
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (dirvish-new-frame file)
      (apply fn args))))

(defun dirvish-update--viewports (win _)
  "Refresh attributes in viewport, added to `window-scroll-functions'."
  (when (and (eq win dirvish-window)
             (eq (selected-frame) (window-frame dirvish-window)))
    (with-selected-window win
      (dirvish-body-update nil t))))

(cl-dolist (fn '(dirvish-next-file
                 dirvish-go-top
                 dirvish-go-bottom
                 dirvish-flag-file-yank))
  (advice-add fn :around 'dirvish-update-line--advice))

(provide 'dirvish-advices)

;;; dirvish-advices.el ends here

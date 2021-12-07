;;; dirvish-init.el --- init/deinit functions for Dirvish. -*- lexical-binding: t -*-

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

;;; Init/deinit functions for dirvish.

;;; Code:

(require 'dirvish-vars)
(require 'dirvish-parents)
(require 'dirvish-preview)
(require 'dirvish-footer)
(require 'dirvish-advices)

(defvar recentf-list)

(defun dirvish-init (&optional one-window)
  "Save previous window config and initialize dirvish."
  (unless (or (posframe-workable-p) one-window)
    (user-error "dirvish.el: requires GUI."))
  (when (eq major-mode 'dirvish-mode) (dirvish-quit))
  (set-frame-parameter nil 'dirvish-one-window one-window)
  (when-let* ((ignore-one-win (not one-window))
              (frame (window-frame))
              (new-dirvish-frame (not (assoc frame dirvish-frame-alist))))
    (push (cons frame (current-window-configuration)) dirvish-frame-alist))
  (when (window-parameter nil 'window-side) (delete-window))
  (dirvish-init--buffer)
  (unless dirvish-initialized
    (dirvish-add--advices)
    (when dirvish-show-icons (setq dirvish-show-icons (ignore-errors (require 'all-the-icons))))
    (when (dirvish-get--i/o-status)
      (dirvish-repeat 'dirvish-footer-update 0 0.1)
      (dirvish-repeat dirvish-set--i/o-status 0 0.1))
    (when (featurep 'recentf) (setq dirvish-orig-recentf-list recentf-list))
    (mailcap-parse-mimetypes)
    (setq dirvish-initialized t)))

(defun dirvish-deinit ()
  "Revert previous window config and deinit dirvish."
  (setq dirvish-initialized nil)
  (setq recentf-list dirvish-orig-recentf-list)
  (mapc #'kill-buffer dirvish-preview-buffers)
  (let ((one-window (frame-parameter nil 'dirvish-one-window))
        (config (cdr-safe (assoc (window-frame) dirvish-frame-alist))))
    (if one-window
        (while (eq 'dirvish-mode (buffer-local-value 'major-mode (current-buffer)))
          (delq (selected-window) dirvish-parent-windows)
          (quit-window))
      (posframe-delete (frame-parameter nil 'dirvish-header-buffer))
      (set-frame-parameter nil 'dirvish-header--frame nil)
      (set-frame-parameter nil 'dirvish-preview-window nil)
      (setq dirvish-frame-alist (delq (assoc (window-frame) dirvish-frame-alist) dirvish-frame-alist))
      (when (window-configuration-p config)
        (set-window-configuration config)))
    (unless
        (or (and one-window (> (length dirvish-parent-windows) 1))
            (> (length dirvish-frame-alist) 1))
      (dirvish-clean--buffers)
      (dirvish-clean--advices)
      (dolist (tm dirvish-repeat-timers) (cancel-timer (symbol-value tm))))
    (unless one-window (set-frame-parameter nil 'dirvish-one-window t))
    (setq dirvish-window nil)
    (setq dirvish-parent-windows ())
    (setq dirvish-preview-buffers ())
    (setq dirvish-parent-buffers ())))

(defun dirvish-init--buffer ()
  (let* ((index (number-to-string (length dirvish-frame-alist)))
         (header-buf (get-buffer-create (concat " *Dirvish Header-" index "*")))
         (preview-buf (get-buffer-create (concat " *Dirvish Preview-" index "*"))))
    (with-current-buffer preview-buf (setq mode-line-format nil))
    (with-current-buffer header-buf (setq-local face-font-rescale-alist nil))
    (set-frame-parameter nil 'dirvish-preview-buffer preview-buf)
    (set-frame-parameter nil 'dirvish-header-buffer header-buf)))

(defun dirvish-clean--buffers ()
  (cl-dolist (buf (buffer-list))
    (let ((name (buffer-name buf))
          (mode (buffer-local-value 'major-mode buf)))
      (when (or (eq 'dired-mode mode) (eq 'dirvish-mode mode)
                (and (not (string-equal name ""))
                     (string-match " \\*Dirvish .*" name)
                     (not (get-buffer-process buf))))
        (kill-buffer buf)))))

(defun dirvish-add--advices ()
  "Add all advice listed in `dirvish-advice-alist'."
  (add-hook 'window-scroll-functions #'dirvish-update--viewports)
  (add-to-list 'display-buffer-alist
               '("\\(\\*info\\|\\*Help\\|\\*helpful\\|magit:\\).*"
                 (display-buffer-in-side-window)
                 (window-height . 0.4)
                 (side . bottom)))
  (add-function :after after-focus-change-function #'dirvish-redisplay--frame)
  (pcase-dolist (`(,file ,sym ,fn) dirvish-advice-alist)
    (with-eval-after-load file (advice-add sym :around fn))))

(defun dirvish-clean--advices ()
  "Remove all advice listed in `dirvish-advice-alist'."
  (remove-hook 'window-scroll-functions #'dirvish-update--viewports)
  (setq display-buffer-alist (cdr display-buffer-alist))
  (remove-function after-focus-change-function #'dirvish-redisplay--frame)
  (pcase-dolist (`(,file ,sym ,fn) dirvish-advice-alist)
    (with-eval-after-load file (advice-remove sym fn))))

(defun dirvish-quit (&optional keep-alive)
  "Revert dirvish settings and disable dirvish."
  (interactive)
  (dirvish-deinit)
  (when (and (not keep-alive)
             (string= (frame-parameter nil 'name) "dirvish-emacs"))
    (delete-frame)))

(defun dirvish-refresh (&optional rebuild filter no-revert)
  "Reset dirvish. With optional prefix ARG (\\[universal-argument])
also rebuild dirvish layout."
  (interactive "P")
  (when rebuild
    (dirvish-parent-build)
    (dirvish-preview-build)
    (dirvish-header-build))
  (unless no-revert (revert-buffer))
  (when filter (dirvish-update--filter))
  (dirvish-body-update)
  (dirvish-preview-update)
  (dirvish-header-update)
  (dirvish-footer-update))

(defun dirvish-find-file (&optional file ignore-hist)
  "Find file in dirvish buffer.

FILE can be a file or a directory, if nil then infer entry from
current `buffer-file-name'. If IGNORE-HIST is non-nil, do not
update `dirvish-history-ring'."
  (interactive)
  (let ((entry (or file (dired-get-filename nil t)))
        (bname (buffer-file-name (current-buffer)))
        (curr-dir (expand-file-name default-directory)))
    (when entry
      (if (file-directory-p entry)
          (let ((hist (directory-file-name entry)))
            (unless ignore-hist
              (when (or (ring-empty-p dirvish-history-ring)
                        (not (eq hist (ring-ref dirvish-history-ring 0))))
                (ring-insert dirvish-history-ring hist)))
            (switch-to-buffer (or (car (dired-buffers-for-dir entry))
                                  (dired-noselect entry)))
            (setq dirvish-child-entry (or bname curr-dir))
            (set-frame-parameter nil 'dirvish-index-path
                                 (or (dired-get-filename nil t) entry))
            (dirvish-refresh t))
        (find-file entry)))))

;;;###autoload
(defun dirvish (&optional path one-window)
  "Launch dired in dirvish-mode."
  (interactive)
  (let* ((file (or path buffer-file-name))
         (dir (if file (expand-file-name (file-name-directory file))
                (expand-file-name default-directory))))
    (dirvish-init one-window)
    (dirvish-find-file dir)))

(put 'dired-subdir-alist 'permanent-local t)

(provide 'dirvish-init)

;;; dirvish-init.el ends here

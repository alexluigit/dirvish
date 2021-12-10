;;; dirvish.el --- a modern file manager based on dired mode. -*- lexical-binding: t -*-
;; Copyright (C) 2021 Alex Lu

;; Author : Alex Lu <https://github.com/alexluigit>
;; Package-Version: 0.7.0
;; Keywords: dirvish, ranger, file, dired
;; Homepage: https://github.com/alexluigit/dirvish.el
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "26") (posframe "1.1.2"))

;;; Commentary:

;; `dirvish.el' is a minimalistic file manager based on `dired-mode'.  It is inspired by ranger (see
;; https://github.com/ranger/ranger), which is a terminal file manager that shows a stack of the
;; parent directories, and updates its parent buffers while navigating the file system with an
;; optional preview window at side showing the content of the selected file.

;; Unlike `ranger.el', which tries to become an all-around emulation of ranger, dirvish.el is more
;; bare-bone, meaning it does NOT try to port all "goodness" from ranger, instead, it tries to:
;;
;;   - provides a better Dired UI
;;   - make some Dired commands more intuitive
;;   - keep all your Dired key bindings
;;
;; The name `dirvish' is a tribute to `vim-dirvish'.

;;; Code:

;;;; Deps

(require 'ring)

;;;; Modules

(require 'dirvish-vars)
(require 'dirvish-helpers)
(require 'dirvish-header)
(require 'dirvish-footer)
(require 'dirvish-parents)
(require 'dirvish-preview)
(require 'dirvish-advices)

;;;; Commands

(defun dirvish-other-buffer ()
  "Replacement for `mode-line-other-buffer' in dirvish-mode."
  (interactive)
  (let ((one-window (frame-parameter nil 'dirvish-one-window)))
    (if one-window
        (switch-to-buffer (other-buffer) nil t)
      (dirvish-find-file (ring-ref dirvish-history-ring 1)))))

(defun dirvish-jump (file)
  "Replacement for `dired-jump'"
  (interactive (list (read-file-name "Jump to: "))) (dirvish-find-file-dwim file))

(defun dirvish-up-directory ()
  "Move to parent directory."
  (interactive)
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish-get--parent current)))
    (if (string= parent current)
        (when (eq system-type 'windows-nt)
          (let* ((output (shell-command-to-string "wmic logicaldisk get name"))
                 (drives (cdr (split-string output)))
                 (drive (completing-read "Select drive: " drives)))
            (when drive (dirvish-find-file drive))))
      (dirvish-find-file parent t))))

(defun dirvish-go-top ()
  "Move to top of file list"
  (interactive)
  (goto-char (point-min)) (dired-next-line 1)
  (dirvish-next-file -1))

(defun dirvish-go-bottom ()
  "Move to bottom of file list"
  (interactive)
  (goto-char (point-max)) (dirvish-next-file 1))

(defun dirvish-next-file (arg)
  "Move lines in dirvish and initiate updates to preview window."
  (interactive "^p")
  (dired-next-line arg)
  (cond
   ((eobp) (unless (region-active-p) (forward-line -1)))
   ((bobp) (dired-next-line 1)))
  (when (dired-move-to-filename nil)
    (set-frame-parameter nil 'dirvish-index-path (dired-get-filename nil t))
    (dirvish-header-update)
    (dirvish-footer-update)
    (dirvish-debounce dirvish-preview-update dirvish-preview-delay)))

(defun dirvish-prev-file (arg)
  (interactive "^p")
  (dirvish-next-file (- 0 arg)))

(defun dirvish-show-history (history)
  "Show history prompt for recent directories"
  (interactive
   (list (completing-read "Select from history: "
                          (cl-remove-duplicates (ring-elements dirvish-history-ring)
                                                :test (lambda (x y) (or (null y) (equal x y)))))))
  (when history (dirvish-find-file history)))

(defun dirvish-new-frame (&optional path)
  "Make a new frame and launch dirvish."
  (interactive (list (read-file-name "Open in new frame: ")))
  (when (with-selected-window (selected-window) (eq major-mode 'dirvish-mode))
    (dirvish-quit))
  (let* ((after-make-frame-functions (lambda (f) (select-frame f)))
         (frame (make-frame '((name . "dirvish-emacs") (alpha . (100 50))))))
    (with-selected-frame frame (dirvish path))))

(defun dirvish-paste (&optional mode)
  "doc"
  (interactive)
  (let* ((regexp (dired-marker-regexp))
         (yanked-files ())
         (mode (or mode 'copy))
         case-fold-search)
    (cl-dolist (buf (seq-filter 'buffer-live-p dirvish-parent-buffers))
      (with-current-buffer buf
        (when (save-excursion (goto-char (point-min))
                              (re-search-forward regexp nil t))
          (setq yanked-files
                (append yanked-files (dired-map-over-marks (dired-get-filename) nil))))))
    (unless yanked-files (error "No files marked for paste."))
    (dirvish-internal-paste yanked-files mode)))

(defun dirvish-yank (&optional arg)
  (interactive "P")
  (if arg (dirvish-paste 'move) (dirvish-paste)))

(defun dirvish-change-level (&optional arg)
  (interactive "p")
  (setq dirvish-depth (or arg 1)) (dirvish-refresh t))

(defun dirvish-toggle-dotfiles ()
  "Show/hide dot-files."
  (interactive)
  (setq dirvish-show-hidden
        (cl-case dirvish-show-hidden
          ('all 'dot) ('dot 'dirvish) ('dirvish 'all)))
  (dirvish-refresh nil t))

(defun dirvish-toggle-preview ()
  "Show/hide preview window."
  (interactive)
  (setq dirvish-enable-preview (not dirvish-enable-preview))
  (dirvish-refresh t)
  (when dirvish-enable-preview
    (dired-hide-details-mode t)))

(defun dirvish-sort-by-criteria (criteria)
  "Call sort-dired by different `CRITERIA'."
  (interactive
   (list
    (read-char-choice
     "criteria: (n/N)ame (e/E)xt (s/S)ize (t/T)ime (c/C)time "
     '(?q ?n ?N ?e ?E ?s ?S ?t ?T ?c ?C))))
  (unless (eq criteria ?q)
    (let* ((c (char-to-string criteria))
           (revp (string-equal c (upcase c)))
           (cc (downcase c))
           (sort-flag
            (cond
             ((string-equal cc "n") '("name" . ""))
             ((string-equal cc "c") '("modified" . " -c"))
             ((string-equal cc "e") '("ext" . " -X"))
             ((string-equal cc "t") '("time" . " -t"))
             ((string-equal cc "s") '("size" . " -S"))))
           (switch (concat dired-listing-switches (cdr sort-flag) (when revp " -r"))))
      (setq dirvish-sort-criteria (car sort-flag))
      (dired-sort-other switch)
      (dirvish-refresh))))

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
(defun dirvish-find-file-dwim (&rest args)
  "Call `dirvish-find-file' or `dired-find-file'."
  (if (derived-mode-p 'dirvish-mode)
      (apply 'dirvish-find-file args)
    (apply 'find-alternate-file args)))

;;;###autoload
(define-minor-mode dirvish-override-dired-jump
  "Override `dired-jump' with `dirvish-jump'."
  :group 'dirvish :global t
  (if dirvish-override-dired-jump
      (advice-add 'dired-jump :around #'dirvish-override-dired)
    (advice-remove 'dired-jump #'dirvish-override-dired)))

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

(provide 'dirvish)

;;; dirvish.el ends here

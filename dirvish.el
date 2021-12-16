;;; dirvish.el --- A modern file manager based on dired mode -*- lexical-binding: t -*-
;; Copyright (C) 2021 Alex Lu

;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 0.7.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (posframe "1.1.2"))

;;; Commentary:

;; `dirvish.el' is a minimalistic file manager based on `dired-mode'.  It is
;; inspired by ranger (see https://github.com/ranger/ranger), which is a
;; terminal file manager that shows a stack of the parent directories, and
;; updates its parent buffers while navigating the file system with an optional
;; preview window at side showing the content of the selected file.

;; Unlike `ranger.el', which tries to become an all-around emulation of ranger,
;; dirvish.el is more bare-bone, meaning it does NOT try to port all "goodness"
;; from ranger, instead, it tries to:
;;
;;   - provides a better Dired UI
;;   - make some Dired commands more intuitive
;;   - keep all your Dired key bindings
;;
;; The name `dirvish' is a tribute to `vim-dirvish'.

;;; Code:

;;;; Deps

(require 'ring)
(require 'recentf)

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
  "Replacement for `mode-line-other-buffer' in `dirvish-mode'."
  (interactive)
  (let ((one-window (frame-parameter nil 'dirvish-one-window)))
    (if one-window
        (switch-to-buffer (other-buffer) nil t)
      (dirvish-find-file (ring-ref dirvish-history-ring 1)))))

(defun dirvish-jump (file)
  "Replacement for `dired-jump' to FILE."
  (interactive (list (read-file-name "Jump to: "))) (dirvish-find-file-dwim file))

(defun dirvish-up-directory ()
  "Move to parent directory."
  (interactive)
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish--get-parent current)))
    (if (string= parent current)
        (when (eq system-type 'windows-nt)
          (let* ((output (shell-command-to-string "wmic logicaldisk get name"))
                 (drives (cdr (split-string output)))
                 (drive (completing-read "Select drive: " drives)))
            (when drive (dirvish-find-file drive))))
      (dirvish-find-file parent t))))

(defun dirvish-go-top ()
  "Move to top of file list."
  (interactive)
  (goto-char (point-min)) (dired-next-line 1)
  (dirvish-next-file -1))

(defun dirvish-go-bottom ()
  "Move to bottom of file list."
  (interactive)
  (goto-char (point-max)) (dirvish-next-file 1))

(defun dirvish-next-file (arg)
  "Move cursor to next line in dirvish and update to preview window.

With optional prefix ARG (\\[universal-argument]), forward ARG
lines."
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
  "Same as `dirvish-next-file', only in opposite direction.

With optional prefix ARG (\\[universal-argument]), backward ARG
lines."
  (interactive "^p")
  (dirvish-next-file (- 0 arg)))

(defun dirvish-show-history (history)
  "Prompt for a target directory from HISTORY and goto it."
  (interactive
   (list (completing-read "Select from history: "
                          (cl-remove-duplicates (ring-elements dirvish-history-ring)
                                                :test (lambda (x y) (or (null y) (equal x y)))))))
  (when history (dirvish-find-file history)))

(defun dirvish-new-frame (&optional path)
  "Make a new frame and launch dirvish with optional PATH."
  (interactive (list (read-file-name "Open in new frame: ")))
  (when (with-selected-window (selected-window) (eq major-mode 'dirvish-mode))
    (dirvish-quit))
  (let* ((after-make-frame-functions (lambda (f) (select-frame f)))
         (frame (make-frame '((name . "dirvish-emacs") (alpha . (100 50))))))
    (with-selected-frame frame (dirvish path))))

(defun dirvish-paste (&optional mode)
  "Paste marked files/directory to current directory according to MODE.

MODE can be `'copy', `'move', `symlink', or `relalink'."
  (interactive)
  (let* ((regexp (dired-marker-regexp))
         (yanked-files ())
         (mode (or mode 'copy))
         case-fold-search)
    (cl-dolist (buf (seq-filter #'buffer-live-p dirvish-parent-buffers))
      (with-current-buffer buf
        (when (save-excursion (goto-char (point-min))
                              (re-search-forward regexp nil t))
          (setq yanked-files
                (append yanked-files (dired-map-over-marks (dired-get-filename) nil))))))
    (unless yanked-files (user-error "No files marked for pasting"))
    (dirvish--paste yanked-files mode)))

(defun dirvish-yank (&optional arg)
  "Paste marked files/directory to current directory.

With optional prefix ARG, delete source files/directories."
  (interactive "P")
  (if arg (dirvish-paste 'move) (dirvish-paste)))

(defun dirvish-change-level (&optional arg)
  "Change `dirvish-depth' to ARG."
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

(cl-defstruct (dirvish
               (:conc-name dirvish-)
               (:constructor make--dirvish))
  "Return a dirvish struct.

It has following fields:

WINDOW-CONF is the window configuration given by
`current-window-configuration' first time the dirvish was
created for current frame (only for full-frame dirvish)."
  window-conf)

(defun dirvish-init (&optional one-window)
  "Save previous window config and initialize dirvish.

If ONE-WINDOW is not-nil, initialize dirvish only in current
window, not the whole frame."
  (unless (or (posframe-workable-p) one-window)
    (user-error "Dirvish.el: posframe unable to initialize under current Emacs instance"))
  (when (eq major-mode 'dirvish-mode) (dirvish-quit))
  (set-frame-parameter nil 'dirvish-one-window one-window)
  (set-frame-parameter nil 'dirvish-meta (make--dirvish))
  (unless one-window
    (setf (dirvish-window-conf (dirvish-meta)) (current-window-configuration))
    (add-to-list 'dirvish-frame-list (window-frame)))
  (when (window-parameter nil 'window-side) (delete-window))
  (dirvish--init-buffer)
  (unless dirvish-initialized
    (dirvish--add-advices)
    (when dirvish-show-icons (setq dirvish-show-icons (ignore-errors (require 'all-the-icons))))
    (when (dirvish--get-IO-status)
      (dirvish-repeat 'dirvish-footer-update 0 0.1)
      (dirvish-repeat dirvish--set-IO-status 0 0.1))
    (when (featurep 'recentf) (setq dirvish-orig-recentf-list recentf-list))
    (mailcap-parse-mimetypes)
    (setq dirvish-initialized t)))

(defun dirvish-deinit ()
  "Revert previous window config and deinit dirvish."
  (setq dirvish-initialized nil)
  (setq recentf-list dirvish-orig-recentf-list)
  (mapc #'kill-buffer dirvish-preview-buffers)
  (let ((one-window (frame-parameter nil 'dirvish-one-window))
        (config (dirvish-window-conf (dirvish-meta))))
    (if one-window
        (while (eq 'dirvish-mode (buffer-local-value 'major-mode (current-buffer)))
          (delq (selected-window) dirvish-parent-windows)
          (quit-window))
      (posframe-delete (frame-parameter nil 'dirvish-header-buffer))
      (set-frame-parameter nil 'dirvish--header-frame nil)
      (set-frame-parameter nil 'dirvish-preview-window nil)
      (setq dirvish-frame-list (delq (window-frame) dirvish-frame-list))
      (when (window-configuration-p config)
        (set-window-configuration config)))
    (unless
        (or (and one-window (> (length dirvish-parent-windows) 1))
            (> (length dirvish-frame-list) 1))
      (dirvish--clean-buffers)
      (dirvish--clean-advices)
      (dolist (tm dirvish-repeat-timers) (cancel-timer (symbol-value tm))))
    (unless one-window (set-frame-parameter nil 'dirvish-one-window t))
    (setq dirvish-window nil)
    (setq dirvish-parent-windows ())
    (setq dirvish-preview-buffers ())
    (setq dirvish-parent-buffers ())))

(defun dirvish-quit (&optional keep-alive)
  "Revert dirvish settings and disable dirvish.

Delete current frame if it's a dirvish-only frame unless KEEP-ALIVE
is not-nil."
  (interactive)
  (dirvish-deinit)
  (when (and (not keep-alive)
             (string= (frame-parameter nil 'name) "dirvish-emacs"))
    (delete-frame)))

(defun dirvish-refresh (&optional rebuild filter no-revert)
  "Reset dirvish.
If REBUILD is not-nil, rebuild dirvish layout;
If FILTER is not-nil, update dirvish file filter;
Unless NO-REVERT is not-nil, revert current buffer."
  (interactive "P")
  (when rebuild
    (dirvish-parent-build)
    (dirvish-preview-build)
    (dirvish-header-build))
  (unless no-revert (revert-buffer))
  (when filter (dirvish--update-filter))
  (dirvish-body-update)
  (dirvish-preview-update)
  (dirvish-header-update)
  (dirvish-footer-update))

(defun dirvish-find-file (&optional file ignore-hist)
  "Find file in dirvish buffer.

FILE can be a file or a directory, if nil then infer entry from
variable `buffer-file-name'.  If IGNORE-HIST is non-nil, do not
update `dirvish-history-ring'."
  (interactive)
  (let ((entry (or file (dired-get-filename nil t)))
        (bname (buffer-file-name (current-buffer)))
        (curr-dir (expand-file-name default-directory)))
    (when entry
      (if (file-directory-p entry)
          (let ((hist (directory-file-name entry))
                enable-dir-local-variables)
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
  "Apply `dirvish-find-file' or `dired-find-file' with ARGS."
  (if (derived-mode-p 'dirvish-mode)
      (apply #'dirvish-find-file args)
    (apply #'find-alternate-file args)))

;;;###autoload
(define-minor-mode dirvish-override-dired-jump
  "Override `dired-jump' with `dirvish-jump'."
  :group 'dirvish :global t
  (if dirvish-override-dired-jump
      (advice-add 'dired-jump :around #'dirvish-override-dired)
    (advice-remove 'dired-jump #'dirvish-override-dired)))

;;;###autoload
(defun dirvish (&optional path one-window)
  "Open dirvish in PATH, optionally in single-window if ONE-WINDOW is not-nil.

PATH defaults to variable `buffer-file-name'."
  (interactive)
  (let* ((file (or path buffer-file-name))
         (dir (if file (expand-file-name (file-name-directory file))
                (expand-file-name default-directory))))
    (dirvish-init one-window)
    (dirvish-find-file dir)))

(put 'dired-subdir-alist 'permanent-local t)

(provide 'dirvish)

;;; dirvish.el ends here

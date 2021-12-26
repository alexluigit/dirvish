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
(require 'dirvish-structs)
(require 'dirvish-header)
(require 'dirvish-footer)
(require 'dirvish-parents)
(require 'dirvish-preview)
(require 'dirvish-advices)

;;;; Setup

(when dirvish-show-icons
  (setq dirvish-show-icons (require 'all-the-icons nil t)))
(mailcap-parse-mimetypes)
(put 'dired-subdir-alist 'permanent-local t)

;;;; Commands

;;;;; Dirvish commands

(defun dirvish-other-buffer ()
  "Replacement for `mode-line-other-buffer' in `dirvish-mode'."
  (interactive)
  (let ((one-window (dirvish-one-window-p (dirvish-meta))))
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
  (dirvish-with-update nil
    (goto-char (point-min))
    (dired-next-line 1)
    (dirvish-next-file -1)))

(defun dirvish-go-bottom ()
  "Move to bottom of file list."
  (interactive)
  (dirvish-with-update nil
    (goto-char (point-max))
    (dirvish-next-file 1)))

(defun dirvish-next-file (arg)
  "Move cursor to next line in dirvish and update to preview window.

With optional prefix ARG (\\[universal-argument]), forward ARG
lines."
  (interactive "^p")
  (dirvish-with-update nil
    (dired-next-line arg)
    (cond
     ((eobp) (unless (region-active-p) (forward-line -1)))
     ((bobp) (dired-next-line 1)))))

(defun dirvish-prev-file (arg)
  "Do `dirvish-next-file' in opposite direction with ARG."
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
  (let ((after-make-frame-functions
          (lambda (f)
            (select-frame f)
            (switch-to-buffer (get-buffer-create "*scratch*")))))
    (make-frame '((name . "dirvish-emacs")))
    (dirvish path)))

(defun dirvish-yank (&optional arg)
  "Paste marked files/directory to current directory.

With optional prefix ARG, delete source files/directories."
  (interactive "P")
  (if arg (dirvish--yank 'move) (dirvish--yank)))

(defun dirvish-change-level (&optional arg)
  "Change `dirvish-depth' to ARG."
  (interactive "p")
  (setq dirvish-depth (or arg 1)) (dirvish-reset t))

(defun dirvish-toggle-preview ()
  "Show/hide preview window."
  (interactive)
  (setq dirvish-enable-preview (not dirvish-enable-preview))
  (dirvish-reset t)
  (when dirvish-enable-preview
    (dired-hide-details-mode t)))

(defun dirvish-insert-subdir ()
  "Replacement for `dired-insert-subdir'."
  (interactive)
  (dirvish-with-update t
    (let ((dired-after-readin-hook
           (remove #'dirvish--dired-overrider dired-after-readin-hook)))
      (dired-insert-subdir (dired-get-filename nil t)))))

(defun dirvish-sort-by-criteria (criteria)
  "Call sort-dired by different `CRITERIA'."
  (interactive
   (list
    (read-char-choice
     "Sort by criteria [capital for reverse]: (d/D)efault (e/E)xt (s/S)ize (t/T)ime (c/C)time "
     '(?q ?d ?D ?e ?E ?s ?S ?t ?T ?c ?C))))
  (unless (eq criteria ?q)
    (let* ((c (char-to-string criteria))
           (revp (string-equal c (upcase c)))
           (cc (downcase c))
           (sort-flag
            (cond
             ((string-equal cc "d") '("default" . ""))
             ((string-equal cc "c") '("modified" . " -c"))
             ((string-equal cc "e") '("ext" . " -X"))
             ((string-equal cc "t") '("time" . " -t"))
             ((string-equal cc "s") '("size" . " -S"))))
           (name (concat (car sort-flag) (when revp " [rev]")))
           (switch (concat dired-listing-switches (cdr sort-flag) (when revp " -r"))))
      (setf (dirvish-sort-criteria (dirvish-meta)) (cons name switch))
      (dirvish-reset))))

(defun dirvish-quit (&optional keep-alive)
  "Revert dirvish settings and disable dirvish.

Delete current frame if it's a dirvish-only frame unless KEEP-ALIVE
is not-nil."
  (interactive)
  (dirvish-deinit)
  (when (and (not keep-alive)
             (string= (frame-parameter nil 'name) "dirvish-emacs"))
    (delete-frame)))

(defun dirvish-reset (&optional rebuild no-revert)
  "Reset dirvish.
If REBUILD is not-nil, rebuild dirvish layout.
Unless NO-REVERT, revert current buffer."
  (interactive "P")
  (dirvish-with-update t
    (when rebuild
      (dirvish-parent-build)
      (dirvish-preview-build)
      (dirvish-header-build))
    (unless no-revert (revert-buffer))
    (dirvish--update-sorter)))

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
                (dired-after-readin-hook
                 (remove #'dirvish--dired-overrider dired-after-readin-hook))
                enable-dir-local-variables)
            (unless ignore-hist
              (when (or (ring-empty-p dirvish-history-ring)
                        (not (eq hist (ring-ref dirvish-history-ring 0))))
                (ring-insert dirvish-history-ring hist)))
            (switch-to-buffer (or (car (dired-buffers-for-dir entry))
                                  (dired-noselect entry)))
            (setq dirvish-child-entry (or bname curr-dir))
            (setf (dirvish-index-path (dirvish-meta))
                  (or (dired-get-filename nil t) entry))
            (dirvish-reset t))
        (find-file entry)))))

;;;;; Global commands

;;;###autoload
(defun dirvish-find-file-dwim (&rest args)
  "Apply `dirvish-find-file' or `dired-find-file' with ARGS."
  (if (derived-mode-p 'dirvish-mode)
      (apply #'dirvish-find-file args)
    (apply #'find-alternate-file args)))

;;;###autoload
(define-minor-mode dirvish-override-dired-mode
  "Override Dired with `dirvish-dired' globally."
  :group 'dirvish :global t
  (if dirvish-override-dired-mode
      (add-hook 'dired-after-readin-hook #'dirvish--dired-overrider)
    (remove-hook 'dired-after-readin-hook #'dirvish--dired-overrider)))

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

;;;###autoload
(defun dirvish-dired (&optional path)
  "Open a single window dirvish for PATH."
  (interactive (list (and current-prefix-arg (read-file-name "Dirvish-dired: "))))
  (dirvish path t))

(provide 'dirvish)

;;; dirvish.el ends here

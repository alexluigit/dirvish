;;; dirvish.el --- A modern file manager based on dired mode -*- lexical-binding: t -*-
;; Copyright (C) 2021-2022 Alex Lu

;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 0.9.7
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1"))

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
(declare-function all-the-icons-dired-mode "all-the-icons-dired")

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
(when (require 'pdf-tools nil t)
  (setq dirvish-preview-dispatchers
        (cl-substitute #'dirvish-preview-pdf-tools-dispatcher
                       #'dirvish-preview-pdf-preface-dispatcher
                       dirvish-preview-dispatchers)))
(when (memq system-type '(windows-nt ms-dos))
  (setq dirvish-preview-dispatchers
        (cl-substitute #'dirvish-preview-directory-dired-dispatcher
                       #'dirvish-preview-directory-exa-dispatcher
                       dirvish-preview-dispatchers)))

;;;; Commands

;;;;; Dirvish commands

(defun dirvish-other-buffer ()
  "Replacement for `mode-line-other-buffer' in `dirvish-mode'."
  (interactive)
  (dirvish-find-file (ring-ref dirvish-history-ring 1)))

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

(defun dirvish-go-top (&optional reverse)
  "Move to top of dirvish buffer.
If REVERSE is non-nil, move to bottom instead."
  (interactive)
  (dirvish-with-update nil
    (goto-char (if reverse (point-max) (point-min)))
    (forward-line (if reverse -1 1))))

(defun dirvish-go-bottom ()
  "Move to bottom of dirvish buffer."
  (interactive)
  (dirvish-go-top t))

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
  (let ((fr (make-frame '((name . "dirvish-emacs")))))
    (with-selected-frame fr
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (dirvish path))))

(defun dirvish-change-level (&optional arg)
  "Change `dirvish-depth' to ARG."
  (interactive "p")
  (setq dirvish-depth (or arg 1)) (dirvish-reset t))

(defun dirvish-toggle-preview ()
  "Show/hide preview window."
  (interactive)
  (setq dirvish-enable-preview (not dirvish-enable-preview))
  (dirvish-reset t))

(defun dirvish-sort-by-criteria (criteria)
  "Call `dired-sort-other' by different `CRITERIA'."
  (interactive
   (list
    (read-char-choice
     "Sort by [capital for reverse, q to quit]: (d/D)efault (e/E)xt (s/S)ize (t/T)ime (c/C)time "
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
           (order (concat (cdr sort-flag) (when revp " -r")))
           (dv (dirvish-curr)))
      (setf (dv-sort-criteria dv) (cons name order))
      (dired-sort-other (string-join (list (dv-ls-switches dv) (cdr sort-flag)) " ")))))

(defun dirvish-quit (&optional keep-frame)
  "Revert dirvish settings and disable dirvish.

Delete current frame if it's a dirvish-only frame unless KEEP-FRAME
is not-nil."
  (interactive)
  (dirvish-deactivate)
  (when (and (not keep-frame)
             (string= (frame-parameter nil 'name) "dirvish-emacs"))
    (delete-frame)))

(defun dirvish-reset (&optional rebuild)
  "Reset dirvish.
If REBUILD is not-nil, rebuild dirvish layout."
  (interactive "P")
  (dirvish-with-update t
    (when rebuild
      (unless (dv-one-window-p (dirvish-curr))
        (delete-other-windows))
      (dirvish-preview-build)
      (dirvish-header-build)
      (dirvish-parent-build))))

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
            (switch-to-buffer (dired-noselect entry (dv-ls-switches (dirvish-curr))))
            (setq dirvish-child-entry (or bname curr-dir))
            (setf (dv-index-path (dirvish-curr))
                  (or (dired-get-filename nil t) entry))
            (dirvish-reset t))
        (find-file entry)))))

(defvar dirvish-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap dired-do-redisplay]           'dirvish-change-level)
    (define-key map [remap dired-hide-details-mode]      'dirvish-toggle-preview)
    (define-key map [remap dired-find-file]              'dirvish-find-file)
    (define-key map [remap dired-find-alternate-file]    'dirvish-find-file)
    (define-key map [remap right-char]                   'dirvish-find-file)
    (define-key map [remap dired-up-directory]           'dirvish-up-directory)
    (define-key map [remap left-char]                    'dirvish-up-directory)
    (define-key map [remap end-of-buffer]                'dirvish-go-bottom)
    (define-key map [remap beginning-of-buffer]          'dirvish-go-top)
    (define-key map [remap dired-sort-toggle-or-edit]    'dirvish-sort-by-criteria)
    (define-key map [remap dired-view-file]              'dirvish-toggle-preview)
    (define-key map [remap quit-window]                  'dirvish-quit)
    (define-key map [remap +dired/quit-all]              'dirvish-quit) ; For doom-emacs
    map)
  "Dirvish mode map.")

;;;;; Global commands

(define-derived-mode dirvish-mode dired-mode "Dirvish"
  "Convert Dired buffer to a Dirvish buffer."
  :group 'dirvish
  :interactive nil
  (let ((dv (dirvish-curr)))
    (when dirvish-child-entry (dired-goto-file dirvish-child-entry))
    (push (selected-window) (dv-parent-windows dv))
    (push (current-buffer) (dv-parent-buffers dv))
    (setq-local dirvish--curr-name (dv-name dv))
    (setq-local revert-buffer-function #'dirvish-revert)
    (when (bound-and-true-p all-the-icons-dired-mode)
      (all-the-icons-dired-mode -1)
      (setq-local tab-width 2))
    (set (make-local-variable 'face-remapping-alist)
         dirvish-parent-face-remap-alist)
    (setq cursor-type nil)
    (setq mode-line-format nil)
    (setq-local face-font-rescale-alist nil)
    (set-window-fringes nil 1 1)
    (dirvish-body-update)
    (if (eq (dv-root-window dv) (selected-window))
        (and dirvish-enable-preview (dired-hide-details-mode t))
      (dired-hide-details-mode t))))

;;;###autoload
(define-minor-mode dirvish-override-dired-mode
  "Override Dired with `dirvish-dired' globally."
  :group 'dirvish :global t
  (if dirvish-override-dired-mode
      (progn
        (dirvish--add-advices)
        (setq find-directory-functions
              (cl-substitute #'dirvish-dired #'dired-noselect find-directory-functions)))
    (dirvish--clean-advices)
    (setq find-directory-functions
          (cl-substitute #'dired-noselect #'dirvish-dired find-directory-functions))))

;;;###autoload
(defun dirvish (&optional path one-window)
  "Open dirvish in PATH, optionally in single-window if ONE-WINDOW is not-nil.

PATH defaults to variable `buffer-file-name'."
  (interactive)
  (let* ((file (or path buffer-file-name))
         (dir (if file (expand-file-name (file-name-directory file))
                (expand-file-name default-directory))))
    (dirvish-activate one-window)
    (dirvish-find-file dir)))

;;;###autoload
(defun dirvish-dired (&optional path other-window)
  "Open a single window dirvish for PATH.
If OTHER-WINDOW is non-nil, do it in other window."
  (interactive (list (and current-prefix-arg (read-file-name "Dirvish-dired: ")) nil))
  (and other-window (switch-to-buffer-other-window "*scratch*")) ; avoid layered dirvish instance
  (dirvish path t))

(provide 'dirvish)

;;; dirvish.el ends here

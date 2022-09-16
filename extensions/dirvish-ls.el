;;; dirvish-ls.el --- Setup ls command switches on the fly -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Setup ls command switches on the fly.

;;; Code:

(require 'dirvish)

(defun dirvish-ls--clear-switches-choices ()
  "Reload the listing switches setup UI."
  (interactive)
  (transient-setup 'dirvish-ls-switches-menu))

(defun dirvish-ls--apply-switches-to-buffer (&optional switches)
  "Apply listing SWITCHES to current buffer."
  (interactive)
  (let* ((args (transient-args transient-current-command))
         (switches (or switches (string-join (append '("-l") args) " "))))
    (when current-prefix-arg (setq dired-listing-switches switches))
    (setq dired-actual-switches switches)
    (revert-buffer)))

(defun dirvish-ls--apply-switches-to-all (&optional switches)
  "Apply listing SWITCHES to current session."
  (interactive)
  (let* ((args (transient-args transient-current-command))
         (switches (or switches (string-join (append '("-l") args) " "))))
    (when current-prefix-arg (setq dired-listing-switches switches))
    (setf (dv-ls-switches (dirvish-curr)) switches)
    (dolist (buf (cl-remove-if-not
                  (lambda (b) (with-current-buffer b (derived-mode-p 'dired-mode))) (buffer-list)))
      (with-current-buffer buf
        (setq dired-actual-switches switches)
        (revert-buffer)))))

(defun dirvish-ls--reset-switches-for-buffer ()
  "Reset listing switches for current buffer."
  (interactive)
  (dirvish-ls--apply-switches-to-buffer dired-listing-switches))

(defun dirvish-ls--reset-switches-for-all ()
  "Reset listing switches for current buffer."
  (interactive)
  (dirvish-ls--apply-switches-to-all dired-listing-switches))

(transient-define-infix dirvish-ls--filter-switch ()
  :description "show all files"
  :class 'transient-switches
  :argument-format "--%s"
  :argument-regexp "\\(--\\(all\\|almost-all\\)\\)"
  :choices '("all" "almost-all"))

(transient-define-infix dirvish-ls--sort-switch ()
  :description "sort by"
  :class 'transient-switches
  :argument-format "--sort=%s"
  :argument-regexp "\\(--sort=\\(time\\|none\\|extension\\|size\\|version\\|width\\)\\)"
  :choices '("time" "none" "extension" "size" "version" "width"))

(transient-define-infix dirvish-ls--time-switch ()
  :description "show time as | sort files with"
  :class 'transient-switches
  :argument-format "--time=%s"
  :argument-regexp "\\(--time=\\(use\\|birth\\|ctime\\)\\)"
  :choices '("use" "birth" "ctime"))

(transient-define-infix dirvish-ls--time-style-switch ()
  :description "time style"
  :class 'transient-switches
  :argument-format "--time-style=%s"
  :argument-regexp "\\(--time-style=\\(full-iso\\|long-iso\\|iso\\|locale\\|+\\)\\)"
  :choices '("full-iso" "long-iso" "iso" "locale" "+"))

(transient-define-infix dirvish-ls--indicator-style-switch ()
  :description "add indicator"
  :class 'transient-switches
  :argument-format "--indicator-style=%s"
  :argument-regexp "\\(--indicator-style=\\(slash\\|file-type\\|classify\\)\\)"
  :choices '("slash" "file-type" "classify"))

(defun dirvish-ls--quicksort-do-sort (switches)
  "Sort current buffer with Dired sort SWITCHES."
  (let* ((regexp "\\(--time=\\w+\\|--sort=\\w+\\|--reverse\\)\\( \\)?")
         (others (replace-regexp-in-string regexp "" dired-actual-switches))
         (new-switches (concat others " " switches)))
    (setq dired-actual-switches new-switches)
    (revert-buffer)))

;;;###autoload (autoload 'dirvish-quicksort "dirvish-ls" nil t)
(defcustom dirvish-ls-quicksort-keys
  '(("n" ""                                   "name (a-z)")
    ("N" "--reverse"                          "name (z-a)")
    ("e" "--sort=extension"                   "extension (a-z)")
    ("E" "--sort=extension --reverse"         "extension (z-a)")
    ("s" "--sort=size"                        "size (largest first)")
    ("S" "--sort=size --reverse"              "size (smallest first)")
    ("v" "--sort=version"                     "version number (earliest first)")
    ("V" "--sort=version --reverse"           "version number (latest first)")
    ("w" "--sort=width"                       "width (shortest first)")
    ("W" "--sort=width --reverse"             "width (longest first)")
    ("m" "--sort=time"                        "modification time (newest first)")
    ("M" "--sort=time --reverse"              "modification time (oldest first)")
    ("a" "--sort=time --time=use"             "access time (newest first)")
    ("A" "--sort=time --time=use --reverse"   "access time (oldest first)")
    ("b" "--sort=time --time=birth"           "birth time (newest first)")
    ("B" "--sort=time --time=birth --reverse" "birth time (oldest first)")
    ("c" "--sort=time --time=ctime"           "change time (newest first)")
    ("C" "--sort=time --time=ctime --reverse" "change time (oldest first)"))
  "SORT-KEYs for command `dirvish-quicksort'.
A SORT-KEY is a (KEY SWITCHES DOC) alist where KEY is the key to
invoke the sort function, SWITCHES is the the sort flags for
`dired-sort-other', DOC is the documentation string."
  :group 'dirvish :type 'alist
  :set
  (lambda (k v)
    (set k v)
    (eval
     `(transient-define-prefix dirvish-quicksort ()
        "Sort Dirvish buffer by different criteria."
        [:description
         (lambda () (dirvish--format-menu-heading "Sort by:"))
         ,@(cl-loop
            for (key switches desc) in v collect
            (list key desc `(lambda ()
                              (interactive)
                              (dirvish-ls--quicksort-do-sort ,switches))))]))))

;;;###autoload (autoload 'dirvish-ls-switches-menu "dirvish-ls" nil t)
(transient-define-prefix dirvish-ls-switches-menu ()
  "Setup Dired listing switches."
  :init-value
  (lambda (o) (oset o value (split-string (or dired-actual-switches ""))))
  [:description
   (lambda ()
     (format "%s\n%s %s\n%s %s"
             (propertize "Setup Listing Switches"
                         'face '(:inherit dired-mark :underline t)
                         'display '((height 1.2)))
             (propertize "lowercased switches also work in" 'face 'font-lock-doc-face)
             (propertize "dired-hide-details-mode" 'face 'font-lock-constant-face)
             (propertize "C-u RET and C-u M-RET will modify" 'face 'font-lock-doc-face)
             (propertize "dired-listing-switches" 'face 'font-lock-constant-face)))
   ["options"
    ("a" dirvish-ls--filter-switch)
    ("s" dirvish-ls--sort-switch)
    ("i" dirvish-ls--indicator-style-switch)
    ("t" dirvish-ls--time-switch)
    ("T" dirvish-ls--time-style-switch)
    ("B" "Scale sizes when printing, eg. 10K" "--block-size=")
    ""
    "toggles"
    ("r" "Reverse order while sorting" "--reverse")
    ("d" "List directories ontop" "--group-directories-first")
    ("~" "Hide backups files (eg. foo~)" "--ignore-backups")
    ("A" "Show the author" "--author")
    ("C" "Show security context" "--context")
    ("H" "Human readable file size" "--human-readable")
    ("G" "Hide group names" "--no-group")
    ("O" "Hide owner names" "-g")
    ("L" "Info for link references or link itself" "--dereference")
    ("N" "Numeric user and group IDs" "--numeric-uid-gid")
    ("P" "Powers of 1000 for file size rather than 1024" "--si")
    ("I" "Show index number" "--inode")
    ("S" "Show the allocated size" "--size")
    ""
    "Actions"
    ("RET" "  Apply to this buffer" dirvish-ls--apply-switches-to-buffer)
    ("M-RET" "Apply to all Dired buffers" dirvish-ls--apply-switches-to-all)
    ("C-r" "  Reset this buffer" dirvish-ls--reset-switches-for-buffer)
    ("M-r" "  Reset all Dired buffers" dirvish-ls--reset-switches-for-all)
    ("C-l" "  Clear choices" dirvish-ls--clear-switches-choices :transient t)]])

(provide 'dirvish-ls)
;;; dirvish-ls.el ends here

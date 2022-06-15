;;; dirvish-fd.el --- find-dired alternative using fd  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.3.21
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `fd' integration for Dirvish.

;;; Code:

(require 'dirvish)

(defcustom dirvish-fd-switches ""
  "Fd arguments inserted before user input."
  :type 'string :group 'dirvish)

(defcustom dirvish-fd-ls-program
  (if-let ((gls (executable-find "gls"))) gls insert-directory-program)
  "Listing program for `fd'."
  :type '(string :tag "Listing program, such as `ls'") :group 'dirvish
  :set (lambda (k v)
         (set k v)
         (and (eq system-type 'darwin) (not (executable-find "gls"))
              (warn "Please install `ls' from coreutils with 'brew install coreutils'"))))

(defconst dirvish-fd-bufname "FD####%s####%s####%s")
(defvar dirvish-fd-program "fd" "The default fd program.")
(defvar dirvish-fd-args-history nil "History list of fd arguments entered in the minibuffer.")
(defvar dirvish-fd-last-input "" "Last used fd arguments.")
(defvar dirvish-fd-actual-switches nil)

(defun dirvish-fd-proc-filter (proc string)
  "Filter for `dirvish-fd' processes PROC and output STRING."
  (let ((buf (process-buffer proc))
	      (inhibit-read-only t))
    (if (buffer-name buf)
	      (with-current-buffer buf
	        (save-excursion
	          (save-restriction
	            (widen)
	            (let ((beg (point-max)))
		            (goto-char beg)
		            (insert string)
		            (goto-char beg)
		            (or (looking-at "^") (forward-line 1))
		            (while (looking-at "^") (insert "  ") (forward-line 1))
		            (goto-char (- beg 3))	; no error if < 0
		            (while (search-forward " ./" nil t)
		              (delete-region (point) (- (point) 2)))))))
      (delete-process proc))))

(cl-defmethod dirvish-search-switches-ml (_dv &context (dirvish-fd-actual-switches string))
  "Return a string showing the DIRVISH-FD-ACTUAL-SWITCHES."
  (unless (dirvish-prop :fd-heading)
    (dirvish-prop :fd-heading
      (let* ((args (split-string dirvish-fd-actual-switches))
             (globp (member "--glob" args))
             (casep (member "--case-sensitive" args))
             (ign-range (cond ((member "--no-ignore" args) "no")
                              ((member "--no-ignore-vcs" args) "no_vcs")
                              (t "all")))
             types exts)
        (dolist (arg args)
          (cond ((string-prefix-p "--type=" arg) (push (substring arg 8) types))
                ((string-prefix-p "--extension=" arg) (push (substring arg 12) exts))))
        (setq types (mapconcat #'concat types ","))
        (setq exts (mapconcat #'concat exts ","))
        (format " %s | %s %s | %s %s | %s %s | %s %s | %s %s | "
                (propertize "FD" 'face 'dired-header)
                (propertize (if globp "glob:" "regex:") 'face 'font-lock-doc-face)
                (propertize dirvish-fd-last-input 'face 'font-lock-regexp-grouping-construct)
                (propertize "type:" 'face 'font-lock-doc-face)
                (propertize (if (equal types "") "all" types) 'face 'font-lock-variable-name-face)
                (propertize "case:" 'face 'font-lock-doc-face)
                (propertize (if casep "sensitive" "smart") 'face 'font-lock-type-face)
                (propertize "ignore:" 'face 'font-lock-doc-face)
                (propertize ign-range 'face 'font-lock-comment-face)
                (propertize "exts:" 'face 'font-lock-doc-face)
                (propertize (if (equal exts "") "all" exts) 'face 'font-lock-string-face)))))
  (dirvish-prop :fd-heading))

;;;###autoload
(defun dirvish-fd (dir pattern)
  "Run `fd' on DIR and go into Dired mode on a buffer of the output.
The command run is essentially:

  fd --color=never -0 `dirvish-fd-switches' PATTERN
     --exec-batch `dirvish-fd-ls-program' `dired-listing-switches' --directory."
  (interactive (list (and current-prefix-arg
                          (read-directory-name "Fd target directory: " nil "" t))
                     (read-string "Fd search pattern: " dirvish-fd-last-input
                                  '(dirvish-fd-args-history . 1))))
  (unless (executable-find "fd")
    (user-error "Dirvish: install `fd' to use this command"))
  ;; In case users issue a new `dirvish-fd' when already in a result buffer
  (when (and (or (not dir) current-prefix-arg)
             dirvish-fd-actual-switches)
    (setq dirvish-fd-actual-switches nil))
  (setq dir (file-name-as-directory (expand-file-name (or dir default-directory))))
  (or (file-directory-p dir) (user-error "'fd' command requires a directory: %s" dir))
  (let* ((reuse (when (dirvish-prop :fd-dir) (current-buffer)))
         (buf-name (format dirvish-fd-bufname dir pattern (make-temp-name "")))
         (buffer (or reuse (get-buffer-create buf-name))))
    (dirvish-with-no-dedication (pop-to-buffer-same-window buffer))
    (with-current-buffer buffer
      (widen)
      (let ((ls-switches (or dired-actual-switches
                             (and reuse (dv-ls-switches (dirvish-curr)))
                             dired-listing-switches))
            (fmt "%s -0 --color=never %s %s --exec-batch %s %s --quoting-style=literal --directory &")
            buffer-read-only)
        (erase-buffer)
        (setq default-directory dir)
        (setq dirvish-fd-last-input pattern) ; save for next interactive call
        (setq dirvish-fd-actual-switches (or dirvish-fd-actual-switches dirvish-fd-switches))
        (shell-command (format fmt dirvish-fd-program dirvish-fd-actual-switches pattern
                               dirvish-fd-ls-program ls-switches)
                       buffer)
        (dired-mode dir ls-switches)
        (setq-local revert-buffer-function
                    (lambda (_ignore-auto _noconfirm)
                      (dirvish-fd default-directory dirvish-fd-last-input)))
        (setq-local dired-subdir-alist (list (cons default-directory (point-min-marker))))
        (let (buffer-read-only)
          (insert "  " dir ":\n" (if dirvish--dired-free-space "total used in directory\n" ""))))
      (let ((proc (get-buffer-process buffer)))
        (set-process-filter proc #'dirvish-fd-proc-filter)
        (set-process-sentinel proc #'dirvish-find-dired-sentinel-ad)
        ;; Initialize the process marker; it is used by the filter.
        (move-marker (process-mark proc) (point) buffer)))
    buffer))

(define-obsolete-function-alias 'dirvish-roam #'dirvish-fd-roam "Jun 08, 2022")
;;;###autoload
(defun dirvish-fd-roam ()
  "Browse all directories using `fd' command.
This command takes a while to index all the directories the first
time you run it.  After the indexing, it fires up instantly."
  (interactive)
  (unless (executable-find "fd")
    (user-error "Dirvish: install `fd' to use this command"))
  (let* ((command "fd -H -td -0 . /")
         (output (shell-command-to-string command))
         (files-raw (split-string output "\0" t))
         (files (dirvish--append-metadata 'file files-raw))
         (file (completing-read "Goto: " files)))
    (dired-jump nil file)))

(provide 'dirvish-fd)
;;; dirvish-fd.el ends here

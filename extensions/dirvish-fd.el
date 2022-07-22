;;; dirvish-fd.el --- find-dired alternative using fd  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.9.23
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
  (let* ((ls (executable-find "ls"))
         (gls (executable-find "gls"))
         (idp (executable-find insert-directory-program))
         (ls-is-gnu? (and ls (= 0 (call-process ls nil nil nil "--version"))))
         (idp-is-gnu-ls? (and idp (= 0 (call-process idp nil nil nil "--version")))))
    (cond
     ;; just use GNU ls if found
     (ls-is-gnu? ls)
     ;; use insert-directory-program if it points to GNU ls
     (idp-is-gnu-ls? insert-directory-program)
     ;; heuristic: GNU ls is often installed as gls by Homebrew on Mac
     ((and (eq system-type 'darwin) gls) gls)
     ;; fallback: use insert-directory-program, but warn the user that it may not be compatible
     (t (warn "`dirvish-fd' requires `ls' from GNU coreutils, please install it")
        insert-directory-program)))
  "Listing program for `fd'."
  :type '(string :tag "Listing program, such as `ls'") :group 'dirvish)

(defconst dirvish-fd-bufname "FD####%s####%s####%s")
(defconst dirvish-fd--header
  (dirvish--mode-line-fmt-setter '(:left (fd-switches) :right (fd-timestamp fd-pwd " ")) t))
(defvar dirvish-fd-program "fd" "The default fd program.")
(defvar dirvish-fd-args-history nil "History list of fd arguments entered in the minibuffer.")
(defvar dirvish-fd-last-input "" "Last used fd arguments.")
(defvar dirvish-fd-actual-switches nil)

(defun dirvish-fd--apply-switches ()
  "Apply fd SWITCHES to current buffer."
  (interactive)
  (let* ((args (transient-args transient-current-command))
         (switches (string-join args " ")))
    (setq dirvish-fd-actual-switches switches)
    (revert-buffer)))

(transient-define-infix dirvish-fd--extensions-switch ()
  :description "Filter results by file extensions"
  :class 'transient-option
  :argument "--extension="
  :multi-value 'repeat
  :prompt
  (lambda (o)
    (let* ((val (oref o value))
           (str (if val (format "(current: %s) " (mapconcat #'concat val ",")) "")))
      (format "%sFile exts separated with comma: " str))))

(transient-define-infix dirvish-fd--exclude-switch ()
  :description "Exclude files/dirs that match the glob pattern"
  :class 'transient-option
  :argument "--exclude="
  :multi-value 'repeat
  :prompt
  (lambda (o)
    (let* ((val (oref o value))
           (str (if val (format "(current: %s) " (mapconcat #'concat val ",")) "")))
      (format "%sGlob patterns (such as *.pyc) separated with comma: " str))))

(transient-define-infix dirvish-fd--search-pattern-infix ()
  "Change search pattern."
  :description "Change search pattern"
  :class 'transient-lisp-variable
  :variable 'dirvish-fd-last-input
  :reader (lambda (_prompt _init _hist)
            (completing-read "Input search pattern: "
                             dirvish-fd-args-history nil nil dirvish-fd-last-input)))

(defun dirvish-fd-sentinel (proc _)
  "Sentinel for `dirvish-fd' processes PROC."
  (let ((dv (dirvish-curr))
        (bufname (buffer-name))
        buffer-read-only)
    (setf (dv-index-dir dv) (cons bufname (current-buffer)))
    (unless (alist-get bufname (dv-roots dv) nil nil #'equal)
      (push (cons bufname (current-buffer)) (dv-roots dv)))
    (with-current-buffer (process-buffer proc)
      (setq-local dirvish--attrs-hash (make-hash-table :test #'equal))
      (dirvish-prop :child (dired-get-filename nil t))
      (dirvish-prop :fd-header 'dirvish-fd--header)
      (delete-matching-lines "find finished at.*\\|^ +$")
      (dirvish--hide-dired-header))
    (dirvish--build dv)))

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

(dirvish-define-mode-line fd-switches
  "Return a formatted string showing the DIRVISH-FD-ACTUAL-SWITCHES."
  (unless (dirvish-prop :fd-header-string)
    (dirvish-prop :fd-header-string
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
  (dirvish-prop :fd-header-string))

(dirvish-define-mode-line fd-timestamp
  "Timestamp of search finished."
  (unless (dirvish-prop :fd-time)
    (dirvish-prop :fd-time
      (format " %s %s  "
              (propertize "Finished at:" 'face 'font-lock-doc-face)
              (propertize (current-time-string) 'face 'success))))
  (when (dv-layout dv) (dirvish-prop :fd-time)))

(dirvish-define-mode-line fd-pwd
  "Current working directory."
  (propertize (abbreviate-file-name default-directory) 'face 'dired-directory))

;;;###autoload (autoload 'dirvish-fd-switches-menu "dirvish-fd" nil t)
(transient-define-prefix dirvish-fd-switches-menu ()
  "Setup fd switches."
  :init-value
  (lambda (o) (oset o value (split-string (or dirvish-fd-actual-switches ""))))
  [:description (lambda () (dirvish--format-menu-heading
                       "Setup FD Switches"
                       "Ignore Range (by default ignore ALL)
  VCS: .gitignore + .git/info/exclude + $HOME/.config/git/ignore
  ALL: VCS + .ignore + .fdignore + $HOME/.config/fd/ignore"))
   ["File types (multiple types can be included)"
    (3 "f" " Search for regular files" "--type=file")
    (3 "d" " Search for directories" "--type=directory")
    (3 "l" " Search for symbolic links" "--type=symlink")
    (3 "s" " Search for sockets" "--type=socket")
    (3 "p" " Search for named pipes" "--type=pipe")
    (3 "x" " Search for executable" "--type=executable")
    (3 "e" " Search for empty files or directories" "--type=empty")
    ""
    "Toggles"
    (3 "-H" "Include hidden files|dirs in the results" "--hidden")
    (3 "-I" "Show results from ALL" "--no-ignore")
    (4 "iv" "Show results from VCS" "--no-ignore-vcs")
    (5 "ip" "Show results from .gitignore in parent dirs" "--no-ignore-parent")
    (3 "-s" "Perform a case-sensitive search" "--case-sensitive")
    (4 "-g" "Perform a glob-based (rather than regex-based) search" "--glob")
    (4 "-F" "Treat the pattern as a literal string" "--fixed-strings")
    (4 "-L" "Traverse symbolic links" "--follow")
    (4 "-p" "Let the pattern match against the full path" "--full-path")
    (5 "mr" "Maximum number of search results" "--max-results")
    (5 "mt" "Do not descend into a different file systems" "--mount")
    (5 "P" " Do not traverse into matching directories" "--prune")
    ""
    "Options"
    (4 "-e" dirvish-fd--extensions-switch)
    (4 "-E" dirvish-fd--exclude-switch)
    (4 "-D" "Max level for directory traversing" "--max-depth=")
    (5 "-d" "Only show results starting at the depth" "--mix-depth=")
    (5 "gd" "Only show results starting at the exact given depth" "--exact-depth=")
    (5 "if" "Add a custom ignore-file in '.gitignore' format" "--ignore-file="
       :reader (lambda (_prompt _init _hist) (read-file-name "Choose ignore file: ")))
    (5 "-S" "Limit results based on the size of files" "--size="
       :reader (lambda (_prompt _init _hist)
                 (read-string "Input file size using the format <+-><NUM><UNIT> (eg. +100m): ")))
    (5 "cn" "Filter results based on the file mtime newer than" "--changed-within="
       :reader (lambda (_prompt _init _hist)
                 (read-string "Input a duration (10h, 1d, 35min) or a time point (2018-10-27 10:00:00): ")))
    (5 "co" "Filter results based on the file mtime older than" "--changed-before="
       :reader (lambda (_prompt _init _hist)
                 (read-string "Input a duration (10h, 1d, 35min) or a time point (2018-10-27 10:00:00): ")))
    (6 "-o" "Filter files by their user and/or group" "--owner="
       :reader (lambda (_prompt _init _hist)
                 (read-string "user|uid:group|gid - eg. john, :students, !john:students ('!' means to exclude files instead): ")))
    ""
    "Actions"
    ("r" dirvish-fd--search-pattern-infix)
    ("RET" "Apply switches" dirvish-fd--apply-switches)]])

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
  (let* ((dv (or (dirvish-prop :dv) (dirvish-new nil :layout dirvish-default-layout)))
         (reuse (when (dirvish-prop :fd-header) (current-buffer)))
         (buf-name (format dirvish-fd-bufname dir pattern (make-temp-name "")))
         (buffer (or reuse (get-buffer-create buf-name))))
    (dirvish-with-no-dedication (pop-to-buffer-same-window buffer))
    (dirvish-prop :dv dv)
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
        (dirvish-mode)
        (setq-local revert-buffer-function
                    (lambda (_ignore-auto _noconfirm)
                      (dirvish-fd default-directory dirvish-fd-last-input)))
        (setq-local dired-subdir-alist (list (cons default-directory (point-min-marker))))
        (let (buffer-read-only)
          (insert "  " dir ":\n" (if dirvish--dired-free-space "total used in directory\n" ""))))
      (let ((proc (get-buffer-process buffer)))
        (set-process-filter proc #'dirvish-fd-proc-filter)
        (set-process-sentinel proc #'dirvish-fd-sentinel)
        ;; Initialize the process marker; it is used by the filter.
        (move-marker (process-mark proc) (point) buffer)))
    buffer))

(define-obsolete-function-alias 'dirvish-roam #'dirvish-fd-jump "Jun 08, 2022")
(define-obsolete-function-alias 'dirvish-fd-roam #'dirvish-fd-jump "Jul 17, 2022")
;;;###autoload
(defun dirvish-fd-jump ()
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
         (file (completing-read "Go to: " files)))
    (dired-jump nil file)))

(provide 'dirvish-fd)
;;; dirvish-fd.el ends here

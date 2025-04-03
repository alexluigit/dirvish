;;; dirvish-fd.el --- find-dired alternative using fd  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.2.7
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `fd' integration for Dirvish.

;;; Code:

(require 'dirvish)
(require 'transient)

(defcustom dirvish-fd-switches ""
  "Fd arguments inserted before user input."
  :type 'string :group 'dirvish)

(defun dirvish-fd--find-fd-program (&optional remote)
  "Find fd programm on a local or `REMOTE' host ."
  (let ((fd (executable-find "fd" remote))
        (fdfind (executable-find "fdfind" remote)))
    (cond (fd fd)
          (fdfind fdfind)
          (t nil))))

(defcustom dirvish-fd-program
  (dirvish-fd--find-fd-program)
  "The default fd program."
  :type 'string :group 'dirvish)

(defcustom dirvish-fd-setup-hook nil
  "Functions called after the `fd` process exits successfully."
  :type 'hook :group 'dirvish)

(defun dirvish-fd--find-gnu-ls (&optional remote)
  "Find ls from gnu coreutils on a local or REMOTE host ."
  (let* ((ls (executable-find "ls" remote))
         (gls (executable-find "gls" remote))
         (idp (executable-find insert-directory-program remote))
         (ls-is-gnu? (and ls (= 0 (process-file ls nil nil nil "--version"))))
         (idp-is-gnu-ls?
          (and idp (= 0 (process-file idp nil nil nil "--version")))))
    (cond
     ;; just use GNU ls if found
     (ls-is-gnu? ls)
     ;; use insert-directory-program if it points to GNU ls
     (idp-is-gnu-ls? insert-directory-program)
     ;; heuristic: GNU ls is often installed as gls by Homebrew on Mac
     ((and (eq system-type 'darwin) gls) gls)
     ;; fallback: use insert-directory-program, but warn the user that it may not be compatible
     (t (warn "`dirvish-fd' requires `ls' from GNU coreutils, please install it")
        insert-directory-program))))

(defcustom dirvish-fd-ls-program
  (dirvish-fd--find-gnu-ls)
  "Listing program for `fd'."
  :type '(string :tag "Listing program, such as `ls'") :group 'dirvish)

(defcustom dirvish-fd-default-dir "/"
  "Default directory for `dirvish-fd-jump'."
  :group 'dirvish :type 'directory)

(defconst dirvish-fd-bufname "🔍%s📁%s📁")
(defconst dirvish-fd-header
  (dirvish--mode-line-composer '(fd-switches) '(fd-took) t))
(defvar-local dirvish-fd--input "" "Last used fd user input.")

(defun dirvish-fd--ensure-fd (remote)
  "Return fd executable on REMOTE or localhost.
Raise an error if fd executable is not available."
  (or (and remote (dirvish-fd--find-fd-program remote)) dirvish-fd-program
      (user-error "`dirvish-fd' requires `fd', please install it")))

(defun dirvish-fd--apply-switches ()
  "Apply fd SWITCHES to current buffer."
  (interactive)
  (let* ((args (transient-args transient-current-command))
         (switches (string-join args " ")))
    (dirvish-prop :fd-switches switches)
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
  :variable 'dirvish-fd--input
  :reader (lambda (_prompt init hist)
            (completing-read "Regex for fd: " nil nil nil init hist)))

;;;###autoload (autoload 'dirvish-fd-switches-menu "dirvish-fd" nil t)
(transient-define-prefix dirvish-fd-switches-menu ()
  "Setup fd switches."
  :init-value
  (lambda (o) (oset o value (split-string (or (dirvish-prop :fd-switches) ""))))
  [:description
   (lambda () (dirvish--format-menu-heading
          "Setup FD Switches"
          "Ignore Range [by default ignore ALL]
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

(defun dirvish-fd--argparser (args)
  "Parse fd args to a list of flags from ARGS."
  (let* ((globp (member "--glob" args))
         (casep (member "--case-sensitive" args))
         (ign-range (cond ((member "--no-ignore" args) "no")
                          ((member "--no-ignore-vcs" args) "no_vcs")
                          (t "all")))
         types exts excludes)
    (dolist (arg args)
      (cond ((string-prefix-p "--type=" arg) (push (substring arg 8) types))
            ((string-prefix-p "--extension=" arg) (push (substring arg 12) exts))
            ((string-prefix-p "--exclude=" arg) (push (substring arg 10) excludes))))
    (setq types (mapconcat #'concat types ","))
    (setq exts (mapconcat #'concat exts ","))
    (setq excludes (mapconcat #'concat excludes ","))
    (dirvish-prop :fd-arglist (list globp casep ign-range types exts excludes))))

(dirvish-define-mode-line fd-switches
  "Return a formatted string showing the DIRVISH-FD-ACTUAL-SWITCHES."
  (pcase-let ((`(,globp ,casep ,ign-range ,types ,exts ,excludes)
               (dirvish-prop :fd-arglist))
              (face (if (dirvish--selected-p) 'dired-header 'dirvish-inactive)))
    (format "  🔍 ⋗ 📁: %s [ %s \"%s\" | %s %s | %s %s | %s %s | %s %s | %s %s ]"
            (propertize
             (abbreviate-file-name default-directory) 'face 'dired-directory)
            (propertize (if globp "glob:" "regex:") 'face face)
            (propertize (or dirvish-fd--input "")
                        'face 'font-lock-regexp-grouping-construct)
            (propertize "type:" 'face face)
            (propertize (if (equal types "") "all" types)
                        'face 'font-lock-variable-name-face)
            (propertize "case:" 'face face)
            (propertize (if casep "sensitive" "smart")
                        'face 'font-lock-type-face)
            (propertize "ignore:" 'face face)
            (propertize ign-range 'face 'font-lock-comment-face)
            (propertize "exts:" 'face face)
            (propertize (if (equal exts "") "all" exts)
                        'face 'font-lock-string-face)
            (propertize "excludes:" 'face face)
            (propertize (if (equal excludes "") "none" excludes)
                        'face 'font-lock-string-face))))

(dirvish-define-mode-line fd-took
  "Time took by last fd search."
  (or (dirvish-prop :fd-time)
      (format "%s %s %s"
              (propertize "Fd indexing… " 'face 'warning)
              (substitute-command-keys "\\[kill-current-buffer]")
              (propertize "to abort" 'face 'warning))))

;;;###autoload
(defun dirvish-fd-jump (&optional current-dir-p)
  "Browse directories using `fd' command.
This command takes a while to index all the directories the first time
you run it.  After the indexing, it fires up instantly except for those
huge directories such as root.  It is recommended to setup your
.fdignore properly before using this command.

If called with \\`C-u' or if CURRENT-DIR-P holds the value 4,
search for directories in the current directory.  Otherwise,
search for directories in `dirvish-fd-default-dir'.

If prefixed twice with \\`C-u' or if CURRENT-DIR-P holds the
value 16, let the user choose the root directory of their search."
  (interactive "p")
  (let* ((base-dir (cond
                    ((eq current-dir-p 4) default-directory)
                    ((eq current-dir-p 16)
                     (let ((dir (car (find-file-read-args
                                      "Select root directory: " nil))))
                       (if (file-directory-p dir)
                           (file-name-as-directory dir)
                         (dirvish--get-parent-path dir))))
                    (t dirvish-fd-default-dir)))
         (remote (file-remote-p base-dir))
         (fd-program (dirvish-fd--ensure-fd remote)))
    (let* ((command (concat fd-program " -H -td --color=never -0 . "
                            (file-local-name base-dir)))
           (default-directory base-dir)
           (output (shell-command-to-string command))
           (files-raw (split-string output "\0" t))
           (files (dirvish--completion-table-with-metadata
                   files-raw '((category . file))))
           (file (completing-read "Go to: " files))
           (full-file (concat remote file)))
      (dired-jump nil full-file))))

(defun dirvish-fd-proc-filter (proc string)
  "Filter for output STRING of `dirvish-fd''s processes PROC."
  (let ((buf (process-buffer proc)))
    (if (not (buffer-name buf)) (delete-process proc)
      (with-current-buffer buf
        (save-excursion
          (save-restriction
            (widen)
            (let ((data (dirvish-prop :fd-cache)) buffer-read-only last file)
              (goto-char (point-max)) (insert string)
              (goto-char (process-mark proc))
              (or (looking-at "^") (forward-line 1))
              ;; strip " ./" prefix and collect data on complete lines
              (while-let ((fb (search-forward " ./" nil t))
                          (le (line-end-position)) ((eq (char-after le) 10)))
                (delete-region fb (- fb 2))
                (setq file (buffer-substring (- fb 2) (- le 2)) last le)
                (beginning-of-line) (insert "  ")
                (puthash file (buffer-substring (- (point) 2) (1+ le)) data)
                (forward-line 1))
              (when last (move-marker (process-mark proc) (1+ last))))))))))

(defsubst dirvish-fd-revert (&rest _)
  "Revert buffer function for fd buffer."
  (dirvish-fd default-directory (or dirvish-fd--input "")))

(defun dirvish-fd-proc-sentinel (proc _)
  "Sentinel for `dirvish-fd' process PROC."
  (when-let* ((buf (process-buffer proc))
              ((buffer-live-p buf))
              (status (process-exit-status proc))
              (took (float-time (time-since (process-get proc 'start)))))
    (unless (eq status 0) (user-error "`fd' exited with status: %s" status))
    (if (< took 1.0)
        (setq took (format "%s ms" (round took 0.001)))
      (setq took (format "%s secs" (/ (round took 0.001) 1000.0))))
    (with-current-buffer buf
      (dirvish-prop :fd-time
        (format " %s %s "
                (propertize "Took:" 'face 'font-lock-doc-face)
                (propertize took 'face 'success)))
      (run-hooks 'dirvish-fd-setup-hook))
    (force-mode-line-update t)))

;;;###autoload
(defun dirvish-fd (dir pattern)
  "Run `fd' on DIR and go into Dired mode on a buffer of the output.
The command run is essentially:

  fd --color=never -0 `dirvish-fd-switches' PATTERN
     --exec-batch `dirvish-fd-ls-program' `dired-listing-switches' --directory."
  (interactive (list (and current-prefix-arg
                          (read-directory-name "Fd target directory: " nil "" t))
                     nil))
  (setq dir (file-name-as-directory
             (expand-file-name (or dir default-directory))))
  (or (file-directory-p dir) (user-error "'fd' requires a directory: %s" dir))
  (let* ((remote (file-remote-p dir))
         (fd-program (dirvish-fd--ensure-fd remote))
         (ls-program (dirvish-fd--find-gnu-ls remote))
         (dv (or (dirvish-curr) (dirvish--get-session) (dirvish--new)))
         (fd-switches (or (dirvish-prop :fd-switches) dirvish-fd-switches ""))
         (ls-switches (or dired-actual-switches (dv-ls-switches dv)))
         (buffer (get-buffer-create "*dirvish-fd*"))
         (root (format dirvish-fd-bufname (or pattern "")
                       (file-name-nondirectory (directory-file-name dir))))
         (bname (concat root (dirvish--timestamp))) process-connection-type proc)
    (with-current-buffer buffer
      (let (buffer-read-only) (erase-buffer))
      (insert "  " dir ":" (make-string (dirvish--subdir-offset) ?\n))
      (dired-mode dir ls-switches)
      (setq-local default-directory dir
                  dired-subdir-alist (list (cons dir (point-min-marker)))
                  dirvish-fd--input (or pattern ""))
      (dirvish--setup-dired #'dirvish-fd-revert)
      (dirvish-prop :fd-cache (dirvish--ht))
      (dirvish-prop :dv (dv-id dv))
      (dirvish-prop :gui (display-graphic-p))
      (dirvish-prop :fd-switches fd-switches)
      (dirvish-prop :cus-header 'dirvish-fd-header)
      (dirvish-prop :remote remote)
      (dirvish-prop :global-header t)
      (dirvish-prop :preview-dps (unless remote (dv-preview-dispatchers dv)))
      (dirvish-prop :attrs (dv-attributes dv))
      (cl-loop for (k v) on dirvish--scopes by 'cddr
               do (dirvish-prop k (and (functionp v) (funcall v))))
      (dirvish-fd--argparser (split-string (or fd-switches "")))
      (dirvish-save-dedication
       (switch-to-buffer buffer) (dirvish--build-layout dv))
      (setq proc (apply #'start-file-process "fd" buffer
                        `(,fd-program "--color=never"
                                      ,@(or (split-string fd-switches) "")
                                      ,(or pattern "")
                                      "--exec-batch" ,ls-program
                                      ,@(or (split-string ls-switches) "")
                                      "--quoting-style=literal" "--directory")))
      (move-marker (process-mark proc) (point) buffer)
      (set-process-filter proc #'dirvish-fd-proc-filter)
      (set-process-sentinel proc #'dirvish-fd-proc-sentinel)
      (set-process-query-on-exit-flag proc nil)
      (process-put proc 'start (float-time))
      (setf (dv-index dv) (cons root buffer))
      (cl-pushnew (cons root buffer) (dv-roots dv) :test #'equal)
      (cl-loop for (_ . b) in (dv-roots dv)
               when (equal (with-current-buffer b (dirvish-prop :root)) root)
               do (dirvish--kill-buffer b))
      (dirvish-prop :root root)
      (rename-buffer bname))))

;;;###autoload
(defun dirvish-fd-ask (dir pattern)
  "The same as `dirvish-fd' but ask initial DIR and PATTERN via prompt."
  (interactive (list (and current-prefix-arg
                          (read-directory-name "Fd target directory: " nil "" t))
                     (read-from-minibuffer "Pattern: ")))
  (dirvish-fd dir pattern))

(provide 'dirvish-fd)
;;; dirvish-fd.el ends here

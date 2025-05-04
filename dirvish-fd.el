;;; dirvish-fd.el --- find-dired alternative using fd  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.3.0
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
  "Find fd programm on a local or REMOTE host ."
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

(defcustom dirvish-fd-header-line-format '(:left (fd-info) :right (fd-status))
  "Header line format for `dirvish-fd'."
  :group 'dirvish :type 'plist)

(defun dirvish-fd--ensure-fd (remote)
  "Return fd executable on REMOTE or localhost.
Raise an error if fd executable is not available."
  (or (and remote (dirvish-fd--find-fd-program remote)) dirvish-fd-program
      (user-error "`dirvish-fd' requires `fd', please install it")))

(defun dirvish-fd--apply-switches ()
  "Apply fd SWITCHES to current buffer."
  (interactive)
  (cl-loop with (re . args) = nil
           for arg in (transient-args transient-current-command)
           if (string-prefix-p "--and=" arg) do (push arg re)
           else do (push arg args)
           finally do (dirvish-fd--argparser re args))
  (revert-buffer))

(transient-define-infix dirvish-fd--extensions-switch ()
  :description "Filter results by file extensions"
  :class 'transient-option
  :argument "--extension="
  :multi-value 'repeat)

(transient-define-infix dirvish-fd--exclude-switch ()
  :description "Exclude files/dirs that match the glob pattern"
  :class 'transient-option
  :argument "--exclude="
  :multi-value 'repeat)

(transient-define-infix dirvish-fd--search-pattern-infix ()
  :description "Change search patterns"
  :class 'transient-option
  :argument "--and="
  :multi-value 'repeat)

;;;###autoload (autoload 'dirvish-fd-switches-menu "dirvish-fd" nil t)
(transient-define-prefix dirvish-fd-switches-menu ()
  "Setup fd switches."
  :init-value (lambda (o) (let ((args (dirvish-prop :fd-info)))
                       (oset o value (append (cadr args) (cddr args)))))
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
    ("RET" "Rerun" dirvish-fd--apply-switches)]])

(defun dirvish-fd--argparser (re args)
  "Parse fd args to a list of flags from ARGS and search regexp RE."
  (let* ((globp (member "--glob" args))
         (casep (member "--case-sensitive" args))
         (ign (cond ((member "--no-ignore" args) "no")
                    ((member "--no-ignore-vcs" args) "no_vcs")
                    (t "all")))
         (status (propertize " ●  " 'face 'dirvish-proc-running))
         comp types exts exc)
    (dolist (arg args)
      (cond ((string-prefix-p "--type=" arg) (push (substring arg 7) types))
            ((string-prefix-p "--extension=" arg) (push (substring arg 12) exts))
            ((string-prefix-p "--exclude=" arg) (push (substring arg 10) exc))))
    (dolist (r re) (push (substring r 6) comp))
    (setq types (mapconcat #'concat types ","))
    (setq exts (mapconcat #'concat exts ","))
    (setq exc (mapconcat #'concat exc ","))
    (setq comp (mapconcat #'concat comp ","))
    (dirvish-prop :fd-info
      (cons (list comp globp casep ign types exts exc status) (cons re args)))))

(dirvish-define-mode-line fd-info
  "Return a formatted string showing the actual fd command line arguments."
  (pcase-let ((`(,re ,globp ,casep ,ign-range ,types ,exts ,excludes ,_)
               (car (dirvish-prop :fd-info)))
              (face (if (dirvish--selected-p) 'dired-header 'dirvish-inactive)))
    (format "  🔍 ⋗ %s [ %s \"%s\" | %s %s | %s %s | %s %s | %s %s | %s %s ]"
            (propertize
             (abbreviate-file-name (directory-file-name default-directory))
             'face 'dired-directory)
            (propertize (if globp "glob:" "regex:") 'face face)
            (propertize (or re "")
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
                        'face 'font-lock-variable-name-face))))

(dirvish-define-mode-line fd-status
  "Status and time took by last fd search."
  (car (last (car (dirvish-prop :fd-info)))))

(defun dirvish-fd--proc-filter (proc string)
  "Filter for output STRING of `dirvish-fd''s process PROC."
  (when-let* (((buffer-name (process-buffer proc)))
              (target (process-get proc 'target)) ((buffer-live-p target)))
    (with-current-buffer target
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (cdar dired-subdir-alist)) (goto-char (dired-subdir-max))
          (cl-loop
           with buffer-read-only = nil
           with (_ regexps case-fold-search) = (dirvish-prop :narrow-info)
           with string = (concat (process-get proc 'tail) string)
           with splits = (split-string string "\n" t)
           with tail = (car (last splits))
           with comp? = (string-suffix-p "\n" string)
           for file in (if comp? splits (butlast splits))
           for f-beg = (string-match " ./" file)
           for f-name = (substring file (+ f-beg 3))
           for f-line = (concat "  " (substring file 0 f-beg) " " f-name "\n")
           do (if (not regexps) (insert f-line)
                (cl-loop for re in regexps
                         unless (string-match re f-name) return nil
                         finally do (insert f-line)))
           finally do (process-put proc 'tail (unless comp? tail))))))))

(defun dirvish-fd--proc-sentinel (proc status)
  "Sentinel for `dirvish-fd' process PROC and its STATUS."
  (when-let* (((buffer-live-p (process-buffer proc)))
              (took (float-time (time-since (process-get proc 'start))))
              (target (process-get proc 'target)) ((buffer-live-p target)))
    (setq took (if (< took 1.0) (format "%s ms" (round took 0.001))
                 (format "%s secs" (/ (round took 0.001) 1000.0))))
    (with-current-buffer target
      (setf (car (last (car (dirvish-prop :fd-info))))
            (cond ((string-prefix-p "killed" status)
                   (propertize " ●  " 'face 'dirvish-proc-failed))
                  ((string-prefix-p "finished" status)
                   (propertize (format "%s ●  " took)
                               'face 'dirvish-proc-finished))
                  (t (propertize " ●  " 'face 'dirvish-proc-failed))))
      (run-hooks 'dirvish-fd-setup-hook))
    (force-mode-line-update t)))

(defun dirvish-fd--start-proc ()
  "Start fd process."
  (let* ((remote (file-remote-p default-directory))
         (fd (dirvish-fd--ensure-fd remote))
         (ls (dirvish-fd--find-gnu-ls remote))
         (fd-args (dirvish-prop :fd-info))
         (buf (get-buffer-create "*dirvish-fd*"))
         process-connection-type proc)
    (when-let* ((op (get-buffer-process buf))) (delete-process op))
    (setq proc (apply #'start-file-process "fd" buf
                      `(,fd "--color=never" ,@(cddr fd-args) ,@(cadr fd-args)
                            "--exec-batch" ,ls
                            ,@(or (split-string dired-actual-switches) "")
                            "--quoting-style=literal" "--directory")))
    (set-process-filter proc #'dirvish-fd--proc-filter)
    (set-process-sentinel proc #'dirvish-fd--proc-sentinel)
    (set-process-query-on-exit-flag proc nil)
    (process-put proc 'start (float-time))
    (process-put proc 'target (current-buffer))))

(defun dirvish-fd-noselect (dv dir pattern)
  "Return the fd buffer for DV at DIR with search PATTERN."
  (let* ((re (mapcan (lambda (x) `(,(format "--and=%s" x)))
                     (if (stringp pattern) (split-string pattern ",") pattern)))
         (ls-switches (or dired-actual-switches (dv-ls-switches dv)))
         (key (file-name-nondirectory (directory-file-name dir)))
         (query (if (stringp pattern) pattern (mapconcat #'concat pattern ",")))
         (buf (get-buffer-create (concat key "🔍" query "🔍" (dv-id dv))))
         (fd (dirvish-prop :fd-info)) (re (or re (cadr fd)))
         (switches (or (cddr fd) (split-string dirvish-fd-switches))))
    (with-current-buffer buf
      (let (buffer-read-only)
        (erase-buffer)
        (insert "  " dir ":" (make-string (dirvish--subdir-offset) ?\n)))
      (unless (derived-mode-p 'dired-mode)
        (let (dired-buffers) (dired-mode dir ls-switches)))
      (setq-local default-directory dir
                  dired-subdir-alist (list (cons dir (point-min-marker))))
      (dirvish-fd--argparser re switches)
      (dirvish-prop :revert
        (lambda (&rest _)
          (setq dired-subdir-alist (list (car (reverse dired-subdir-alist))))
          (let (buffer-read-only)
            (buffer-disable-undo)
            (delete-region (goto-char (dirvish-prop :content-begin)) (point-max)))
          (buffer-enable-undo)
          (dirvish-fd--start-proc)))
      (let* ((fmt dirvish-fd-header-line-format)
             (l (plist-get fmt :left)) (r (plist-get fmt :right)))
        (dirvish-prop :cus-header (dirvish--mode-line-composer l r t)))
      (dirvish-prop :global-header t)
      (dirvish--setup-dired)
      (dirvish-fd--start-proc) buf)))

;;;###autoload
(defun dirvish-fd (dir pattern)
  "Run `fd' on DIR and go into Dired mode on a buffer of the output.
The command run is essentially:

  fd --color=never `dirvish-fd-switches'
     --and PATTERN [--and PATTERN1 --and PATTERN2 … ]
     --exec-batch `dirvish-fd-ls-program' `dired-listing-switches' --directory

If called with \\`C-u', prompt for the target directory,
`default-directory' is used.  If prefixed with \\`C-u' twice, also
prompt for the search regex PATTERN as a comma separated list."
  (interactive (list (and current-prefix-arg
                          (read-directory-name "Fd target directory: " nil "" t))
                     (and (equal current-prefix-arg '(16))
                          (completing-read-multiple "Pattern: " nil))))
  (let* ((dir (or dir default-directory))
         (buf (dirvish-dired-noselect-a nil dir nil (or pattern "")))
         (dv (with-current-buffer buf (dirvish-curr))))
    (dirvish-save-dedication (switch-to-buffer buf) (dirvish--build-layout dv))))

(define-obsolete-function-alias 'dirvish-fd-ask #'dirvish-fd "Apr 4, 2025")

(provide 'dirvish-fd)
;;; dirvish-fd.el ends here

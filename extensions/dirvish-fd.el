;;; dirvish-fd.el --- find-dired alternative using fd  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
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

(defcustom dirvish-fd-regex-builder
  (if (fboundp 'orderless-pattern-compiler)
      #'orderless-pattern-compiler
    #'split-string)
  "Function used to compose the regex list for narrowing.
The function takes the input string as its sole argument and
should return a list of regular expressions."
  :group 'dirvish :type 'function)

(defcustom dirvish-fd-debounce 0.2
  "Like `dirvish-redisplay-debounce', but used for fd input."
  :group 'dirvish :type 'float)

(defcustom dirvish-fd-default-dir "/"
  "Default directory for `dirvish-fd-jump'."
  :group 'dirvish :type 'directory)

(defconst dirvish-fd-bufname "🔍%s📁%s📁%s")
(defconst dirvish-fd-header
  (dirvish--mode-line-fmt-setter '(fd-switches) '(fd-timestamp fd-pwd " ") t))
(defvar dirvish-fd-input-history nil "History list of fd input in the minibuffer.")
(defvar dirvish-fd-debounce-timer nil)
(defvar-local dirvish-fd--output "")
(defvar-local dirvish-fd--input "" "Last used fd user input.")

(defun dirvish-fd--ensure-fd (remote)
  "Return fd executable on REMOTE or localhost.
Raise an error if fd executable is not available."
  (or (and remote (dirvish-fd--find-fd-program remote)) dirvish-fd-program
      (user-error "`dirvish-fd' requires `fd', please install it")))

(defsubst dirvish-fd--header-offset ()
  "Return # of header lines in a fd buffer."
  (if (or (not (boundp 'dired-free-space))
          (eq (bound-and-true-p dired-free-space) 'separate))
      2 1))

(defsubst dirvish-fd--bufname (input dir dv)
  "Return fd buffer name of DV with user INPUT at DIR."
  (format dirvish-fd-bufname (or input "")
          (file-name-nondirectory (directory-file-name dir))
          (dv-name dv)))

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
  :reader (lambda (_prompt _init _hist)
            (completing-read "Input search pattern: "
                             dirvish-fd-input-history nil nil dirvish-fd--input)))

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

(defun dirvish-fd-switch-to-buffer (buffer)
  "Switch to BUFFER with window undedicated."
  (let ((dedicated (window-dedicated-p)) (win (selected-window)))
    (set-window-dedicated-p win nil)
    (prog1 (switch-to-buffer buffer)
      (set-window-dedicated-p win dedicated))))

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
              (face (if (dirvish--window-selected-p dv)
                        'dired-header 'shadow)))
    (format "  %s | %s"
            (propertize "FD" 'face face)
            (if (not (dirvish-prop :fd-time))
                (substitute-command-keys
		 "Processing... press \\[dirvish-fd-kill] to abort the search")
              (format "%s \"%s\" | %s %s | %s %s | %s %s | %s %s | %s %s |"
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
                                  'face 'font-lock-string-face))))))

(dirvish-define-mode-line fd-timestamp
  "Timestamp of search finished."
  (when (car (dv-layout dv)) (dirvish-prop :fd-time)))

(dirvish-define-mode-line fd-pwd
  "Current working directory."
  (propertize (abbreviate-file-name default-directory) 'face 'dired-directory))

(define-obsolete-function-alias 'dirvish-roam #'dirvish-fd-jump "Jun 08, 2022")
(define-obsolete-function-alias 'dirvish-fd-roam #'dirvish-fd-jump "Jul 17, 2022")
;;;###autoload
(defun dirvish-fd-jump (&optional current-dir-p)
  "Browse directories using `fd' command.
This command takes a while to index all the directories the first
time you run it.  After the indexing, it fires up instantly.

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
           (files (dirvish--append-metadata 'file files-raw))
           (file (completing-read "Go to: " files))
           (full-file (concat remote file)))
      (dired-jump nil full-file))))

(defun dirvish-fd-proc-filter (proc string)
  "Filter for `dirvish-fd' processes PROC and output STRING."
  (let ((buf (process-buffer proc)))
    (if (buffer-name buf)
        (with-current-buffer buf
          (setq dirvish-fd--output (concat dirvish-fd--output string)))
      (delete-process proc))))

(defun dirvish-fd--read-input ()
  "Setup INPUT reader for fd."
  (minibuffer-with-setup-hook #'dirvish-fd-minibuffer-setup-h
    (condition-case nil
        (read-string "🔍: " nil dirvish-fd-input-history)
      (quit (prog1 'cancelled (message "Fd search cancelled"))))))

(defun dirvish-fd--parse-output ()
  "Parse fd command output."
  (goto-char (dirvish-prop :content-begin))
  (cl-loop
   with res = () with buffer-read-only = nil
   for file in (split-string dirvish-fd--output "\n" t)
   for idx = (string-match " ./" file)
   for f-name = (substring file (+ idx 3))
   for f-full = (concat "  " (substring file 0 idx) " " f-name "\n") do
   (progn (insert f-full) (push (cons f-name f-full) res))
   finally return (prog1 (nreverse res) (goto-char (point-min)))))

(defun dirvish-fd-find (entry)
  "Run fd accroring to ENTRY."
  (let* ((dv (or dirvish--this (dirvish-curr)))
         (roots (and dv (dv-roots dv)))
         (buf (and roots (alist-get entry roots nil nil #'equal))))
    (or buf
        (pcase-let ((`(,pattern ,dir ,_) (split-string (substring entry 1) "📁")))
          (dirvish-fd dir pattern)))))

(defsubst dirvish-fd-revert (&rest _)
  "Revert buffer function for fd buffer."
  (dirvish-fd default-directory (or dirvish-fd--input "")))

(cl-defun dirvish-fd-proc-sentinel (proc _)
  "Sentinel for `dirvish-fd' process PROC."
  (pcase-let* ((buf (process-buffer proc))
               (success (eq (process-exit-status proc) 0))
               (`(,input ,dir ,dv) (process-get proc 'info)))
    (when (not success)
      (user-error "Dirvish fd error: %s" dirvish-fd--output))
    (unless (buffer-live-p buf)
      (cl-return-from dirvish-fd-proc-sentinel
        (message "`fd' process terminated")))
    (with-selected-window (dv-root-window dv)
      (unless (eq (current-buffer) buf)
        (dirvish-fd-switch-to-buffer buf)))
    (with-current-buffer buf
      (setq-local dirvish-fd--input input
                  dirvish-fd--output (dirvish-fd--parse-output)
                  revert-buffer-function #'dirvish-fd-revert)
      (dirvish-prop :fd-time
        (format " %s %s "
                (propertize "Finished at:" 'face 'font-lock-doc-face)
                (propertize (current-time-string)
                            'face (if success 'success 'error))))
      (cond ((not input) (setq input (dirvish-fd--read-input)))
            (t (dirvish-update-body-h)))
      (when (eq input 'cancelled)
        (cl-return-from dirvish-fd-proc-sentinel (kill-buffer buf)))
      (let ((bufname (dirvish-fd--bufname input dir dv)))
        (dirvish-prop :root bufname)
        (setf (dv-index dv) (cons bufname buf))
        (push (cons bufname buf) (dv-roots dv))
        (dirvish--kill-buffer (get-buffer bufname))
        (rename-buffer bufname)))))

(defun dirvish-fd--narrow (&optional input glob)
  "Filter the subdir with regexs composed from INPUT.
When GLOB, convert the regexs using `dired-glob-regexp'."
  (let ((regexs (cond ((eq (length input) 0) nil)
                      (glob (mapcar #'dired-glob-regexp
                                    (funcall dirvish-fd-regex-builder input)))
                      (t (funcall dirvish-fd-regex-builder input))))
        buffer-read-only)
    (goto-char (cdar dired-subdir-alist))
    (forward-line (dirvish-fd--header-offset))
    (dirvish-prop :content-begin (point))
    (delete-region (point) (dired-subdir-max))
    (save-excursion
      (if (not regexs)
          (cl-loop for (_ . line) in dirvish-fd--output do (insert line))
        (cl-loop for (file . line) in dirvish-fd--output
                 unless (cl-loop for regex in regexs
                                 thereis (not (string-match regex file)))
                 do (insert line))))
    (dirvish-update-body-h)))

(defun dirvish-fd-minibuffer-update-h ()
  "Minibuffer update function for `dirvish-fd'."
  (dirvish-debounce fd
    (let* ((buf (window-buffer (minibuffer-selected-window)))
           (input (minibuffer-contents-no-properties)))
      (with-current-buffer buf
        (setq dirvish-fd--input input)
        (dirvish-fd--narrow input (car (dirvish-prop :fd-arglist)))))))

(defun dirvish-fd-minibuffer-setup-h ()
  "Minibuffer setup function for `dirvish-fd'."
  (add-hook 'post-command-hook #'dirvish-fd-minibuffer-update-h nil t))

(defun dirvish-fd-kill ()
  "Kill the `fd' process running in the current buffer."
  (interactive)
  (dirvish--kill-buffer (current-buffer)))

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
  (or (file-directory-p dir)
      (user-error "'fd' command requires a directory: %s" dir))
  (let* ((remote (file-remote-p dir))
         (fd-program (dirvish-fd--ensure-fd remote))
         (ls-program (or (and remote (dirvish-fd--find-gnu-ls remote))
                         dirvish-fd-ls-program))
         (dv (or (dirvish-curr) (progn (dirvish dir) dirvish--this)))
         (fd-switches (or (dirvish-prop :fd-switches) dirvish-fd-switches ""))
         (ls-switches (or dired-actual-switches (dv-ls-switches dv)))
         (buffer (dirvish--util-buffer 'fd dv nil t)))
    (dirvish--kill-buffer (get-buffer (dirvish-fd--bufname pattern dir dv)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "  " dir ":" (make-string (dirvish-fd--header-offset) ?\n))
      (dired-mode dir ls-switches)
      (setq-local default-directory dir
                  dired-subdir-alist (list (cons dir (point-min-marker))))
      (dirvish-init-dired-buffer)
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map (current-local-map))
        (define-key map "\C-c\C-k" #'dirvish-fd-kill)
        (use-local-map map))
      (dirvish-prop :dv (dv-name dv))
      (dirvish-prop :gui (display-graphic-p))
      (dirvish-prop :fd-switches fd-switches)
      (dirvish-prop :cus-header 'dirvish-fd-header)
      (dirvish-prop :remote remote)
      (dirvish-prop :global-header t)
      (let ((proc (apply #'start-file-process
                         "fd" buffer
                         `(,fd-program "--color=never"
                           ,@(or (split-string fd-switches) "")
                           ,(or pattern "")
                           "--exec-batch" ,ls-program
                           ,@(or (split-string ls-switches) "")
                           "--quoting-style=literal" "--directory"))))
        (set-process-filter proc #'dirvish-fd-proc-filter)
        (set-process-sentinel proc #'dirvish-fd-proc-sentinel)
        (dirvish-fd--argparser (split-string (or fd-switches "")))
        (process-put proc 'info (list pattern dir dv))))
    (dirvish-fd-switch-to-buffer buffer)))


;;;###autoload
(defun dirvish-fd-ask (dir pattern)
  "The same as `dirvish-fd' but ask initial `pattern' via prompt. "
  (interactive (list (and current-prefix-arg
                          (read-directory-name "Fd target directory: " nil "" t))
                     (read-from-minibuffer "Pattern: ")))
  (dirvish-fd dir pattern))

(provide 'dirvish-fd)
;;; dirvish-fd.el ends here

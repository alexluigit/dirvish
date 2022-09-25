;;; dirvish-yank.el --- Multi-stage and async copy/paste/link utilities -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Multi-stage and asynchronous copy/paste/link utilities in Dirvish.

;; With the multi-stage operations, you can gather files from multiple Dired
;; buffers into a single "clipboard", then copy or move all of them to the
;; target location.

;; Here are the available commands:
;; Note that they are asynchronous and work on both localhost and remote host.
;; - `dirvish-yank'
;; - `dirvish-move'
;; - `dirvish-symlink'
;; - `dirvish-relative-symlink'
;; - `dirvish-hardlink'
;; - `dirvish-rsync' (requires 'rsync' executable)

;;; Code:

(require 'dired-aux)
(require 'dirvish)
(require 'tramp)

(defcustom dirvish-yank-sources 'all
  "The way to collect source files.
The value can be a symbol or a function that returns a fileset."
  :group 'dirvish
  :type '(choice (const :tag "Marked files in current buffer" buffer)
                 (const :tag "Marked files in current session" session)
                 (const :tag "Marked files in all Dired buffers" all)
                 (function :tag "Custom function")))

(defcustom dirvish-yank-auto-unmark t
  "Control if yank commands should unmark when complete."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-yank-overwrite-existing-files 'ask
  "Whether to overwrite existing files when calling yank commands."
  :group 'dirvish
  :type '(choice (const :tag "prompt for confirmation" ask)
                 (const :tag "always overwrite" always)
                 (const :tag "never overwrite, skip transferring this file" skip)
                 (const :tag "never overwrite, create new file instead" never)))

(defcustom dirvish-yank-new-name-style 'append-to-ext
  "Control the way to compose new filename."
  :group 'dirvish
  :type '(choice (const :tag "append INDEX~ to file extension" append-to-ext)
                 (const :tag "append INDEX~ to file name" append-to-filename)
                 (const :tag "prepend INDEX~ to file name" prepend-to-filename)))

(defcustom dirvish-yank-rsync-program "rsync"
  "The rsync binary that we are going to use."
  :type 'string :group 'dirvish)

(defcustom dirvish-yank-rsync-args "-avz --info=progress2"
  "The default options for the rsync command."
  :type 'string :group 'dirvish)

;;;###autoload (autoload 'dirvish-yank-menu "dirvish-yank" nil t)
(defcustom dirvish-yank-keys
  '(("y" "Yank (paste) here"           dirvish-yank)
    ("m" "Move here"                   dirvish-move)
    ("s" "Make symlinks here"          dirvish-symlink)
    ("r" "Make relative symlinks here" dirvish-relative-symlink)
    ("h" "Make hardlinks here"         dirvish-hardlink))
  "YANK-KEYs for command `dirvish-yank-menu'.
A YANK-KEY is a (KEY DOC CMD) alist where KEY is the key to
invoke the CMD, DOC is the documentation string."
  :group 'dirvish :type 'alist
  :set
  (lambda (k v)
    (set k v)
    (eval
     `(transient-define-prefix dirvish-yank-menu ()
        "Yank commands menu."
        [:description
         (lambda () (dirvish--format-menu-heading "Select yank operation on marked files:"))
         ,@v]
        (interactive)
        (if (derived-mode-p 'dired-mode)
            (transient-setup 'dirvish-yank-menu)
          (user-error "Not in a Dirvish buffer"))))))

(defconst dirvish-yank-fn-string
  '((dired-copy-file . "Copying")
    (dired-rename-file . "Moving")
    (dired-hardlink . "Hardlink")
    (make-symbolic-link . "Symlink")
    (dired-make-relative-symlink . "Relative symlink")
    (rsync . "Rsync")))
(defvar dirvish-yank-log-buffers nil)
;; copied from `dired-async' and `dired-rsync'
(defconst dirvish-yank-env-variables-regexp
  "\\`\\(tramp-\\(default\\|connection\\|remote\\)\\|ange-ftp\\)-.*"
  "Variables matching this regexp will be loaded on Child Emacs.")
(defvar dirvish-passphrase-stall-regex "Enter passphrase for key"
  "A regex to detect passphrase prompts.")
(defvar dirvish-percent-complete-regex "[[:digit:]]\\{1,3\\}%"
  "A regex to extract the % complete from a file.")
(defvar dirvish-yank--remote-portfwd
  "ssh -p %d -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"
  "An explicit ssh command for rsync to use port forwarded proxy.
The string is treated as a format string where %d is replaced with the
results of `dirvish-yank--get-remote-port'.")

(defun dirvish-yank--get-remote-port ()
  "Return the remote port we shall use for the reverse port-forward."
  (+ 50000 (length dirvish-yank-log-buffers)))

(defun dirvish-yank--get-srcs (&optional range)
  "Get all marked filenames in RANGE.
RANGE can be `buffer', `session', `frame', `all'."
  (setq range (or range 'buffer))
  (cl-remove-duplicates
   (cl-loop
    with case-fold-search = nil
    with regexp = (dired-marker-regexp)
    with buffers = (pcase range
                     ('buffer (list (current-buffer)))
                     ('session (mapcar #'cdr (dv-roots (dirvish-curr))))
                     ('all (cl-loop for b in (buffer-list)
                                    when (with-current-buffer b
                                           (eq major-mode 'dired-mode))
                                    collect b)))
    for buffer in (seq-filter #'buffer-live-p buffers) append
    (with-current-buffer buffer
      (when (save-excursion (goto-char (point-min))
                            (re-search-forward regexp nil t))
        (dired-map-over-marks (dired-get-filename) nil))))
   :test #'equal))

(defun dirvish-yank--read-dest (method &optional force)
  "Read dest dir for METHOD when `current-prefix-arg' or FORCE."
  (list (when (or current-prefix-arg force)
          (read-file-name (format "%s files to: " method)
                          (dired-dwim-target-directory)
                          nil nil nil 'file-directory-p))))

(defun dirvish-yank--filename-for-rsync (file)
  "Reformat a tramp FILE to one usable for rsync."
  (if (tramp-tramp-file-p file)
      (with-parsed-tramp-file-name file tfop
        (format "%s%s:%s" (if tfop-user (format "%s@" tfop-user) "") tfop-host
                (shell-quote-argument tfop-localname)))
    (shell-quote-argument file)))

(defun dirvish-yank-proc-sentinel (proc _exit)
  "Sentinel for yank task PROC."
  (pcase-let ((proc-buf (process-buffer proc))
              (`(,buffer) (process-get proc 'details))
              (success (eq (process-exit-status proc) 0)))
    (setq dirvish-yank-log-buffers (remove proc-buf dirvish-yank-log-buffers))
    (if success (kill-buffer proc-buf) (pop-to-buffer proc-buf))
    (when (eq buffer (current-buffer))
      (with-current-buffer buffer
        (revert-buffer) (dirvish-update-body-h)))))

(defun dirvish-yank-proc-filter (proc string)
  "Filter for yank task PROC's STRING."
  (let ((proc-buf (process-buffer proc)))
    (when (string-match dirvish-passphrase-stall-regex string) ; check for prompt
      (process-send-string proc (concat (read-passwd string) "\n")))
    (let ((old-process-mark (process-mark proc)))
      (when (buffer-live-p proc-buf)
        (with-current-buffer proc-buf
          (when (string-match dirvish-percent-complete-regex string)
            (dirvish-prop :yank-percent (match-string 0 string))
            (force-mode-line-update t))
          (let ((moving (= (point) old-process-mark)))
            (save-excursion
              (goto-char old-process-mark)
              (insert string)
              (set-marker (process-mark proc) (point)))
            (if moving (goto-char (process-mark proc)))))))))

(defun dirvish-yank--execute (cmd details &optional batch)
  "Execute CMD, put DETAILS into the process.
When BATCH, execute the command using `emacs -q -batch'."
  (pcase-let* ((process-connection-type nil) (name "*dirvish-yank*")
               (buf (dirvish--util-buffer
                     (format "yank@%s" (current-time-string)) nil nil t))
               (`(,_ ,_ ,dest ,_) details)
               (proc (if batch
                         (let* ((q (if (file-remote-p dest) "-q" "-Q"))
                                (c (list "emacs" q "-batch" "--eval" cmd)))
                           (make-process :name name :buffer buf :command c))
                       (start-process-shell-command name buf cmd))))
    (with-current-buffer buf (dirvish-prop :yank-details details))
    (process-put proc 'details details)
    (set-process-sentinel proc #'dirvish-yank-proc-sentinel)
    (set-process-filter proc #'dirvish-yank-proc-filter)
    (when dirvish-yank-auto-unmark
      (cl-loop for buf in (buffer-list)
               do (with-current-buffer buf
                    (when (eq major-mode 'dired-mode)
                      (dired-unmark-all-marks)))))
    (push buf dirvish-yank-log-buffers)))

(defun dirvish-yank--newbase (base-name fileset dest)
  "Ensure an unique filename for BASE-NAME at DEST with FILESET."
  (let ((bname~ base-name) (idx 1))
    (while (member bname~ fileset)
      (setq bname~
            (pcase dirvish-yank-new-name-style
              ('append-to-ext (format "%s%s~" base-name idx))
              ('append-to-filename
               (format "%s%s~.%s"
                       (file-name-sans-extension base-name)
                       idx (file-name-extension base-name)))
              ('prepend-to-filename (format "%s~%s" idx base-name)))
            idx (1+ idx)))
    (cons (expand-file-name base-name dest) (expand-file-name bname~ dest))))

(defun dirvish-yank--new-srcs (srcs dest)
  "Generate new unique file name pairs from SRCS and DEST."
  (cl-loop
   with overwrite = (eq dirvish-yank-overwrite-existing-files 'always)
   with never = (eq dirvish-yank-overwrite-existing-files 'never)
   with skip = (eq dirvish-yank-overwrite-existing-files 'skip)
   with (skipped to-rename) = ()
   with dfiles = (directory-files dest nil nil t)
   with fmt = "Overwrite `%s'? (y)es (n)o (s)kip (q)uit (Y)es-to-all (N)o-to-all (S)kip-all"
   for src in srcs
   for base = (file-name-nondirectory src)
   for collision = (member base dfiles) do
   (cond ((equal src (concat dest base))
          (user-error "Source and target are the same file `%s'" src))
         (overwrite nil)
         ((and never collision)
          (push (dirvish-yank--newbase base dfiles dest) to-rename))
         ((and skip collision) (push src skipped))
         (collision
          (cl-case (read-char-choice (format fmt base) '(?y ?Y ?n ?N ?q))
            (?y nil)
            (?n (push (dirvish-yank--newbase base dfiles dest) to-rename))
            (?s (push src skipped))
            (?Y (setq overwrite t))
            (?N (setq never t)
                (push (dirvish-yank--newbase base dfiles dest) to-rename))
            (?S (push src skipped) )
            (?q (user-error "Dirvish[info]: yank task aborted")))))
   finally return
   (progn (cl-loop for (from . to) in to-rename do (rename-file from to))
          (cl-set-difference srcs skipped :test #'equal))))

(defun dirvish-yank-inject-env (include-regexp)
  "Return a `setq' form that replicates part of the calling environment.
It sets the value for every variable matching INCLUDE-REGEXP."
  `(setq ,@(let (bindings)
             (mapatoms
              (lambda (sym)
                (let* ((sname (and (boundp sym) (symbol-name sym)))
                       (value (and sname (symbol-value sym))))
                  (when (and sname (string-match include-regexp sname)
                             (not (string-match "-syntax-table\\'" sname)))
                    (unless (or (stringp value) (memq value '(nil t))
                                (numberp value) (vectorp value))
                      (setq value `(quote ,value)))
                    (setq bindings (cons value bindings)
                          bindings (cons sym bindings))))))
             bindings)))

;; Thanks to `dired-rsync.el'
;; also see: https://unix.stackexchange.com/questions/183504/how-to-rsync-files-between-two-remotes
(defun dirvish-yank-r2r-handler (srcs dest shost dhost)
  "Construct and trigger an rsync run for remote copy.
This command sync SRCS on SHOST to DEST on DHOST."
  (let* ((duser (with-parsed-tramp-file-name dest tfop
                  (or tfop-user (getenv "USER"))))
         (port (dirvish-yank--get-remote-port))
         (dest (shell-quote-argument (file-local-name dest)))
         (rsync-cmd
          (format "\"%s %s -e \\\"%s\\\" %s %s@localhost:%s\""
                  dirvish-yank-rsync-program
                  dirvish-yank-rsync-args
                  (format dirvish-yank--remote-portfwd port)
                  (string-join srcs " ") duser dest))
         (bind-addr (format "localhost:%d:%s:22" port dhost))
         (cmd (string-join
               (list "ssh" "-A" "-R" bind-addr shost rsync-cmd) " ")))
    (dirvish-yank--execute cmd (list (current-buffer) srcs dest 'rsync))))

(defun dirvish-yank-l2fr-handler (srcs dest)
  "Execute a local to/from remote rsync command for SRCS and DEST."
  (let* ((srcs (mapcar #'dirvish-yank--filename-for-rsync srcs))
         (dest (dirvish-yank--filename-for-rsync dest))
         (rsync-cmd (flatten-tree (list dirvish-yank-rsync-program
                                        dirvish-yank-rsync-args srcs dest)))
         (cmd (string-join rsync-cmd " ")))
    (dirvish-yank--execute cmd (list (current-buffer) srcs dest 'rsync))))

(defun dirvish-yank-default-handler (method srcs dest)
  "Execute a local yank command with type of METHOD.
SRCS and DEST have to be in the same HOST (local or remote)."
  (let* ((srcs (dirvish-yank--new-srcs srcs dest))
         (cmd `(prog1 (require 'dired-aux)
                 ,(dirvish-yank-inject-env dirvish-yank-env-variables-regexp)
                 (cl-loop
                  with srcs = '(,@srcs) with count = (float (length srcs))
                  with dired-recursive-copies = 'always
                  with dired-copy-preserve-time = ,dired-copy-preserve-time
                  for idx from 1 for src in srcs
                  for percent = (if (eq (float idx) count) 100
                                  (floor (* (/ idx count) 100)))
                  do (progn (message "%s%%" percent)
                            (condition-case err
                                (funcall #',method src ,dest t)
                              (file-error
                               (message "%s: %s\n" (car err) (cdr err)) nil)))
                  finally (cl-loop for b in (buffer-list) thereis
                                   (and (string-match "\\`\\*ftp.*"
                                                      (buffer-name b))
                                        (prog1 b (kill-buffer b))))))))
    (dirvish-yank--execute
     (format "%S" cmd) (list (current-buffer) srcs dest method) 'batch)))

;; copied from `dired-rsync'
(defun dirvish-yank--extract-host-from-tramp (file-or-path &optional split-user)
  "Extract the tramp host part of FILE-OR-PATH.
It SPLIT-USER is set we remove the user@ part as well.  We assume
hosts don't need quoting."
  (with-parsed-tramp-file-name file-or-path tfop
    (if (or split-user (not tfop-user)) tfop-host
      (format "%s@%s" tfop-user tfop-host))))

(defun dirvish-yank--extract-remote (files)
  "Get string identifying the remote connection of FILES."
  (cl-loop with hosts = () for f in files for h = (file-remote-p f)
           do (cl-pushnew h hosts :test #'equal)
           when (> (length hosts) 1)
           do (user-error "Dirvish[error]: SOURCEs need to be in the same host")
           finally return (car hosts)))

(defun dirvish-yank--apply (method dest)
  "Apply yank METHOD to DEST."
  (setq dest (expand-file-name (or dest (dired-current-directory))))
  (let ((srcs (or (and (functionp dirvish-yank-sources)
                       (funcall dirvish-yank-sources))
                  (dirvish-yank--get-srcs dirvish-yank-sources)
                  (user-error "Dirvish[error]: no marked files"))))
    (dirvish-yank-default-handler method srcs dest)))

(dirvish-define-mode-line yank
  "Progress of yank tasks."
  (let ((number-of-tasks (length dirvish-yank-log-buffers)))
    (cond ((= number-of-tasks 0))
          ((= number-of-tasks 1)
           (pcase-let* ((buf (car dirvish-yank-log-buffers))
                        (`(,_ ,srcs ,dest ,method)
                         (with-current-buffer buf (dirvish-prop :yank-details)))
                        (percent (with-current-buffer buf
                                   (dirvish-prop :yank-percent)))
                        (count (length srcs)))
             (format "%s%s: %s ⇛ %s "
                     (propertize
                      (format "%s" (alist-get method dirvish-yank-fn-string))
                      'face 'font-lock-constant-face)
                     (if (not percent) ""
                       (propertize (format " [ %s%%%%%%%%  ] " percent)
                                   'face 'success))
                     (propertize
                      (if (= count 1) (car srcs) (format "%s files" count))
                      'face 'font-lock-keyword-face)
                     (propertize dest 'face 'font-lock-doc-face))))
          ((> number-of-tasks 1)
           (format " %s %s%s "
                   (propertize (number-to-string number-of-tasks)
                               'face 'font-lock-keyword-face)
                   (propertize "running yank task" 'face 'font-lock-doc-face)
                   (propertize (if (> number-of-tasks 1) "s" "")
                               'face 'font-lock-doc-face))))))

;;;###autoload
(defun dirvish-yank (&optional dest)
  "Paste marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it
defaults to `dired-current-directory.'"
  (interactive (dirvish-yank--read-dest 'yank))
  (dirvish-yank--apply 'dired-copy-file dest))

;;;###autoload
(defun dirvish-move (&optional dest)
  "Move marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it
defaults to `dired-current-directory'."
  (interactive (dirvish-yank--read-dest 'move))
  (dirvish-yank--apply 'dired-rename-file dest))

;;;###autoload
(defun dirvish-symlink (&optional dest)
  "Symlink marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it
defaults to `dired-current-directory'."
  (interactive (dirvish-yank--read-dest 'symlink))
  (dirvish-yank--apply 'make-symbolic-link dest))

;;;###autoload
(defun dirvish-relative-symlink (&optional dest)
  "Similar to `dirvish-symlink', but link files relatively.
Prompt for DEST when prefixed with \\[universal-argument], it
defaults to `dired-current-directory'."
  (interactive (dirvish-yank--read-dest 'relalink))
  (dirvish-yank--apply 'dired-make-relative-symlink dest))

;;;###autoload
(defun dirvish-hardlink (&optional dest)
  "Hardlink marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it
defaults to `dired-current-directory'."
  (interactive (dirvish-yank--read-dest 'hardlink))
  (dirvish-yank--apply 'dired-hardlink dest))

;;;###autoload
(defun dirvish-rsync (dest)
  "Rsync marked files to DEST, prompt for DEST if not called with.
If either the sources or the DEST is located in a remote host,
the `dirvish-yank-rsync-program' and `dirvish-yank-rsync-args'
are used to transfer the files.

This command requires proper ssh authentication setup to work
correctly for file transfer involving remote hosts, because rsync
command is always run locally, the password prompts may lead to
unexpected errors."
  (interactive (dirvish-yank--read-dest 'rsync t))
  (setq dest (expand-file-name (or dest (dired-current-directory))))
  (let* ((dvec (and (tramp-tramp-file-p dest) (tramp-dissect-file-name dest)))
         (srcs (or (and (functionp dirvish-yank-sources)
                        (funcall dirvish-yank-sources))
                   (dirvish-yank--get-srcs dirvish-yank-sources)
                   (user-error "Dirvish[error]: no marked files")))
         (src-0 (prog1 (car srcs) (dirvish-yank--extract-remote srcs)))
         (svec (and (tramp-tramp-file-p src-0) (tramp-dissect-file-name src-0))))
    (cond
     ;; shost and dhost are different remote hosts
     ((and svec dvec (not (tramp-local-host-p svec))
           (not (tramp-local-host-p dvec)))
      (dirvish-yank-r2r-handler
       srcs dest (dirvish-yank--extract-host-from-tramp src-0)
       (dirvish-yank--extract-host-from-tramp dest t)))
     ;; either shost or dhost is localhost
     ((or (and (not svec) dvec (not (tramp-local-host-p dvec)))
          (and (not dvec) svec (not (tramp-local-host-p svec))))
      (dirvish-yank-l2fr-handler srcs dest))
     ;; using default handler
     (t (dirvish-yank-default-handler 'dired-copy-file srcs dest)))))

(provide 'dirvish-yank)
;;; dirvish-yank.el ends here

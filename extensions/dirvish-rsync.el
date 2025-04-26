;;; dirvish-rsync.el --- Rsync integration for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.3.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This extension introduces `dirvish-rsync' command (which requires `rsync'
;; executable), mirroring the functionality of Alex Bennée's `dired-rsync'.
;; Uniquely, `dirvish-rsync' gathers marked files from multiple Dired buffers.
;; It also provides a transient menu `dirvish-rsync-switches-menu', for
;; temporary adjustments to `dirvish-rsync-args'.

;;; Code:

(require 'dirvish-yank)
(require 'tramp)

(define-obsolete-variable-alias 'dirvish-yank-rsync-program 'dirvish-rsync-program "Fed 9, 2025")
(defcustom dirvish-rsync-program "rsync"
  "The rsync binary that we are going to use."
  :type 'string :group 'dirvish)

(define-obsolete-variable-alias 'dirvish-yank-rsync-args 'dirvish-rsync-args "Fed 9, 2025")
(defcustom dirvish-rsync-args
  '("--archive" "--verbose" "--compress" "--info=progress2")
  "The default options for the rsync command."
  :type '(repeat string) :group 'dirvish)

(defcustom dirvish-rsync-r2r-ssh-port "22"
  "Default ssh port of receiver when yanking in remote to remote scenario.
In this scenario rsync will be run on remote host, so it has no access
to your ~/.ssh/config file.  If you have some settings there you have to
specify them somehow.  One way is to set global default values and other
way is to set them locally before copying, using rsync-transient menu."
  :type 'string :group 'dirvish)

(defcustom dirvish-rsync-r2r-ssh-user nil
  "Default ssh user of receiver when yanking in remote to remote scenario.
When it is nil, do not specify any user.  See
`dirvish-rsync-r2r-ssh-port' for more details."
  :type '(choice string (const nil)) :group 'dirvish)

(defcustom dirvish-rsync-r2r-use-direct-connection nil
  "When t, copy data directly from host1 to host2.
If this is not possible, for example when host2 is not reacheable from
host1 set this option to nil.  When it is nil the tunnel will be created
between host1 and host2, using running machine as proxy.  For both cases
make sure that you have passwordless access to both hosts and that
ssh-agent is properly set-up.  For checking that, everything works try
to execute a command \"ssh -A host1 ssh -o StrictHostKeyChecking=no
host2 hostname\".  Also make sure that ssh-agent Environment variables
are propagated to Emacs."
  :type 'boolean :group 'dirvish)

(defcustom dirvish-rsync-shortcut-key-for-yank-menu "R"
  "A shortcut key added to `dirvish-yank-menu'."
  :type 'string :group 'dirvish)

(defcustom dirvish-rsync-use-yank-menu t
  "When t, append a shortcut to invoke `dirvish-rsync' in `dirvish-yank-menu'.
The shortcut key is denoted by `dirvish-rsync-shortcut-key-for-yank-menu'."
  :type 'boolean :group 'dirvish
  :set (lambda (k v)
         (set k v)
         (if v (dirvish-yank--menu-setter
                nil (append dirvish-yank-keys
                            `((,dirvish-rsync-shortcut-key-for-yank-menu
                               "Rsync here" dirvish-rsync))))
           (dirvish-yank--menu-setter nil dirvish-yank-keys))))

(defvar dirvish-rsync--remote-ssh-args
  "-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"
  "These args will be used for invoking ssh on remote host (in r2r case).")
(defvar dirvish-rsync--transient-input-history nil
  "History list of rsync transient input in the minibuffer.")
(defvar crm-separator)

(defvar-local dirvish-rsync--r2r-direct-conn nil
  "Local value for enabling direct copy in r2r case.")
(defvar-local dirvish-rsync--r2r-ssh-recv-host nil
  "Local value of r2r receiver host.")
(defvar-local dirvish-rsync--r2r-ssh-recv-port nil
  "Local value of r2r receiver port.")
(defvar-local dirvish-rsync--r2r-ssh-recv-user nil
  "Local value of r2r receiver user.")

(defun dirvish-rsync--get-remote-host ()
  "Return the remote port we shall use for the reverse port-forward."
  (+ 50000 (length dirvish-yank-log-buffers)))

(defun dirvish-rsync--filename (file)
  "Reformat a tramp FILE to one usable for rsync."
  (if (tramp-tramp-file-p file)
      (with-parsed-tramp-file-name file tfop
        (format "%s%s:%s" (if tfop-user (format "%s@" tfop-user) "") tfop-host
                (shell-quote-argument tfop-localname)))
    (shell-quote-argument file)))

(defun dirvish-rsync--compose-command ()
  "Compose rsync command and args into the string.
Retrieve rsync args from current session or `dirvish-rsync-args'."
  (format "%s %s"
          dirvish-rsync-program
          (string-join
           (or (dirvish-prop :rsync-switches) dirvish-rsync-args) " ")))

(defun dirvish-rsync--local-ssh-args (host-info)
  "Compose ssh args used for sshing to source host.
HOST-INFO is a list of host/user/port parsed from the tramp string."
  (let* ((port (cl-third host-info))
         (port-str (if port (concat "-p" port) ""))
         (user (cl-second host-info))
         (user-str (if user (concat user "@") "")))
    (concat port-str " " user-str (cl-first host-info))))

(defun dirvish-rsync--r2r-escape-single-quote (str)
  "Properly escape all single quotes in STR.
STR should be processed by `shell-quote-argument' already.  Single
quotes require special care since we wrap remote command with them.
Bash doesn't allow nesting of single quotes (even escaped ones), so we
need to turn string into multiple concatenated strings."
  ;; use string-replace from emacs-28.1 when support of older versions is dropped
  (replace-regexp-in-string "'" "'\"'\"'" str t t))

;; Thanks to `dired-rsync.el'
;; also see: https://unix.stackexchange.com/questions/183504/how-to-rsync-files-between-two-remotes
(defun dirvish-rsync--r2r-handler (srcs shost-info dhost-info)
  "Construct and trigger an rsync run for remote copy.
This command sync SRCS on SHOST to DEST on DHOST.  SHOST-INFO and
DHOST-INFO are lists containing host,user,port,localname extracted from
the tramp string."
  (let* ((srcs (mapcar (lambda (x)
                         (thread-last x file-local-name shell-quote-argument
                                      dirvish-rsync--r2r-escape-single-quote))
                       srcs))
         (src-str (string-join srcs " "))
         (shost (cl-first shost-info))
         (dhost (cl-first dhost-info))
         (dhost-real (or dirvish-rsync--r2r-ssh-recv-host
                         (cl-first dhost-info)))
         (duser (or dirvish-rsync--r2r-ssh-recv-user
                    (cl-second dhost-info)
                    dirvish-rsync-r2r-ssh-user))
         (dport (or dirvish-rsync--r2r-ssh-recv-port
                    (cl-third dhost-info)
                    dirvish-rsync-r2r-ssh-port))
         (dest (thread-last (cl-fourth dhost-info)
                            shell-quote-argument
                            dirvish-rsync--r2r-escape-single-quote))
         ;; 1. dhost == shost
         ;; ssh [-p dport] [duser@]dhost 'rsync <rsync-args> <srcs> <dest>'
         ;; 2. dhost != shost and `dirvish-rsync-r2r-use-direct-connection' == t
         ;; ssh -A [-p sport] [suser@]shost 'rsync <rsync-args> -e "ssh <ssh-remote-opts> [-p dport]" <srcs> [duser@]dhost:<dest> '
         ;; 3. dhost != shost and `dirvish-rsync-r2r-use-direct-connection' == nil
         ;; ssh -A -R <bind-addr> [-p sport] [suser@]shost 'rsync <rsync-args> -e "ssh <ssh-remote-opts> -p <tunnel_port>" <srcs> [duser@]localhost:<dest>'
         (cmd (cond ((equal shost dhost)
                     (string-join
                      (list "ssh"
                            (dirvish-rsync--local-ssh-args dhost-info)
                            "'"
                            (dirvish-rsync--compose-command)
                            src-str dest "'")
                      " "))
                    ((if dirvish-rsync--r2r-direct-conn
                         (equal dirvish-rsync--r2r-direct-conn "yes")
                       dirvish-rsync-r2r-use-direct-connection)
                     (string-join
                      (list "ssh -A "
                            (dirvish-rsync--local-ssh-args shost-info)
                            " '" (dirvish-rsync--compose-command)
                            (format " -e \"ssh %s %s\" "
                                    (if dport (concat "-p" dport) "")
                                    dirvish-rsync--remote-ssh-args)
                            src-str " "
                            (if duser
                                (format "%s@%s" duser dhost-real)
                              dhost-real)
                            ":" dest "'")))
                    (t (let* ((port (dirvish-rsync--get-remote-host))
                              (bind-addr (format "localhost:%d:%s:%s"
                                                 port dhost-real dport)))
                         (string-join
                          (list "ssh -A -R " bind-addr " "
                                (dirvish-rsync--local-ssh-args shost-info)
                                " '" (dirvish-rsync--compose-command)
                                (format " -e \"ssh -p %s %s\" "
                                        port dirvish-rsync--remote-ssh-args)
                                src-str
                                " "
                                (if duser
                                    (format "%s@localhost" duser)
                                  "localhost")
                                ":" dest "'")))))))
    (dirvish-yank--execute cmd (list (current-buffer) srcs dest 'rsync))))

(defun dirvish-rsync--l2fr-handler (srcs dest)
  "Execute a local to/from remote rsync command for SRCS and DEST."
  (let* ((srcs (mapcar #'dirvish-rsync--filename srcs))
         (dest (dirvish-rsync--filename dest))
         (rsync-cmd (flatten-tree (list (dirvish-rsync--compose-command)
                                        srcs dest)))
         (cmd (string-join rsync-cmd " ")))
    (dirvish-yank--execute cmd (list (current-buffer) srcs dest 'rsync))))

;; copied from `dired-rsync'
(defun dirvish-rsync--extract-host-from-tramp (file-or-path)
  "Extract the tramp host part of FILE-OR-PATH.
Returns list that contains (host user port localname)."
  (with-parsed-tramp-file-name file-or-path tfop
    (when tfop-hop
      (user-error "DIRVISH[rsync]: Paths with hop are not supported!"))
    (list tfop-host tfop-user tfop-port tfop-localname)))

(defun dirvish-rsync--extract-remote (files)
  "Get string identifying the remote connection of FILES."
  (cl-loop with hosts = () for f in files for h = (file-remote-p f)
           do (cl-pushnew h hosts :test #'equal)
           when (> (length hosts) 1)
           do (user-error "DIRVISH[rsync]: SOURCEs need to be in the same host")
           finally return (car hosts)))

;;;###autoload
(defun dirvish-rsync (dest)
  "Rsync marked files to DEST, prompt for DEST if not called with.
If either the sources or the DEST is located in a remote host, the
`dirvish-rsync-program' and `dirvish-rsync-args' are used to transfer
the files.

This command requires proper ssh authentication setup to work correctly
for file transfer involving remote hosts, because rsync command is
always run locally, the password prompts may lead to unexpected errors."
  (interactive (dirvish-yank--read-dest 'rsync))
  (setq dest (expand-file-name (or dest (dired-current-directory))))
  (let* ((dvec (and (tramp-tramp-file-p dest) (tramp-dissect-file-name dest)))
         (srcs (or (and (functionp dirvish-yank-sources)
                        (funcall dirvish-yank-sources))
                   (dirvish-yank--get-srcs dirvish-yank-sources)
                   (user-error "DIRVISH[rsync]: no marked files")))
         (src-0 (prog1 (car srcs) (dirvish-rsync--extract-remote srcs)))
         (svec (and (tramp-tramp-file-p src-0) (tramp-dissect-file-name src-0))))
    (cond
     ;; shost and dhost are different remote hosts
     ((and svec dvec (not (tramp-local-host-p svec))
           (not (tramp-local-host-p dvec)))
      (dirvish-rsync--r2r-handler
       srcs (dirvish-rsync--extract-host-from-tramp src-0)
       (dirvish-rsync--extract-host-from-tramp dest)))
     ;; either shost, dhost or both are localhost
     (t (dirvish-rsync--l2fr-handler srcs dest)))))

(defun dirvish-rsync--transient-init-rsync-switches (obj)
  "Select initial values for transient suffixes, possibly from OBJ.
Use values from the local session or Emacs session or saved transient
values."
  (or (dirvish-prop :rsync-switches)
      ;; don't touch if it is alreday set
      (if (and (slot-boundp obj 'value) (oref obj value))
          (oref obj value)
        ;; check saved values
        (if-let* ((saved (assq (oref obj command) transient-values)))
            (cdr saved)
          ;; use default value at last resort
          dirvish-rsync-args))))

(transient-define-infix dirvish-rsync--r2r-ssh-host ()
  "Set ssh host of receiver in remote to remote case."
  :description "Ssh host of receiver"
  :class 'transient-lisp-variable
  :variable 'dirvish-rsync--r2r-ssh-recv-host
  :reader (lambda (_prompt _init _hist)
            (completing-read
             "Ssh receiver host: "
             nil nil nil dirvish-rsync--transient-input-history)))

(transient-define-infix dirvish-rsync--r2r-ssh-port ()
  "Set ssh port of receiver in remote to remote case."
  :description "Ssh port of receiver"
  :class 'transient-lisp-variable
  :variable 'dirvish-rsync--r2r-ssh-recv-port
  :reader (lambda (_prompt _init _hist)
            (completing-read
             "Ssh receiver port: "
             nil nil nil dirvish-rsync--transient-input-history)))

(transient-define-infix dirvish-rsync--r2r-ssh-user ()
  "Set ssh user of receiver in remote to remote case."
  :description "Ssh user of receiver"
  :class 'transient-lisp-variable
  :variable 'dirvish-rsync--r2r-ssh-recv-user
  :reader (lambda (_prompt _init _hist)
            (completing-read
             "Ssh receiver user: "
             nil nil nil dirvish-rsync--transient-input-history)))

(transient-define-infix dirvish-rsync--r2r-direct-conn ()
  :class 'transient-lisp-variable
  :variable 'dirvish-rsync--r2r-direct-conn
  :reader (lambda (_prompt _init _hist)
            (completing-read "direct: " '(yes no) nil t)))

(transient-define-prefix dirvish-rsync-transient-configure ()
  "Configure romete-to-remote connections for `dirvish-rsync'."
  ["Remote to remote"
   ("rh" "Receiver host" dirvish-rsync--r2r-ssh-host)
   ("rp" "Receiver port" dirvish-rsync--r2r-ssh-port)
   ("ru" "Receiver user" dirvish-rsync--r2r-ssh-user)
   ("rd" "Direct connection" dirvish-rsync--r2r-direct-conn)])

;; inspired by `dired-rsync-transient'
(define-obsolete-function-alias 'dirvish-rsync-transient #'dirvish-rsync-switches-menu "Feb 09, 2025")
;;;###autoload (autoload 'dirvish-rsync-switches-menu "dirvish-rsync" nil t)
(transient-define-prefix dirvish-rsync-switches-menu ()
  "Transient menu for `dirvish-rsync'."
  :init-value (lambda (o)
                (oset o value (dirvish-rsync--transient-init-rsync-switches o)))
  ["Common Arguments"
   ("-a" "archive mode; equals to -rlptgoD" ("-a" "--archive"))
   ("-s" "no space-splitting; useful when remote filenames contain spaces" ("-s" "--protect-args") :level 4)
   ("-r" "recurse into directories" ("-r" "--recursive") :level 5)
   ("-z" "compress file data during the transfer" ("-z" "--compress"))]
  ["Files selection args"
   ("-C" "auto-ignore files in the same way CVS does" ("-C" "--cvs-exclude") :level 4)
   ("=e" "exclude files matching PATTERN" "--exclude="
    :multi-value repeat :reader dirvish-rsync--transient-read-multiple
    :prompt "exclude (e.g. ‘*.git’ or ‘*.bin,*.elc’): ")
   ("=i" "include files matching PATTERN" "--include="
    :multi-value repeat :reader dirvish-rsync--transient-read-multiple
    :prompt "include (e.g. ‘*.pdf’ or ‘*.org,*.el’): " :level 5)]
  ["Sender specific args"
   ("-L" "transform symlink into referent file/dir" ("-L" "--copy-links") :level 4)
   ("-x" "don't cross filesystem boundaries" ("-x" "--one-file-system") :level 5)
   ("-l" "copy symlinks as symlinks" ("-l" "--links") :level 5)
   ("-c" "skip based on checksum, not mod-time & size" ("-c" "--checksum") :level 6)
   ("-m" "prune empty directory chains from file-list" ("-m" "--prune-empty-dirs") :level 6)
   ("--size-only" "skip files that match in size" "--size-only" :level 6)]
  ["Receiver specific args"
   ("-R" "use relative path names" ("-R" "--relative") :level 4)
   ("-u" "skip files that are newer on the receiver" ("-u" "--update") :level 4)
   ("=d" "delete extraneous files from dest dirs" "--delete" :level 4)
   ("-b" "make backups" ("-b" "--backup") :level 5)
   ("=bs" "backup suffix" "--suffix="
    :prompt "backup suffix: "
    :reader (lambda (prompt &optional _initial-input history)
              (completing-read prompt nil nil nil nil history))
    :level 5)
   ("-num" "don't map uid/gid values by user/group name" "--numeric-ids" :level 5)
   ("-ex" "skip creating new files on receiver" "--existing" :level 6)
   ("-K" "treat symlinked dir on receiver as dir" ("-K" "--keep-dirlinks") :level 6)]
  ["Information output"
   ("-v" "increase verbosity" ("-v" "--verbose"))
   ("-i" "output a change-summary for all updates" "-i" :level 5)
   ("-h" "output numbers in a human-readable format" "-h" :level 5)
   ("=I" "per-file (1) or total transfer (2) progress" "--info="
    :choices ("progress1" "progress2") :level 4)]
  ["Configure"
   ("C" "Set variables..."  dirvish-rsync-transient-configure)]
  ["Action"
   [("RET" "Apply switches and copy" dirvish-rsync--apply-switches-and-copy)]])

(defun dirvish-rsync--transient-read-multiple
    (prompt &optional _initial-input _history)
  "Read multiple values after PROMPT with optional INITIAL_INPUT and HISTORY."
  (let ((crm-separator ","))
    (completing-read-multiple
     prompt nil nil nil nil dirvish-rsync--transient-input-history)))

(defun dirvish-rsync--apply-switches-and-copy (args)
  "Execute rsync command generated by transient ARGS."
  (interactive (list (transient-args transient-current-command)))
  (dirvish-prop :rsync-switches args)
  (call-interactively #'dirvish-rsync))

(provide 'dirvish-rsync)
;;; dirvish-rsync.el ends here

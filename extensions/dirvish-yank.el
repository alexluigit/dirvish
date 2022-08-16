;;; dirvish-yank.el --- Multi-stage and async copy/paste/link utilities -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.9.23
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

;;; Code:

(require 'dired-aux)
(require 'dirvish)

(defcustom dirvish-yank-sources 'all
  "The way to collect source files.
The value can be a symbol or a function that returns a fileset."
  :group 'dirvish
  :type '(choice (const :tag "Marked files in current buffer" buffer)
                 (const :tag "Marked files in current session" session)
                 (const :tag "Marked files in all session within selected frame" frame)
                 (const :tag "Marked files in all sessions" all)
                 (function :tag "Custom function")))

(defcustom dirvish-yank-auto-unmark t
  "Control if yank commands should unmark when complete."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-yank-overwrite-existing-files 'ask
  "Whether to overwrite existing files when calling yank commands."
  :group 'dirvish
  :type '(choice (const :tag "prompt for confirmation" ask)
                 (const :tag "always overwrite" always)
                 (const :tag "never overwrite, create new file instead" never)))

(defcustom dirvish-yank-new-name-style 'append-to-ext
  "Control the way to compose new filename."
  :group 'dirvish
  :type '(choice (const :tag "append INDEX~ to file extension" append-to-ext)
                 (const :tag "append INDEX~ to file name" append-to-filename)
                 (const :tag "prepend INDEX~ to file name" prepend-to-filename)))

(defcustom dirvish-yank-methods
  '((yank     . "cp -frv")
    (move     . "mv -fv")
    (symlink  . "ln -sf")
    (relalink . "ln -srf")
    (hardlink . "cp -al")
    (rsync    . "rsync -avz"))
  "Yank methods and their flags."
  :group 'dirvish :type 'alist)

(defconst dirvish-yank-fallback-methods '((yank . dired-copy-file) (move . dired-rename-file)))
(defvar dirvish-yank-task-counter 0)
(defvar dirvish-yank--link-methods '(symlink relalink hardlink))
;; copied from `dired-rsync'
(defvar dirvish-yank--remote-portfwd
  "ssh -p %d -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"
  "An explicit ssh command for rsync to use port forwarded proxy.
The string is treated as a format string where %d is replaced with the
results of `dirvish-yank--get-remote-port'.")

(defun dirvish-yank--get-remote-port ()
  "Return the remote port we shall use for the reverse port-forward."
  (+ 50000 (length
            (seq-filter
             (lambda (p) (string-prefix-p "*Dirvish-yank" (process-name p)))
             (process-list)))))

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
                     ('frame (cl-loop for i in (reverse (dirvish-get-all 'roots nil t))
                                      by 'cddr collect i))
                     ('all (cl-loop for i in (reverse (dirvish-get-all 'roots t t))
                                    by 'cddr collect i)))
    for buffer in (seq-filter #'buffer-live-p buffers) append
    (with-current-buffer buffer
      (when (save-excursion (goto-char (point-min))
                            (re-search-forward regexp nil t))
        (dired-map-over-marks (dired-get-filename) nil))))
   :test #'equal))

(defun dirvish-yank--read-dest (method)
  "Helper function to read dest dir for METHOD."
  (when current-prefix-arg
    (read-file-name (format "%s files to: " method)
                    (dired-dwim-target-directory)
                    nil nil nil 'file-directory-p)))

(defun dirvish-yank--filename-for-rsync (file)
  "Reformat a tramp FILE to one usable for rsync."
  (if (tramp-tramp-file-p file)
      (with-parsed-tramp-file-name file tfop
        (format "%s%s:%s" (if tfop-user (format "%s@" tfop-user) "") tfop-host
                (shell-quote-argument tfop-localname)))
    (shell-quote-argument file)))

(defun dirvish-yank--extract-host (files)
  "Get host of FILES."
  (cl-loop
   with hosts = ()
   with sysname = (system-name)
   for f in files
   for h = (or (file-remote-p f 'host) sysname)
   do (cl-pushnew h hosts :test #'equal)
   when (> (length hosts) 1)
   do (user-error "Dirvish[error]: SOURCEs need to be in the same host")
   finally return (car hosts)))

(defun dirvish-yank--sentinel (process _exit)
  "Sentinel for yank task PROCESS."
  (when (memq (process-status process) '(exit signal))
    (shell-command-set-point-after-cmd (process-buffer process)))
  (setq dirvish-yank-task-counter (1- dirvish-yank-task-counter))
  (with-current-buffer (dirvish--util-buffer "yank-log")
    (save-excursion
      (delete-region
       (point-min)
       (let ((max (point-max)))
         (if (< max 20000)
             (point-min)
           (goto-char max)
           (dotimes (_n 40) (backward-paragraph))
           (point))))))
  (when-let* ((dv (process-get process 'dv))
              (dv-buf (window-buffer (dv-root-window dv))))
    (when (and (buffer-live-p dv-buf)
               (or (eq dv-buf (current-buffer))
                   (not (with-current-buffer dv-buf
                          (dirvish-prop :tramp)))))
      (with-current-buffer dv-buf (revert-buffer)))))

(defun dirvish-yank--execute (cmd &optional remotep)
  "Run yank CMD in the same host.
If REMOTEP, the CMD is passed to `tramp-handle-shell-command',
otherwise it is passed to `start-process-shell-command'."
  (let* ((process-connection-type nil)
         (buffer (dirvish--util-buffer "yank-log"))
         (display-buffer-alist
          '(("\\*Dirvish-yank-log\\*" (display-buffer-no-window))))
         (async-shell-command-buffer nil) ; it's a hack for buffer reuse
         (proc (if remotep
                   (tramp-handle-shell-command cmd buffer)
                 (start-process-shell-command "Dirvish-yank" buffer cmd))))
    (process-put proc 'dv (dirvish-curr))
    (set-process-sentinel proc #'dirvish-yank--sentinel)
    (when dirvish-yank-auto-unmark
      (cl-loop for buf in (reverse (dirvish-get-all 'roots t t)) by 'cddr
               do (with-current-buffer buf (dired-unmark-all-marks))))
    (setq dirvish-yank-task-counter (1+ dirvish-yank-task-counter))))

(defun dirvish-yank--ensure-newname (file base-name fileset dest)
  "Ensure an unique filename for FILE at DEST with FILESET.
BASE-NAME is the filename of file without directory."
  (let ((bname~ base-name) (idx 1))
    (while (member bname~ fileset)
      (setq bname~
            (shell-quote-argument
             (pcase dirvish-yank-new-name-style
               ('append-to-ext (format "%s%s~" base-name idx))
               ('append-to-filename
                (format "%s%s~.%s"
                        (file-name-sans-extension base-name)
                        idx (file-name-extension base-name)))
               ('prepend-to-filename (format "%s~%s" idx base-name)))))
      (setq idx (1+ idx)))
    (cons file (concat dest bname~))))

(defun dirvish-yank--prepare-dest-names (srcs dest)
  "Generate new unique file name pairs from SRCS and DEST."
  (cl-loop
   with overwrite = (eq dirvish-yank-overwrite-existing-files 'always)
   with never = (eq dirvish-yank-overwrite-existing-files 'never)
   with dest-local = (shell-quote-argument (file-local-name dest))
   with old-files = (mapcar #'shell-quote-argument
                            (directory-files dest nil nil t))
   with prompt-str = "%s exists, overwrite? (y)es (n)o (q)uit (Y)es-for-all (N)o-for-all"
   for file in srcs
   for base-name = (file-name-nondirectory file)
   for paste-name = (concat dest-local base-name)
   for collision = (member base-name old-files)
   for prompt = (format prompt-str base-name) collect
   (cond
    (overwrite (cons file (if (file-directory-p file) dest-local paste-name)))
    ((and never collision)
     (dirvish-yank--ensure-newname file base-name old-files dest-local))
    (collision
     (cl-case (read-char-choice prompt '(?y ?Y ?n ?N ?q))
       (?y (cons file (if (file-directory-p file) dest-local paste-name)))
       (?n (dirvish-yank--ensure-newname file base-name old-files dest-local))
       (?Y (setq overwrite t)
           (cons file (if (file-directory-p file) dest-local paste-name)))
       (?N (setq never t)
           (dirvish-yank--ensure-newname file base-name old-files dest-local))
       (?q (user-error "Dirvish[info]: yank task aborted"))))
    (t (cons file (if (file-directory-p file) dest-local paste-name))))))

(defun dirvish-yank--fallback-handler (method srcs dest)
  "Execute a fallback yank command with type of METHOD.
SRCS and DEST are source files and destination."
  (cl-loop
   with newnames = (dirvish-yank--prepare-dest-names srcs dest)
   with fn = (alist-get method dirvish-yank-fallback-methods)
   for (from . to) in newnames
   do (apply fn from to t)))

(defun dirvish-yank--l2l-handler (method srcs dest host)
  "Execute a local yank command with type of METHOD.
SRCS and DEST have to be in the same HOST (local or remote)."
  (let* ((srcs (mapcar
                (lambda (f) (shell-quote-argument (file-local-name f))) srcs))
         (fmt (format "%s %%s %%s" (alist-get method dirvish-yank-methods)))
         (newnames (dirvish-yank--prepare-dest-names srcs dest))
         (cmd (cl-loop
               with pairs = ()
               for (from . to) in newnames
               do (setq pairs (append pairs (list (format fmt from to))))
               finally return (format "%s &" (string-join pairs " & ")))))
    (dirvish-yank--execute cmd (not (eq host 'local)))))

(defun dirvish-yank--l2fr-handler (srcs dest)
  "Execute a local to/from remote rsync command for SRCS and DEST."
  (let* ((rsync-cmd (alist-get 'rsync dirvish-yank-methods))
         (srcs (mapcar #'dirvish-yank--filename-for-rsync srcs))
         (final-dest (dirvish-yank--filename-for-rsync dest))
         (cmd (string-join
               (flatten-tree (list rsync-cmd srcs final-dest)) " ")))
    (dirvish-yank--execute cmd)))

;; Thanks to `dired-rsync.el'
;; also see: https://unix.stackexchange.com/questions/183504/how-to-rsync-files-between-two-remotes
(defun dirvish-yank--r2r-handler (srcs dest shost dhost)
  "Construct and trigger an rsync run for remote copy.
This command sync SRCS on SHOST to DEST on DHOST."
  (let* ((duser (with-parsed-tramp-file-name dest tfop
                  (or tfop-user (getenv "USER"))))
         (port (dirvish-yank--get-remote-port))
         (dest (shell-quote-argument (file-local-name dest)))
         (rsync-cmd
          (format "\"%s -e \\\"%s\\\" %s %s@localhost:%s\""
                  (alist-get 'rsync dirvish-yank-methods)
                  (format dirvish-yank--remote-portfwd port)
                  (string-join srcs " ") duser dest))
         (bind-addr (format "localhost:%d:%s:22" port dhost))
         (cmd (string-join
               (list "ssh" "-A" "-R" bind-addr shost rsync-cmd) " ")))
    (dirvish-yank--execute cmd)))

(defun dirvish-yank--apply (method dest)
  "Apply yank METHOD to DEST."
  (let* ((dest (or dest (dired-current-directory)))
         (sysname (system-name))
         (dvec (and (tramp-tramp-file-p dest) (tramp-dissect-file-name dest)))
         (dhost (or (file-remote-p dest 'host) sysname))
         (srcs (or (and (functionp dirvish-yank-sources)
                        (funcall dirvish-yank-sources))
                   (dirvish-yank--get-srcs dirvish-yank-sources)
                   (user-error "Dirvish[error]: no marked files")))
         (src-0 (car srcs))
         (svec (and (tramp-tramp-file-p src-0) (tramp-dissect-file-name src-0)))
         (shost (dirvish-yank--extract-host srcs)))
    (cond
     ((and (memq method dirvish-yank--link-methods)
           (not (equal shost dhost)))
      (user-error "Dirvish[error]: can not make links between different hosts"))
     ((and (not (and (or (not svec) (dirvish--host-in-whitelist-p svec))
                     (or (not dvec) (dirvish--host-in-whitelist-p dvec))))
           (not (memq method dirvish-yank--link-methods)))
      (dirvish-yank--fallback-handler method srcs dest))
     ((equal shost dhost)
      (dirvish-yank--l2l-handler method srcs dest shost))
     ((not (or (equal shost sysname) (equal dhost sysname)))
      (dirvish-yank--r2r-handler srcs dest shost dhost))
     (t
      (dirvish-yank--l2fr-handler srcs dest)))))

(dirvish-define-mode-line yank
  "Number of running yank tasks."
  (when (> dirvish-yank-task-counter 0)
    (format " %s %s%s "
            (propertize (number-to-string dirvish-yank-task-counter)
                        'face 'font-lock-keyword-face)
            (propertize "running yank task" 'face 'font-lock-doc-face)
            (propertize (if (> dirvish-yank-task-counter 1) "s" "")
                        'face 'font-lock-doc-face))))

;;;###autoload
(defun dirvish-yank (&optional dest)
  "Paste marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument].

If you want to use this command and friends (such as
`dirvish-move') for file transfer involving remote hosts, you'll
need to have proper ssh configuration for those hosts, because an
asynchronous TRAMP connection and the rsync command (which always
run locally) require working SSH authentication which bypasses
the password entering to work, which see Info
node `(tramp)Improving performance of asynchronous remote
processes' and the man page `rsync(1)'.  If the remote host does
not come with proper ssh configuration, the fallback command
defined in `dirvish-yank-fallback-methods' are used.

To make TRAMP more responsive, follow the instructions in Info
node `(tramp)Frequently Asked Questions' to speed it up."
  (interactive (dirvish-yank--read-dest 'yank))
  (dirvish-yank--apply 'yank dest))

;;;###autoload
(defun dirvish-move (&optional dest)
  "Move marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument].  Also
see `dirvish-yank' for additional information."
  (interactive (dirvish-yank--read-dest 'move))
  (dirvish-yank--apply 'move dest))

;;;###autoload
(defun dirvish-symlink (&optional dest)
  "Symlink marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument].  Also
see `dirvish-yank' for additional information."
  (interactive (dirvish-yank--read-dest 'symlink))
  (dirvish-yank--apply 'symlink dest))

;;;###autoload
(defun dirvish-relative-symlink (&optional dest)
  "Similar to `dirvish-symlink', but link files relatively.
Prompt for DEST when prefixed with \\[universal-argument].  Also
see `dirvish-yank' for additional information."
  (interactive (dirvish-yank--read-dest 'relalink))
  (dirvish-yank--apply 'relalink dest))

;;;###autoload
(defun dirvish-hardlink (&optional dest)
  "Hardlink marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument].  Also
see `dirvish-yank' for additional information."
  (interactive (dirvish-yank--read-dest 'hardlink))
  (dirvish-yank--apply 'hardlink dest))

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
        (if (derived-mode-p 'dirvish-mode)
            (transient-setup 'dirvish-yank-menu)
          (user-error "Not in a Dirvish buffer"))))))

(provide 'dirvish-yank)
;;; dirvish-yank.el ends here

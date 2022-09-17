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

;;; Code:

(require 'dired-aux)
(require 'dirvish)
(require 'dirvish-tramp)

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
             (lambda (p) (string-prefix-p " *Dirvish-yank" (process-name p)))
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
                   (not (with-current-buffer dv-buf (dirvish-prop :remote)))))
      (with-current-buffer dv-buf (revert-buffer)))))

(defun dirvish-yank--execute (cmd)
  "Run yank CMD in the same host."
  (let* ((process-connection-type nil)
         (buffer (dirvish--util-buffer "yank-log"))
         (proc (start-file-process-shell-command
                (buffer-name buffer) buffer cmd)))
    (process-put proc 'dv (dirvish-curr))
    (set-process-sentinel proc #'dirvish-yank--sentinel)
    (when dirvish-yank-auto-unmark
      (cl-loop for buf in (reverse (dirvish-get-all 'roots t t)) by 'cddr
               do (with-current-buffer buf (dired-unmark-all-marks))))
    (cl-incf dirvish-yank-task-counter)))

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

(defun dirvish-yank--prepare-dest-names (srcs dest)
  "Generate new unique file name pairs from SRCS and DEST."
  (cl-loop
   with overwrite = (eq dirvish-yank-overwrite-existing-files 'always)
   with never = (eq dirvish-yank-overwrite-existing-files 'never)
   with dfiles = (directory-files dest nil nil t)
   with fmt = "%s exists, overwrite? (y)es (n)o (q)uit (Y)es-to-all (N)o-to-all"
   with to-rename = ()
   for file in srcs
   for base = (file-name-nondirectory file)
   for collision = (member base dfiles) do
   (cond (overwrite nil)
         ((and never collision)
          (push (dirvish-yank--newbase base dfiles dest) to-rename))
         (collision
          (cl-case (read-char-choice (format fmt base) '(?y ?Y ?n ?N ?q))
            (?y nil)
            (?n (push (dirvish-yank--newbase base dfiles dest) to-rename))
            (?Y (setq overwrite t))
            (?N (setq never t)
                (push (dirvish-yank--newbase base dfiles dest) to-rename))
            (?q (user-error "Dirvish[info]: yank task aborted")))))
   finally (cl-loop for (from . to) in to-rename do (rename-file from to))))

(defun dirvish-yank--fallback-handler (method srcs dest)
  "Execute a fallback yank command with type of METHOD.
SRCS and DEST are source files and destination."
  (dirvish-yank--prepare-dest-names srcs dest)
  (cl-loop with fn = (alist-get method dirvish-yank-fallback-methods)
           for src in srcs do (funcall fn src dest t)))

(defun dirvish-yank--l2l-handler (method srcs dest)
  "Execute a local yank command with type of METHOD.
SRCS and DEST have to be in the same HOST (local or remote)."
  (let* ((method (alist-get method dirvish-yank-methods))
         (l-files (cl-loop for f in srcs collect
                           (shell-quote-argument (file-local-name f))))
         (files (mapconcat #'concat l-files ","))
         (cmd (format "%s %s %s" method
                      (if (> (length srcs) 1) (format "{%s}" files) files)
                      (shell-quote-argument (file-local-name dest)))))
    (dirvish-yank--prepare-dest-names srcs dest)
    (dirvish-yank--execute cmd)))

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
     ((and (not (and (or (not svec) (dirvish-tramp--async-p svec))
                     (or (not dvec) (dirvish-tramp--async-p dvec))))
           (not (memq method dirvish-yank--link-methods)))
      (dirvish-yank--fallback-handler method srcs dest))
     ((equal shost dhost)
      (dirvish-yank--l2l-handler method srcs dest))
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
  "Paste marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it
defaults to `dired-current-directory.'

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
  "Move marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it
defaults to `dired-current-directory'.  See `dirvish-yank' for
additional information."
  (interactive (dirvish-yank--read-dest 'move))
  (dirvish-yank--apply 'move dest))

;;;###autoload
(defun dirvish-symlink (&optional dest)
  "Symlink marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it
defaults to `dired-current-directory'.  See `dirvish-yank' for
additional information."
  (interactive (dirvish-yank--read-dest 'symlink))
  (dirvish-yank--apply 'symlink dest))

;;;###autoload
(defun dirvish-relative-symlink (&optional dest)
  "Similar to `dirvish-symlink', but link files relatively.
Prompt for DEST when prefixed with \\[universal-argument], it
defaults to `dired-current-directory'.  See `dirvish-yank' for
additional information."
  (interactive (dirvish-yank--read-dest 'relalink))
  (dirvish-yank--apply 'relalink dest))

;;;###autoload
(defun dirvish-hardlink (&optional dest)
  "Hardlink marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it
defaults to `dired-current-directory'.  See `dirvish-yank' for
additional information."
  (interactive (dirvish-yank--read-dest 'hardlink))
  (dirvish-yank--apply 'hardlink dest))

(provide 'dirvish-yank)
;;; dirvish-yank.el ends here

;;; dirvish-yank.el --- Multi-stage and async copy/paste/link utilities in Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.2.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (dirvish "1.2.0"))

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

(require 'tramp)
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

(defvar dirvish-yank--progress (cons 0 0))
(defvar dirvish-yank--buffer (dirvish--ensure-temp-buffer "yank"))
(defvar dirvish-yank--link-methods '(symlink relalink hardlink))
(defvar dirvish-yank--status-timer nil)
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
        (format "%s%s:\"%s\"" (if tfop-user (format "%s@" tfop-user) "") tfop-host
                (shell-quote-argument tfop-localname)))
    (shell-quote-argument file)))

(defun dirvish-yank--extract-host (files)
  "Get host of FILES."
  (cl-loop
   with hosts = ()
   for f in files
   for h = (or (file-remote-p f 'host) 'local)
   do (cl-pushnew h hosts :test #'equal)
   when (> (length hosts) 1)
   do (user-error "Dirvish: SOURCEs need to be in the same host")
   finally return (car hosts)))

(defun dirvish-yank--status-update ()
  "Update current yank task progress."
  (with-current-buffer dirvish-yank--buffer
    (let* ((proc-exit "Process \\(.*\\) \\(exited\\|finished\\)\\(.*\\)")
           (progress (how-many proc-exit (point-min) (point-max))))
      (if (eq progress (cdr dirvish-yank--progress))
          (progn
            (goto-char (point-min))
            (save-excursion
              (delete-region
               (point)
               (progn (goto-char (point-max))
                      (dotimes (_n 20) (backward-paragraph))
                      (point))))
            (while (re-search-forward proc-exit nil t)
              (replace-match
               (format "Task \\2 @ %s \\3\n" (current-time-string))))
            (setq dirvish-yank--progress (cons 0 0))
            (when (timerp dirvish-yank--status-timer)
              (cancel-timer dirvish-yank--status-timer))
            (setq dirvish-yank--status-timer nil)
            (when-let (dv (dirvish-curr))
              (with-current-buffer (window-buffer (dv-root-window dv))
                (revert-buffer))))
        (setcar dirvish-yank--progress progress)))))

(defun dirvish-yank--execute (cmd)
  "Run yank CMD in subprocesses."
  (let ((process-connection-type nil)
        (procname (format "*Dirvish-yank @ %s*" (current-time-string))))
    (start-process-shell-command procname dirvish-yank--buffer cmd)
    (when dirvish-yank-auto-unmark
      (cl-dolist (buf (dirvish-get-all 'dired-buffers t t))
        (with-current-buffer buf (dired-unmark-all-marks))))
    (setcdr dirvish-yank--progress (1+ (cdr dirvish-yank--progress)))
    (unless dirvish-yank--status-timer
      (setq dirvish-yank--status-timer
            (run-with-timer 0 0.1 #'dirvish-yank--status-update)))))

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
   with always = (eq dirvish-yank-overwrite-existing-files 'always)
   with never = (eq dirvish-yank-overwrite-existing-files 'never)
   with dest-local = (shell-quote-argument (file-local-name dest))
   with dest-old-files = (mapcar #'shell-quote-argument
                                 (directory-files dest nil nil t))
   with prompt-str = "%s exists, overwrite? (y)es (n)o (q)uit (Y)es-for-all (N)o-for-all"
   for file in srcs
   for base-name = (file-name-nondirectory file)
   for paste-name = (concat dest-local base-name)
   for collision = (member base-name dest-old-files) ;; avoid using `file-exists-p' for performance
   for prompt = (format prompt-str base-name) collect
   (cond
    (always (cons file (if (file-directory-p file) dest-local paste-name)))
    ((and never collision)
     (dirvish-yank--ensure-newname file base-name dest-old-files dest-local))
    (collision
     (cl-case (read-char-choice prompt '(?y ?Y ?n ?N ?q))
       (?y (cons file (if (file-directory-p file) dest-local paste-name)))
       (?n (dirvish-yank--ensure-newname file base-name dest-old-files dest-local))
       (?Y (setq always t)
           (cons file (if (file-directory-p file) dest-local paste-name)))
       (?N (setq never t)
           (dirvish-yank--ensure-newname file base-name dest-old-files dest-local))
       (?q (user-error "Dirvish: yank task aborted"))))
    (t (cons file (if (file-directory-p file) dest-local paste-name))))))

(defun dirvish-yank--l2l-handler (method srcs dest host)
  "Execute a local yank command with type of METHOD.
SRCS and DEST have to be in the same HOST (local or remote)."
  (let* ((srcs (mapcar
                (lambda (f) (shell-quote-argument (file-local-name f))) srcs))
         (fmt (format "%s %%s %%s" (alist-get method dirvish-yank-methods)))
         (newnames (dirvish-yank--prepare-dest-names srcs dest))
         (localp (eq host 'local))
         (cmd
          (cl-loop
           with pairs = ()
           for (from . to) in newnames
           do (setq pairs (append pairs (list (format fmt from to))))
           finally return (format (if localp "%s%s" "%s\"%s\"")
                                  (if localp "" (format "ssh %s " host))
                                  (string-join pairs " & ")))))
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
         (dhost (or (file-remote-p dest 'host) 'local))
         (srcs (or (and (functionp dirvish-yank-sources)
                        (funcall dirvish-yank-sources))
                   (dirvish--marked-files dirvish-yank-sources)
                   (user-error "Dirvish: no marked files")))
         (shost (dirvish-yank--extract-host srcs)))
    (cond
     ((and (memq method dirvish-yank--link-methods)
           (not (equal shost dhost)))
      (user-error "Dirvish: can not make links between different hosts"))
     ((equal shost dhost)
      (dirvish-yank--l2l-handler method srcs dest shost))
     ((not (or (eq shost 'local) (eq dhost 'local)))
      (dirvish-yank--r2r-handler srcs dest shost dhost))
     (t
      (dirvish-yank--l2fr-handler srcs dest)))))

;;;###autoload (autoload 'dirvish-yank-ml "dirvish-yank" nil t)
(dirvish-define-mode-line yank
  "Current move/paste task progress."
  (pcase-let ((`(,progress . ,total) dirvish-yank--progress))
    (when (> total 0)
      (format "%s%s " (propertize "Yanking: " 'face 'bold)
              (propertize (format "%s of %s" progress total)
                          'face 'font-lock-keyword-face)))))

;;;###autoload
(defun dirvish-yank (&optional dest)
  "Paste marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument]."
  (interactive (dirvish-yank--read-dest 'yank))
  (dirvish-yank--apply 'yank dest))

;;;###autoload
(defun dirvish-move (&optional dest)
  "Move marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument]."
  (interactive (dirvish-yank--read-dest 'move))
  (dirvish-yank--apply 'move dest))

;;;###autoload
(defun dirvish-symlink (&optional dest)
  "Symlink marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument]."
  (interactive (dirvish-yank--read-dest 'symlink))
  (dirvish-yank--apply 'symlink dest))

;;;###autoload
(defun dirvish-relative-symlink (&optional dest)
  "Similar to `dirvish-symlink', but link files relatively.
Prompt for DEST when prefixed with \\[universal-argument]."
  (interactive (dirvish-yank--read-dest 'relalink))
  (dirvish-yank--apply 'relalink dest))

;;;###autoload
(defun dirvish-hardlink (&optional dest)
  "Hardlink marked files to DEST (which defaults to `dired-current-directory').
Prompt for DEST when prefixed with \\[universal-argument]."
  (interactive (dirvish-yank--read-dest 'hardlink))
  (dirvish-yank--apply 'hardlink dest))

(provide 'dirvish-yank)
;;; dirvish-yank.el ends here

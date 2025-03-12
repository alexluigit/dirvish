;;; dirvish-yank.el --- Multi-stage and async copy/paste/link utilities -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.2.7
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
(require 'transient)

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
                 (const :tag "skip transferring files with same names" skip)
                 (const :tag "overwrite and backup the original file" backup)))

(defcustom dirvish-yank-new-name-style 'append-to-ext
  "Control the way to compose new filename."
  :group 'dirvish
  :type '(choice (const :tag "append INDEX~ to file extension" append-to-ext)
                 (const :tag "append INDEX~ to file name" append-to-filename)
                 (const :tag "prepend INDEX~ to file name" prepend-to-filename)))

(defcustom dirvish-yank-keep-success-log t
  "If non-nil then keep logs of all completed yanks.
By default only keep the log buffer alive for failed tasks."
  :type 'boolean :group 'dirvish)

(defun dirvish-yank--menu-setter (symbol pairs)
  "Set key-command PAIRS for SYMBOL `dirvish-yank-menu'."
  (when symbol (set symbol pairs))
  (eval
   `(transient-define-prefix dirvish-yank-menu ()
      "Yank commands menu."
      [:description
       (lambda () (dirvish--format-menu-heading
              "Select yank operation on marked files:"))
       ,@pairs]
      (interactive)
      (if (derived-mode-p 'dired-mode)
          (transient-setup 'dirvish-yank-menu)
        (user-error "Not in a Dirvish buffer")))))

;;;###autoload (autoload 'dirvish-yank-menu "dirvish-yank" nil t)
(defcustom dirvish-yank-keys
  '(("y" "Yank (paste) here"           dirvish-yank)
    ("m" "Move here"                   dirvish-move)
    ("s" "Make symlinks here"          dirvish-symlink)
    ("r" "Make relative symlinks here" dirvish-relative-symlink)
    ("h" "Make hardlinks here"         dirvish-hardlink))
  "YANK-KEYs for command `dirvish-yank-menu'.
A YANK-KEY is a (KEY DOC CMD) alist where KEY is the key to invoke the
CMD, DOC is the documentation string."
  :group 'dirvish :type 'alist :set #'dirvish-yank--menu-setter)

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
;; matches "Enter passphrase for key ..." (ssh) and "password for ..." (samba)
(defvar dirvish-yank-passphrase-stall-regex
  "\\(Enter \\)?[Pp]ass\\(word\\|phrase\\) for\\( key\\)?"
  "A regex to detect passphrase prompts.")
(defvar dirvish-yank-percent-complete-regex "[[:digit:]]\\{1,3\\}%"
  "A regex to extract the % complete from a file.")

(defun dirvish-yank--get-srcs (&optional range)
  "Get all marked filenames in RANGE.
RANGE can be `buffer', `session', `all'."
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
                                           (derived-mode-p 'dired-mode))
                                    collect b)))
    for buffer in (seq-filter #'buffer-live-p buffers) append
    (with-current-buffer buffer
      (when (save-excursion (goto-char (point-min))
                            (re-search-forward regexp nil t))
        (dired-map-over-marks (dired-get-filename) nil))))
   :test #'equal))

(defun dirvish-yank--read-dest (method)
  "Read dest dir for METHOD when prefixed with `current-prefix-arg'."
  (list (when current-prefix-arg
          (read-file-name (format "%s files to: " method)
                          (dired-dwim-target-directory)
                          nil nil nil 'file-directory-p))))

(defun dirvish-yank-proc-sentinel (proc _exit)
  "Sentinel for yank task PROC."
  (pcase-let ((proc-buf (process-buffer proc))
              (`(,buffer ,_ ,_ ,method) (process-get proc 'details))
              (status (process-status proc))
              (success (eq (process-exit-status proc) 0)))
    (when (memq status '(exit signal))
      (if (and success (not dirvish-yank-keep-success-log))
          (kill-buffer proc-buf)
        (with-current-buffer (get-buffer-create "*dirvish-yank-log*")
          (goto-char (point-max))
          (insert "\n\n" (format "%s" method)
                  " finished @ " (current-time-string) "\n")
          (insert-buffer-substring proc-buf)
          (kill-buffer proc-buf)
          ;; truncate old logs
          (save-excursion
            (delete-region
             (point-min)
             (let ((max (point-max)))
               (if (< max 20000)
                   (point-min)
                 (goto-char max)
                 (dotimes (_n 40) (backward-paragraph))
                 (point)))))
          (unless success
            (message "Task FAILED with exit code %s" (process-exit-status proc))
            (pop-to-buffer (current-buffer)))))
      (when (eq buffer (current-buffer))
        (with-current-buffer buffer
          (revert-buffer) (dirvish--update-display))))))

(defun dirvish-yank-proc-filter (proc string)
  "Filter for yank task PROC's STRING."
  (let ((proc-buf (process-buffer proc)))
    ;; check for passphrase prompt
    (when (string-match dirvish-yank-passphrase-stall-regex string)
      (process-send-string proc (concat (read-passwd string) "\n")))
    ;; Answer yes for `large file' prompt
    (when (string-match "File .* is large\\(.*\\), really copy" string)
      (process-send-string proc "y\n"))
    (let ((old-process-mark (process-mark proc)))
      (when (buffer-live-p proc-buf)
        (with-current-buffer proc-buf
          (when (string-match dirvish-yank-percent-complete-regex string)
            (dirvish-prop :yank-percent (match-string 0 string))
            (force-mode-line-update t))
          (let ((moving (= (point) old-process-mark)))
            (save-excursion
              (goto-char old-process-mark)
              (insert string)
              (set-marker (process-mark proc) (point)))
            (if moving (goto-char (process-mark proc)))))))))

(defun dirvish-yank--execute (cmd details &optional batch)
  "Handle execution of CMD.
When BATCH, execute the command using `emacs -q -batch'.  Propagate
DETAILS to the process.  Remove markers when `dirvish-yank-auto-unmark'
is t."
  (pcase-let* ((`(,_ ,_ ,dest ,_) details)
               (command (if batch
                            (let ((q (if (file-remote-p dest) "-q" "-Q")))
                              (list dirvish-emacs-bin q "-batch" "--eval" cmd))
                          cmd)))
    (dirvish-yank--start-proc command details)
    (when dirvish-yank-auto-unmark
      (cl-loop for buf in (buffer-list)
               do (with-current-buffer buf
                    (when (derived-mode-p 'dired-mode)
                      (dired-unmark-all-marks)))))))

(defun dirvish-yank--start-proc (cmd details)
  "Start a new process for CMD, put DETAILS into the process."
  (let* ((process-connection-type nil) (name "*dirvish-yank*")
         (buf (get-buffer-create (format "*yank@%s*" (current-time-string))))
         (fn (lambda () (setq dirvish-yank-log-buffers
                         (delete buf dirvish-yank-log-buffers))))
         (proc (if (listp cmd)
                   (make-process :name name :buffer buf :command cmd)
                 (start-process-shell-command name buf cmd))))
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook fn nil t) ; user may kill yank buffers
      (dirvish-prop :yank-details details))
    (process-put proc 'details details)
    (set-process-sentinel proc #'dirvish-yank-proc-sentinel)
    (set-process-filter proc #'dirvish-yank-proc-filter)
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

(defun dirvish-yank--filename-pairs (method srcs dest)
  "Generate file name pairs from SRCS and DEST for yank METHOD."
  (cl-loop
   with overwrite = (eq dirvish-yank-overwrite-existing-files 'always)
   with backup = (eq dirvish-yank-overwrite-existing-files 'backup)
   with skip = (eq dirvish-yank-overwrite-existing-files 'skip)
   with (result to-rename) = ()
   with dfiles = (directory-files dest nil nil t)
   for src in srcs
   for help-form = (format-message "\
File `%s' exists, type one of the following keys to continue.

- `y' or SPC to overwrite this file WITHOUT backup
- `!' answer `y' (overwrite) for all remaining files
- `n' or DEL to skip this file
- `N' answer `n' (skip) for all remaining files
- `b' to overwrite and backup this files
- `B' answer `b' (overwrite and backup) for all remaining files
- `q' or ESC to abort the task" src)
   for base = (file-name-nondirectory src)
   for collision = (member base dfiles) do
   (cond ((equal src (concat dest base))
          ;; user may want to make symlink in the same directory
          (if (memq method '(dired-make-relative-symlink make-symbolic-link))
              (push (cons src (cdr (dirvish-yank--newbase base dfiles dest)))
                    result)
            (user-error
             "DIRVISH[yank]: source and target are the same file `%s'" src)))
         (overwrite (push (cons src dest) result))
         ((and backup collision)
          (push (dirvish-yank--newbase base dfiles dest) to-rename)
          (push (cons src dest) result))
         ((and skip collision))
         (collision
          (cl-case (read-char-choice
                    (concat (format-message "Overwrite `%s'?" base)
                            (format " [Type yn!bq or %s] "
                                    (key-description (vector help-char))))
                    '(?y ?\s ?! ?n ?\177 ?N ?b ?B ?q ?\e))
            ((?y ?\s) (push (cons src dest) result))
            (?! (setq overwrite t) (push (cons src dest) result))
            ((?n ?\177) nil)
            (?N (setq skip t) nil)
            (?b (push (dirvish-yank--newbase base dfiles dest) to-rename)
                (push (cons src dest) result))
            (?B (setq backup t)
                (push (dirvish-yank--newbase base dfiles dest) to-rename)
                (push (cons src dest) result))
            ((?q ?\e) (user-error "DIRVISH[yank]: task aborted"))))
         (t (push (cons src dest) result)))
   finally return
   (prog1 result
     (cl-loop for (from . to) in to-rename do (rename-file from to)))))

(defun dirvish-yank--inject-env (include-regexp)
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

(defun dirvish-yank-default-handler (method srcs dest)
  "Execute yank METHOD on SRCS to DEST."
  (let* ((pairs (dirvish-yank--filename-pairs method srcs dest))
         (count (float (length pairs)))
         (cmd `(progn
                 (require 'dired-aux)
                 (require 'dired-x)
                 ,(dirvish-yank--inject-env dirvish-yank-env-variables-regexp)
                 (cl-loop
                  with dired-recursive-copies = 'always
                  with dired-copy-preserve-time = ,dired-copy-preserve-time
                  for idx from 1
                  for (from . to) in '(,@pairs)
                  for percent = (if (eq (float idx) ,count) 100
                                  (floor (* (/ idx ,count) 100)))
                  do (progn (message "%s -> %s [%s%%]" from to percent)
                            (condition-case err
                                (funcall #',method from to t)
                              (file-error
                               (message "%s: %s\n" (car err) (cdr err)) nil)))
                  finally (cl-loop for b in (buffer-list) thereis
                                   (and (string-match "\\`\\*ftp.*"
                                                      (buffer-name b))
                                        (prog1 b (kill-buffer b)))))))
         print-level print-length)
    (dirvish-yank--execute
     (prin1-to-string cmd) (list (current-buffer) srcs dest method) 'batch)))

(defun dirvish-yank--apply (method dest)
  "Apply yank METHOD to DEST."
  (setq dest (expand-file-name (or dest (dired-current-directory))))
  (let ((srcs (or (and (functionp dirvish-yank-sources)
                       (funcall dirvish-yank-sources))
                  (dirvish-yank--get-srcs dirvish-yank-sources)
                  (user-error "DIRVISH[yank]: no marked files"))))
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
                   (propertize "running tasks" 'face 'font-lock-doc-face)
                   (propertize (if (> number-of-tasks 1) "s" "")
                               'face 'font-lock-doc-face))))))

;;;###autoload
(defun dirvish-yank (&optional dest)
  "Paste marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it defaults
to `dired-current-directory.'"
  (interactive (dirvish-yank--read-dest 'yank))
  (dirvish-yank--apply 'dired-copy-file dest))

;;;###autoload
(defun dirvish-move (&optional dest)
  "Move marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it defaults
to `dired-current-directory'."
  (interactive (dirvish-yank--read-dest 'move))
  (dirvish-yank--apply 'dired-rename-file dest))

;;;###autoload
(defun dirvish-symlink (&optional dest)
  "Symlink marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it defaults
to `dired-current-directory'."
  (interactive (dirvish-yank--read-dest 'symlink))
  (dirvish-yank--apply 'make-symbolic-link dest))

;;;###autoload
(defun dirvish-relative-symlink (&optional dest)
  "Similar to `dirvish-symlink', but link files relatively.
Prompt for DEST when prefixed with \\[universal-argument], it defaults
to `dired-current-directory'."
  (interactive (dirvish-yank--read-dest 'relalink))
  (dirvish-yank--apply 'dired-make-relative-symlink dest))

;;;###autoload
(defun dirvish-hardlink (&optional dest)
  "Hardlink marked files to DEST.
Prompt for DEST when prefixed with \\[universal-argument], it defaults
to `dired-current-directory'."
  (interactive (dirvish-yank--read-dest 'hardlink))
  (dirvish-yank--apply 'dired-hardlink dest))

(provide 'dirvish-yank)
;;; dirvish-yank.el ends here

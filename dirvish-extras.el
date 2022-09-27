;;; dirvish-extras.el --- Extra utilities and transient prefixes for dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; dirvish-extras.el contains the TRAMP integration for dirvish, it is only
;; loaded after a TRAMP connection is initiated.  Besides, it provides some
;; utilities and transient prefixes.  This is an optimization to avoid having to
;; load functions that are rarely used during start-up.
;;
;; Commands included:
;; - `dirvish-find-file-true-path'
;; - `dirvish-copy-file-name' (autoload)
;; - `dirvish-copy-file-path' (autoload)
;; - `dirvish-copy-file-directory'
;; - `dirvish-total-file-size' (autoload)
;; - `dirvish-rename-space-to-underscore'
;;
;; Transient prefixes included (all autoloaded):
;; - `dirvish-file-info-menu'
;; - `dirvish-renaming-menu'
;; - `dirvish-subdir-menu'
;; - `dirvish-chxxx-menu'
;; - `dirvish-mark-menu'
;; - `dirvish-epa-dired-menu'
;; - `dirvish-setup-menu'

;;; Code:

(require 'dirvish)
(require 'tramp)

(defclass dirvish-attribute (transient-infix)
  ((variable  :initarg :variable))
  "Class for dirvish attributes.")

(cl-defmethod transient-format-description ((obj dirvish-attribute))
  "Format description for DIRVISH-ATTRIBUTE instance OBJ."
  (format "%s%s" (oref obj description)
          (propertize " " 'display '(space :align-to (- right 5)))))

(cl-defmethod transient-format-value ((obj dirvish-attribute))
  "Format value for DIRVISH-ATTRIBUTE instance OBJ."
  (let* ((val (oref obj value))
         (face (if (equal val "+") 'transient-argument 'transient-inactive-value)))
    (propertize val 'face face)))

(cl-defmethod transient-init-value ((obj dirvish-attribute))
  "Initialize value for DIRVISH-ATTRIBUTE instance OBJ."
  (let ((sym (oref obj variable)))
    (oset obj value (if (memq sym dirvish-attributes) "+" "-"))))

(cl-defmethod transient-infix-read ((obj dirvish-attribute))
  "Read value from DIRVISH-ATTRIBUTE instance OBJ."
  (oset obj value (if (equal (oref obj value) "+") "-" "+")))

(cl-defmethod transient-infix-set ((obj dirvish-attribute) value)
  "Set relevant value in DIRVISH-ATTRIBUTE instance OBJ to VALUE."
  (let* ((item (oref obj variable))
         (old-val (purecopy dirvish-attributes))
         (new-val (if (equal value "+") (cl-pushnew item old-val)
                    (remq item old-val)))
         (attrs (append '(hl-line symlink-target) new-val)))
    (mapc #'require '(dirvish-widgets dirvish-vc dirvish-collapse))
    (dirvish--render-attrs 'clear)
    (setq-local dirvish--working-attrs (dirvish--attrs-expand attrs))
    (dirvish--render-attrs)))

(defconst dirvish-tramp-preview-cmd
  "head -n 1000 %s 2>/dev/null || ls -Alh --group-directories-first %s 2>/dev/null")
(defvar dirvish-tramp-hosts '())

(defun dirvish-ls-output-parser (entry output)
  "Parse ls OUTPUT for ENTRY and store it in `dirvish--attrs-hash'."
  (dolist (file (and (> (length output) 2) (cl-subseq output 2 -1)))
    (cl-destructuring-bind
        (inode priv lnum user group size mon day time &rest path)
        (split-string file)
      (let* ((sym (cl-position "->" path :test #'equal))
             (f-name (string-join (cl-subseq path 0 sym) " "))
             (f-mtime (concat mon " " day " " time))
             (f-truename (and sym (string-join (cl-subseq path (1+ sym)) " ")))
             (f-dirp (string-prefix-p "d" priv))
             (f-type (or f-truename f-dirp)))
        (puthash (intern (secure-hash 'md5 (expand-file-name f-name entry)))
                 `(:builtin ,(list f-type lnum user group nil
                                   f-mtime nil size priv nil inode)
                   :type ,(cons (if f-dirp 'dir 'file) f-truename))
                 dirvish--attrs-hash)))))

(defun dirvish-gnuls-available-p (dir)
  "Check if GNU ls is available or not over DIR."
  (with-temp-buffer
    (cl-letf (((symbol-function 'display-message-or-buffer) #'ignore))
      (let ((default-directory dir))
        (= (tramp-handle-shell-command "ls --version") 0)))))

(defun dirvish-noselect-tramp (fn dir flags remote)
  "Return the Dired buffer at DIR with listing FLAGS.
Save the REMOTE host to `dirvish-tramp-hosts'.
FN is the original `dired-noselect' closure."
  (let* ((r-flags (cdr (assoc remote dirvish-tramp-hosts #'equal)))
         (ftp (tramp-ftp-file-name-p dir))
         (short-flags "-Alh")
         (gnu? t)
         (dired-buffers nil) ; disable reuse from dired
         (buffer (apply fn (list dir (if ftp short-flags (or r-flags flags))))))
    (unless (or r-flags ftp)
      (setq gnu? (dirvish-gnuls-available-p dir))
      (push (cons remote (if gnu? flags short-flags)) dirvish-tramp-hosts))
    (unless gnu?
      (kill-buffer buffer)
      (setq buffer (apply fn (list dir short-flags))))
    (with-current-buffer buffer
      (dirvish-prop :tramp (tramp-dissect-file-name dir))
      buffer)))

(defun dirvish-tramp--async-p (&optional vec)
  "Return t if tramp connection VEC support async commands."
  (when-let ((vec (or vec (dirvish-prop :tramp))))
    (or (tramp-local-host-p vec)
        (and (tramp-get-method-parameter vec 'tramp-direct-async)
             (tramp-get-connection-property vec "direct-async-process" nil)))))

(defun dirvish-tramp-dir-data-proc-s (proc _exit)
  "Sentinel for `dirvish-data-for-dir''s process PROC."
  (unwind-protect
      (pcase-let* ((`(,dir ,buf ,setup) (process-get proc 'meta))
                   (str (with-current-buffer (process-buffer proc)
                          (substring-no-properties (buffer-string))))
                   (data (split-string str "\n")))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (dirvish-ls-output-parser dir data)
            (when setup (run-hooks 'dirvish-setup-hook))
            (unless (derived-mode-p 'wdired-mode) (dirvish-update-body-h)))))
    (dirvish--kill-buffer (process-buffer proc))))

(cl-defmethod dirvish-data-for-dir
  (dir buffer setup &context ((dirvish-prop :remote) string))
  "DIR BUFFER SETUP DIRVISH-PROP."
  (when (dirvish-tramp--async-p (dirvish-prop :tramp))
    (let* ((process-connection-type nil)
           (buf (dirvish--util-buffer (make-temp-name "dir-data-")))
           (cmd (format "ls -1lahi %s" (file-local-name dir)))
           (proc (start-file-process-shell-command (buffer-name buf) buf cmd)))
      (process-put proc 'meta (list dir buffer setup))
      (set-process-sentinel proc #'dirvish-tramp-dir-data-proc-s))))

(dirvish-define-preview tramp (file _ dv)
  "Preview files with `ls' or `head' for tramp files."
  (let ((vec (dirvish-prop :tramp)))
    (if (not (dirvish-tramp--async-p vec))
        '(info . "File preview is not supported in current connection")
      (let ((process-connection-type nil)
            (localname (file-remote-p file 'localname))
            (buf (dirvish--util-buffer 'preview dv nil t)) proc)
        (when-let ((proc (get-buffer-process buf))) (delete-process proc))
        (setq proc (start-file-process-shell-command
                    (buffer-name buf) buf
                    (format dirvish-tramp-preview-cmd localname localname)))
        (set-process-sentinel
         proc (lambda (proc _sig)
                (when (memq (process-status proc) '(exit signal))
                  (shell-command-set-point-after-cmd (process-buffer proc)))))
        (set-process-filter
         proc (lambda (proc str)
                (with-current-buffer (process-buffer proc)
                  (fundamental-mode)
                  (insert str))))
        `(buffer . ,buf)))))

(defun dirvish-find-file-true-path ()
  "Open truename of (maybe) symlink file under the cursor."
  (interactive)
  (dired-jump nil (file-truename (dired-get-filename nil t))))

(defun dirvish--kill-and-echo (string)
  "Echo last killed STRING."
  (kill-new string)
  (let ((hint (propertize
               "Copied: " 'face 'font-lock-builtin-face)))
    (message "%s" (format "%s%s" hint string))))

(defun dirvish-copy-file-true-path ()
  "Copy truename of (maybe) symlink file under the cursor."
  (interactive)
  (dirvish--kill-and-echo
   (file-truename (dired-get-filename nil t))))

;;;###autoload
(defun dirvish-copy-file-name (&optional multi-line)
  "Copy filename of marked files.
If MULTI-LINE, make every name occupy a new line."
  (interactive "P")
  (let* ((files (dired-get-marked-files t))
         (names (mapconcat #'concat files (if multi-line "\n" " "))))
    (dirvish--kill-and-echo (if multi-line (concat "\n" names) names))))

;;;###autoload
(defun dirvish-copy-file-path (&optional multi-line)
  "Copy filepath of marked files.
If MULTI-LINE, make every path occupy a new line."
  (interactive "P")
  (let* ((files (mapcar #'file-local-name (dired-get-marked-files)))
         (names (mapconcat #'concat files (if multi-line "\n" " "))))
    (dirvish--kill-and-echo (if multi-line (concat "\n" names) names))))

(defun dirvish-copy-remote-path (&optional multi-line)
  "Copy remote path of marked files.
If MULTI-LINE, make every path occupy a new line."
  (interactive "P")
  (let* ((tramp (or (dirvish-prop :tramp) (user-error "Not a remote folder")))
         (files (cl-loop for file in (dired-get-marked-files)
                         for user = (tramp-file-name-user tramp)
                         for host = (tramp-file-name-host tramp)
                         for localname = (file-local-name file)
                         collect (format "%s%s%s:%s" (or user "")
                                         (if user "@" "") host localname)))
         (names (mapconcat #'concat files (if multi-line "\n" " "))))
    (dirvish--kill-and-echo (if multi-line (concat "\n" names) names))))

(defun dirvish-copy-file-directory ()
  "Copy directory name of file under the cursor."
  (interactive)
  (dirvish--kill-and-echo
   (expand-file-name default-directory)))

;;;###autoload
(defun dirvish-total-file-size (&optional fileset)
  "Echo total file size of FILESET.
FILESET defaults to `dired-get-marked-files'."
  (interactive)
  (cl-labels ((f-name (f) (if (not (file-directory-p f)) f
                              (directory-files-recursively f ".*" nil t)))
              (f-size (f) (condition-case nil
                              (file-attribute-size (file-attributes f))
                            ('file-error 0))))
    (let* ((fileset (or fileset (dired-get-marked-files)))
           (count (propertize (number-to-string (length fileset))
                              'face 'font-lock-builtin-face))
           (size (thread-last fileset (mapcar #'f-name) flatten-tree
                              (mapcar #'f-size) (cl-reduce #'+)
                              file-size-human-readable)))
      (message "%s" (format "Total size of %s entries: %s" count size)))))

(defun dirvish-rename-space-to-underscore ()
  "Rename marked files by replacing space to underscore."
  (interactive)
  (require 'dired-aux)
  (if (derived-mode-p 'dired-mode)
      (let ((markedFiles (dired-get-marked-files )))
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              markedFiles)
        (revert-buffer))
    (user-error "Not in a Dired buffer")))

(defun dirvish--marked-files-as-info-string ()
  "Return all marked files as a string."
  (let* ((files (dired-get-marked-files t))
         (count (length files)))
    (cond ((<= count 1)
           (format "current file: %s" (dired-get-filename t t)))
          ((<= count 10)
           (format "marked files:\n  %s" (mapconcat #'concat files "\n  ")))
          (t (format "marked files:\n  %s\n  ... and %s more (%s in total)"
                     (mapconcat #'concat (seq-take files 10) "\n  ")
                     (- count 10) count)))))

;;;###autoload (autoload 'dirvish-file-info-menu "dirvish-extras" nil t)
(transient-define-prefix dirvish-file-info-menu ()
  "Gather file information."
  [:description
   (lambda () (dirvish--format-menu-heading
          "Get File Information"
          (dirvish--marked-files-as-info-string)))
   ("n"   "Copy file NAMEs in one line <n> / multiple lines <C-u n>"
    dirvish-copy-file-name)
   ("p"   "Copy file PATHs in one line <p> / multiple lines <C-u p>"
    dirvish-copy-file-path)
   ("P"   "Copy remote PATHs in one line <P> / multiple lines <C-u P>"
    dirvish-copy-remote-path
    :if (lambda () (dirvish-prop :remote)))
   ("d"   "Copy file DIRECTORY"                dirvish-copy-file-directory)
   ("l"   "Copy symlink's truename"            dirvish-copy-file-true-path
    :if (lambda () (file-symlink-p (dired-get-filename nil t))))
   ("L"   "Go to symlink's truename"           dirvish-find-file-true-path
    :if (lambda () (file-symlink-p (dired-get-filename nil t))))
   ("s"   "Get total size of marked files"     dirvish-total-file-size)
   ("t"   "Show file TYPE"                     dired-show-file-type)])

(transient-define-prefix dirvish-subdir-menu ()
  "Help Menu for Dired subdir management."
  [:description
   (lambda () (dirvish--format-menu-heading "Manage subdirs"))
   ("i" "  Insert subdir"    dired-maybe-insert-subdir :transient t)
   ("k" "  Kill subdir"      dired-kill-subdir :transient t)
   ("n" "  Next subdir"      dired-next-subdir :transient t)
   ("p" "  Prev subdir"      dired-prev-subdir :transient t)
   ("j" "  Jump to subdir"   dired-goto-subdir)
   ("$" "  Hide subdir"      dired-hide-subdir :transient t)
   ("M-$" "Hide all subdirs" dired-hide-all)])

;;;###autoload (autoload 'dirvish-chxxx-menu "dirvish-extras" nil t)
(transient-define-prefix dirvish-chxxx-menu ()
  "Help Menu for file attribute modification commands."
  [:description
   (lambda () (dirvish--format-menu-heading "Modify file's attributes"))
   ("g"   "Change file's GROUP"          dired-do-chgrp)
   ("m"   "Change file's MODE"           dired-do-chmod)
   ("o"   "Change file's OWNER"          dired-do-chown)
   ("t"   "Change file's TIMESTAMP"      dired-do-touch)
   ("p"   "Change file's PATH"           dired-do-rename)])

;;;###autoload (autoload 'dirvish-mark-menu "dirvish-extras" nil t)
(transient-define-prefix dirvish-mark-menu ()
  "Help Menu for `dired-mark/do-*' commands."
  [["Mark or unmark files:"
    ("e" "  by Extension"                dired-mark-extension :transient t)
    ("*" "  by Regexp (file name)"       dired-mark-files-regexp :transient t)
    ("c" "  by Regexp (file content)"    dired-mark-files-containing-regexp :transient t)
    ("s" "  by Subdir"                   dired-mark-subdir-files :transient t)
    ("x" "  by Executable"               dired-mark-executables :transient t)
    ("/" "  by Directory"                dired-mark-directories :transient t)
    ("@" "  by Symlink"                  dired-mark-symlinks :transient t)
    ("&" "  by Garbage"                  dired-flag-garbage-files :transient t)
    ("#" "  by Auto-saved"               dired-flag-auto-save-files :transient t)
    ("~" "  by Backup"                   dired-flag-backup-files :transient t)
    ("." "  by Numerical backup"         dired-clean-directory :transient t)
    ("u" "  Unmark this file"            dired-unmark :transient t)
    ("DEL" "Unmark and move up line"     dired-unmark-backward :transient t)
    ("U" "  Unmark all files"            dired-unmark-all-files :transient t)
    ("t" "  Toggle marks"                dired-toggle-marks :transient t)
    ("n" "  Move to next marked file"    dired-next-marked-file :transient t)
    ("p" "  Move to prev marked file"    dired-prev-marked-file :transient t)]
   ["Actions on marked files:"
    ("O"   "Open"                        dired-do-find-marked-files)
    ("S"   "Symlink"                     dired-do-symlink)
    ("H"   "Hardlink"                    dired-do-hardlink)
    ("P"   "Print"                       dired-do-print)
    ("X"   "Delete flagged"              dired-do-flagged-delete)
    ("r"   "Search file contents"        dired-do-find-regexp)
    ("R"   "Replace file contents"       dired-do-find-regexp-and-replace)
    ("B"   "Byte compile elisp"          dired-do-byte-compile)
    ("L"   "Load elisp"                  dired-do-load)
    ("z"   "Compress to"                 dired-do-compress-to)
    ("Z"   "Compress"                    dired-do-compress)
    ("!"   "Shell command"               dired-do-shell-command)
    ("&"   "Async shell command"         dired-do-async-shell-command)
    ("N"   "Echo number of marked files" dired-number-of-marked-files)
    ("A"   "Modify file's attributes"    dirvish-chxxx-menu)
    ("C"   "Change mark type"            dired-change-marks)
    ("k"   "Kill lines"                  dired-do-kill-lines)]]
  (interactive)
  (require 'dired-x)
  (require 'dired-aux)
  (transient-setup 'dirvish-mark-menu))

(transient-define-prefix dirvish-renaming-menu ()
  "Help Menu for file renaming in Dired."
  [:description
   (lambda () (dirvish--format-menu-heading "File renaming"))
   ("u"   "Upper-case file name"          dired-upcase)
   ("l"   "Lower-case file name"          dired-downcase)
   ("_"   "Replace SPC with UNDERSCORE"   dirvish-rename-space-to-underscore :if-derived 'dirvish-mode)
   ("w"   "Enter wdired [writable dired]" wdired-change-to-wdired-mode :if-not-derived wdired-mode)])

(transient-define-prefix dirvish-epa-dired-menu ()
  "Help menu for `epa-dired-do-*' commands."
  [:description
   (lambda () (dirvish--format-menu-heading "GNUpg assistant"))
   ("e" "Encrypt" epa-dired-do-encrypt)
   ("d" "Decrypt" epa-dired-do-decrypt)
   ("v" "Verify"  epa-dired-do-verify)
   ("s" "Sign"    epa-dired-do-sign)])

;;;###autoload (autoload 'dirvish-dired-cheatsheet "dirvish-extras" nil t)
(transient-define-prefix dirvish-dired-cheatsheet ()
  "A collection of most frequently used Dired commands."
  [:description
   (lambda () (dirvish--format-menu-heading
          "Dired cheatsheet"
          "The keys listed here may be different from the actual bindings"))
   ("n" "  Move to next line"      dired-next-line :transient t)
   ("p" "  Move to prev line"      dired-previous-line :transient t)
   (">" "  Move to next dirline"   dired-next-dirline :transient t)
   ("<" "  Move to prev dirline"   dired-prev-dirline :transient t)
   ("." "  Add an empty file"      dired-create-empty-file)
   ("+" "  Add a directory"        dired-create-directory)
   ("X" "  Delete files"           dired-do-delete)
   ("v" "  View this file"         dired-view-file)
   ("g" "  Refresh buffer"         revert-buffer)
   ("f" "  Find file"              dired-find-file)
   ("o" "  Find file other window" dired-find-file-other-window)
   ("j" "  Go to line for file"    dired-goto-file)
   ("^" "  Go to parent directory" dired-up-directory)
   ("=" "  Compare files"          dired-diff)
   ("(" "  Toggle details"         dired-hide-details-mode)
   ("d" "  Display this file"      dired-display-file)
   ("s" "  Manage subdirs"         dirvish-subdir-menu)
   (":" "  GnuPG helpers"          dirvish-epa-dired-menu)
   ("h" "  More info about Dired"  describe-mode)])

;;;###autoload (autoload 'dirvish-setup-menu "dirvish-extras" nil t)
(defcustom dirvish-ui-setup-items
  '(("s"  file-size      attr     "File size")
    ("t"  file-time      attr     "File modification time")
    ("c"  collapse       attr     "Collapse unique nested paths"
     (not (dirvish-prop :remote)))
    ("v"  vc-state       attr     "Version control state"
     (and (display-graphic-p) (dirvish-prop :vc-backend)))
    ("m"  git-msg        attr     "Git commit messages"
     (and (dirvish-prop :vc-backend) (not (dirvish-prop :remote))))
    ("1" '(0 nil  0.4)   layout   "     -       | current (60%) | preview (40%)")
    ("2" '(0 nil  0.8)   layout   "     -       | current (20%) | preview (80%)")
    ("3" '(1 0.08 0.8)   layout   "parent (8%)  | current (12%) | preview (80%)")
    ("4" '(1 0.11 0.55)  layout   "parent (11%) | current (33%) | preview (55%)"))
  "ITEMs for `dirvish-setup-menu'.
A ITEM is a list consists of (KEY VAR SCOPE DESCRIPTION PRED)
where KEY is the keybinding for the item, VAR can be valid
attribute (as in `dirvish-attributes') or a layout recipe (see
`dirvish-layout-recipes'), SCOPE can be `attr' or `layout'.
DESCRIPTION is the documentation for the VAR.  PRED, when
present, is wrapped with a lambda and being put into the `:if'
keyword in that prefix or infix."
  :group 'dirvish :type 'alist
  :set
  (lambda (k v)
    (set k v)
    (let ((attr-alist (seq-filter (lambda (i) (eq (nth 2 i) 'attr)) v))
          (layout-alist (seq-filter (lambda (i) (eq (nth 2 i) 'layout)) v)))
      (cl-labels ((new-infix (i)
                    (let* ((infix-var (nth 1 i))
                           (infix-name (intern (format "dirvish-%s-infix" (nth 1 i))))
                           (infix-pred (nth 4 i)))
                      (eval `(transient-define-infix ,infix-name ()
                               :class 'dirvish-attribute
                               :variable ',infix-var
                               :description ,(nth 3 i)
                               :if (lambda () ,(if infix-pred `,@infix-pred t))))))
                  (expand-infix (i) (list (car i) (intern (format "dirvish-%s-infix" (nth 1 i)))))
                  (layout-option (i) (list (car i)
                                           (propertize (nth 3 i) 'face 'font-lock-doc-face)
                                           `(lambda () (interactive) (dirvish-switch-layout ,(nth 1 i))))))
        (mapc #'new-infix attr-alist)
        (eval
         `(transient-define-prefix dirvish-setup-menu ()
            "Configure current Dirvish session."
            [:description
             (lambda () (dirvish--format-menu-heading "Setup Dirvish UI"))
             ["Attributes:"
              ,@(mapcar #'expand-infix attr-alist)]]
            ["Switch layouts:"
             :if (lambda () (dv-layout (dirvish-curr)))
             ,@(mapcar #'layout-option layout-alist)]
            ["Actions:"
             ("M-t" "Toggle fullscreen" dirvish-layout-toggle)
             ("RET" "Quit and revert buffer"
              (lambda () (interactive) (dirvish--init-session (dirvish-curr)) (revert-buffer)))]
            (interactive)
            (if dirvish--props
                (transient-setup 'dirvish-setup-menu)
              (user-error "`dirvish-setup-menu' is for Dirvish only"))))))))

(provide 'dirvish-extras)
;;; dirvish-extras.el ends here

;;; dirvish-extras.el --- Extra utilities and transient prefixes for dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provided:
;;
;; Commands:
;; - `dirvish-find-file-true-path'
;; - `dirvish-copy-file-name'
;; - `dirvish-copy-file-path'
;; - `dirvish-copy-file-directory'
;; - `dirvish-total-file-size'
;; - `dirvish-rename-space-to-underscore'
;;
;; Transient prefixes:
;; - `dirvish-file-info-menu'
;; - `dirvish-renaming-menu'
;; - `dirvish-subdir-menu'
;; - `dirvish-chxxx-menu'
;; - `dirvish-mark-menu'
;; - `dirvish-renaming-menu'
;; - `dirvish-epa-dired-menu'
;; - `dirvish-setup-menu'

;;; Code:

(require 'dirvish)
(eval-when-compile (require 'dirvish-tramp))
(declare-function dirvish--count-file-size "dirvish-widgets")

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
    (oset obj value (if (memq sym (dv-attributes (dirvish-curr))) "+" "-"))))

(cl-defmethod transient-infix-read ((obj dirvish-attribute))
  "Read value from DIRVISH-ATTRIBUTE instance OBJ."
  (oset obj value (if (equal (oref obj value) "+") "-" "+")))

(cl-defmethod transient-infix-set ((obj dirvish-attribute) value)
  "Set relevant value in DIRVISH-ATTRIBUTE instance OBJ to VALUE."
  (let* ((dv (dirvish-curr))
         (item (oref obj variable))
         (curr-val (dv-attributes dv))
         (new-val (if (equal value "+") (push item curr-val) (remq item curr-val))))
    (cl-loop for buf in (mapcar #'cdr (dv-roots dv)) do
             (with-current-buffer buf
               (dolist (ov (mapcar #'car (dv-attribute-fns dv)))
                 (remove-overlays (point-min) (point-max) ov t))))
    (setf (dv-attributes dv) new-val)
    (dirvish--refresh-slots dv)
    (dirvish-update-body-h)))

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
  (require 'dirvish-widgets)
  (let* ((fileset (or fileset (dired-get-marked-files)))
         (count (propertize (number-to-string (length fileset))
                            'face 'font-lock-builtin-face))
         (size (file-size-human-readable (dirvish--count-file-size fileset))))
    (message "%s" (format "Total size of %s entries: %s" count size))))

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
   ("t"   "Show file TYPE"                     dired-show-file-type)
   ("m"   "Show media properties"              dirvish-media-properties)])

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
              (lambda () (interactive) (dirvish--build (dirvish-curr)) (revert-buffer)))]
            (interactive)
            (if dirvish--props
                (transient-setup 'dirvish-setup-menu)
              (user-error "`dirvish-setup-menu' is for Dirvish only"))))))))

(provide 'dirvish-extras)
;;; dirvish-extras.el ends here

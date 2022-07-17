;;; dirvish-menu.el --- Transient-based help menu for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.9.23
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; This library is a Dirvish extension which provides transient-based help
;;; menu.  Most commands in the menu are just built-in Dired commands.

;;; Code:

(require 'dirvish)

(defclass dirvish-menu-toggles (transient-infix)
  ((variable  :initarg :variable)
   (scope     :initarg :scope nil))
  "[Experimental] Class for Dirvish toggles.")

(cl-defmethod transient-format-description ((obj dirvish-menu-toggles))
  "Format description for DIRVISH-MENU-TOGGLES instance OBJ."
  (format "%s%s" (oref obj description)
          (propertize " " 'display '(space :align-to (- right 5)))))

(cl-defmethod transient-format-value ((obj dirvish-menu-toggles))
  "Format value for DIRVISH-MENU-TOGGLES instance OBJ."
  (let* ((val (oref obj value))
         (face (if (equal val "+") 'transient-argument 'transient-inactive-value)))
    (propertize val 'face face)))

(cl-defmethod transient-init-value ((obj dirvish-menu-toggles))
  "Initialize value for DIRVISH-MENU-TOGGLES instance OBJ."
  (let ((sym (oref obj variable))
        (scope (funcall (oref obj scope) (dirvish-curr))))
    (oset obj value (if (memq sym scope) "+" "-"))))

(cl-defmethod transient-infix-read ((obj dirvish-menu-toggles))
  "Read value from DIRVISH-MENU-TOGGLES instance OBJ."
  (oset obj value (if (equal (oref obj value) "+") "-" "+")))

(cl-defmethod transient-infix-set ((obj dirvish-menu-toggles) value)
  "Set relevant value in DIRVISH-MENU-TOGGLES instance OBJ to VALUE."
  (let* ((dv (dirvish-curr))
         (item (oref obj variable))
         (slot-name (oref obj scope))
         (curr-val (funcall slot-name dv))
         (new-val (if (equal value "+") (push item curr-val) (remq item curr-val))))
    (cl-loop for buf in (mapcar #'cdr (dv-roots dv)) do
             (with-current-buffer buf
               (dolist (ov (mapcar #'car (dv-attribute-fns dv)))
                 (remove-overlays (point-min) (point-max) ov t))))
    (cl-case slot-name
      ('dv-attributes (setf (dv-attributes dv) new-val))
      ('dv-preview-dispatchers (setf (dv-preview-dispatchers dv) new-val)))
    (dirvish--refresh-slots dv)
    (dirvish-update-body-h)))

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

;;;###autoload (autoload 'dirvish-chxxx-menu "dirvish-menu" nil t)
(transient-define-prefix dirvish-chxxx-menu ()
  "Help Menu for file attribute modification commands."
  [:description
   (lambda () (dirvish--format-menu-heading "Modify file's attributes"))
   ("g"   "Change file's GROUP"          dired-do-chgrp)
   ("m"   "Change file's MODE"           dired-do-chmod)
   ("o"   "Change file's OWNER"          dired-do-chown)
   ("t"   "Change file's TIMESTAMP"      dired-do-touch)
   ("p"   "Change file's PATH"           dired-do-rename)])

;;;###autoload (autoload 'dirvish-mark-menu "dirvish-menu" nil t)
(transient-define-prefix dirvish-mark-menu ()
  "Help Menu for `dired-mark/do-*' commands."
  [["Mark or unmark files:"
    ("e" "  by Extension"                dired-mark-extension :transient t)
    ("*" "  by Regexp (file name)"       dired-mark-files-regexp :transient t)
    ("c" "  by Regexp (file content)"    dired-mark-files-containing-regexp :transient t)
    ("s" "  by Subdir"                   dired-mark-subdir-files :transient t)
    ("E" "  by Executable"               dired-mark-executables :transient t)
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

;;;###autoload (autoload 'dirvish-file-info-menu "dirvish-menu" nil t)
(transient-define-prefix dirvish-file-info-menu ()
  "Gather file information."
  [:description
   (lambda () (dirvish--format-menu-heading
          "Get File Information"
          "C-u n: separate NAMEs into different lines
C-u p: separate PATHs into different lines "))
   ("n"   "Copy file NAMEs"                dirvish-copy-file-name)
   ("p"   "Copy file PATHs"                dirvish-copy-file-path)
   ("d"   "Copy file DIRECTORY"            dirvish-copy-file-directory)
   ("l"   "Copy symlink's truename"        dirvish-copy-file-true-path
    :if (lambda () (file-symlink-p (dired-get-filename nil t))))
   ("L"   "Go to symlink's truename"       dirvish-find-file-true-path
    :if (lambda () (file-symlink-p (dired-get-filename nil t))))
   ("s"   "Get total size of marked files" dirvish-total-file-size)
   ("t"   "Show file TYPE"                 dired-show-file-type)
   ("m"   "Show media properties"          dirvish-media-properties)])

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

;;;###autoload (autoload 'dirvish-setup-menu "dirvish-menu" nil t)
(defcustom dirvish-menu-setup-items
  '(("s"  file-size      attr     "File size")
    ("c"  collapse       attr     "Collapse unique nested paths"
     (or (not (dirvish-prop :tramp)) (tramp-local-host-p (dirvish-prop :tramp))))
    ("v"  vc-state       attr     "Version control state"
     (dirvish-prop :vc-backend))
    ("m"  git-msg        attr     "Git commit messages"
     (and (dirvish-prop :vc-backend) (not (dirvish-prop :tramp))))
    ("d"  vc-diff        preview  "Version control diff")
    ("1" '(0 nil  0.4)   layout   "     -       | current (60%) | preview (40%)")
    ("2" '(0 nil  0.8)   layout   "     -       | current (20%) | preview (80%)")
    ("3" '(1 0.08 0.8)   layout   "parent (8%)  | current (12%) | preview (80%)")
    ("4" '(1 0.11 0.55)  layout   "parent (11%) | current (33%) | preview (55%)"))
  "ITEMs for `dirvish-setup-menu'.
A ITEM is a list consists of (KEY VAR SCOPE DESCRIPTION PRED)
where KEY is the keybinding for the item, VAR can be valid
attribute (as in `dirvish-attributes') or preview dispatcher (as
in `dirvish-preview-dispatchers') or a layout recipe (see
`dirvish-layout-recipes'), SCOPE can be `attr', `preview' or
`layout'.  DESCRIPTION is the documentation for the VAR.  PRED,
when present, is wrapped with a lambda and being put into the
`:if' keyword in that prefix or infix."
  :group 'dirvish :type 'alist
  :set
  (lambda (k v)
    (set k v)
    (let ((attr-alist (seq-filter (lambda (i) (eq (nth 2 i) 'attr)) v))
          (preview-alist (seq-filter (lambda (i) (eq (nth 2 i) 'preview)) v))
          (layout-alist (seq-filter (lambda (i) (eq (nth 2 i) 'layout)) v)))
      (cl-labels ((new-infix (i)
                    (let* ((infix-var (nth 1 i))
                           (infix-name (intern (format "dirvish-%s-infix" (nth 1 i))))
                           (slot-name (pcase (nth 2 i)
                                        ('attr "attributes")
                                        ('preview "preview-dispatchers")))
                           (infix-scope (intern (format "dv-%s" slot-name)))
                           (infix-pred (nth 4 i)))
                      (eval `(transient-define-infix ,infix-name ()
                               :class 'dirvish-menu-toggles
                               :variable ',infix-var
                               :scope ',infix-scope
                               :description ,(nth 3 i)
                               :if (lambda () ,(if infix-pred `,@infix-pred t))))))
                  (expand-infix (i) (list (car i) (intern (format "dirvish-%s-infix" (nth 1 i)))))
                  (layout-option (i) (list (car i)
                                           (propertize (nth 3 i) 'face 'font-lock-doc-face)
                                           `(lambda () (interactive) (dirvish-switch-layout ,(nth 1 i))))))
        (mapc #'new-infix attr-alist)
        (mapc #'new-infix preview-alist)
        (eval
         `(transient-define-prefix dirvish-setup-menu ()
            "Configure current Dirvish session."
            [:description
             (lambda () (dirvish--format-menu-heading "Setup Dirvish UI"))
             ["Attributes:"
              ,@(mapcar #'expand-infix attr-alist)]]
            ["Preview:"
             :if (lambda () (and (not (dirvish-prop :tramp)) (dv-layout (dirvish-curr))))
             ,@(mapcar #'expand-infix preview-alist)]
            ["Switch layouts:"
             :if (lambda () (dv-layout (dirvish-curr)))
             ,@(mapcar #'layout-option layout-alist)]
            ["Actions:"
             ("RET" "Quit and revert buffer"
              (lambda () (interactive) (dirvish--build (dirvish-curr)) (revert-buffer)))]
            (interactive)
            (if dirvish--props
                (transient-setup 'dirvish-setup-menu)
              (user-error "`dirvish-setup-menu' is for Dirvish only"))))))))

(provide 'dirvish-menu)
;;; dirvish-menu.el ends here

;;; dirvish-menu.el --- Transient-based help menu for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.8.14
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
  (or (oref obj description) (symbol-name (oref obj variable))))

(cl-defmethod transient-format-value ((obj dirvish-menu-toggles))
  "Format value for DIRVISH-MENU-TOGGLES instance OBJ."
  (let* ((val (oref obj value))
         (face (if (equal val "On") 'transient-argument 'transient-inactive-value)))
    (propertize val 'face face)))

(cl-defmethod transient-init-value ((obj dirvish-menu-toggles))
  "Initialize value for DIRVISH-MENU-TOGGLES instance OBJ."
  (let ((sym (oref obj variable))
        (scope (funcall (oref obj scope) (dirvish-curr))))
    (oset obj value (if (memq sym scope) "On" "Off"))))

(cl-defmethod transient-infix-read ((obj dirvish-menu-toggles))
  "Read value from DIRVISH-MENU-TOGGLES instance OBJ."
  (oset obj value (if (equal (oref obj value) "On") "Off" "On")))

(cl-defmethod transient-infix-set ((obj dirvish-menu-toggles) value)
  "Set relevant value in DIRVISH-MENU-TOGGLES instance OBJ to VALUE."
  (let* ((dv (dirvish-curr))
         (item (oref obj variable))
         (slot-name (oref obj scope))
         (curr-val (funcall slot-name dv))
         (new-val (if (equal value "On") (push item curr-val) (remq item curr-val))))
    (cl-loop for buf in (mapcar #'cdr (dv-roots dv)) do
             (with-current-buffer buf
               (dolist (ov (mapcar #'car (dv-attribute-fns dv)))
                 (remove-overlays (point-min) (point-max) ov t))))
    (cl-case slot-name
      ('dv-attributes (setf (dv-attributes dv) new-val))
      ('dv-preview-dispatchers (setf (dv-preview-dispatchers dv) new-val)))
    (dirvish--refresh-slots dv)
    (dirvish-update-body-h)))

(defun dirvish-menu--format-heading (string &optional scale)
  "Format STRING as a menu heading.
When CENTER, align it at center.  SCALE defaults to 1.2."
  (setq scale (or scale 1.2))
  (propertize (capitalize string)
              'face '(:inherit dired-mark :underline t)
              'display `((raise (/ (- scale 1) 2)) (height ,scale))))

(transient-define-prefix dirvish-subdir-menu ()
  "Help Menu for Dired subdir management."
  ["Manage subdirs"
   ("i" "  Insert subdir"    dired-maybe-insert-subdir :transient t)
   ("k" "  Kill subdir"      dired-kill-subdir :transient t)
   ("n" "  Next subdir"      dired-next-subdir :transient t)
   ("p" "  Prev subdir"      dired-prev-subdir :transient t)
   ("j" "  Jump to subdir"   dired-goto-subdir)
   ("$" "  Hide subdir"      dired-hide-subdir :transient t)
   ("M-$" "Hide all subdirs" dired-hide-all)])

;;;###autoload (autoload 'dirvish-mark-menu "dirvish-menu" nil t)
(transient-define-prefix dirvish-mark-menu ()
  "Help Menu for `dired-mark-*' commands."
  [["Mark or unmark files:"
    ("e" "  by Extension"                dired-mark-extension :transient t)
    ("%" "  by Regexp (file name)"       dired-mark-files-regexp :transient t)
    ("g" "  by Regexp (file content)"    dired-mark-files-containing-regexp :transient t)
    ("s" "  by Subdir"                   dired-mark-subdir-files :transient t)
    ("*" "  by Executable"               dired-mark-executables :transient t)
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
    ("F"   "Open"                        dired-do-find-marked-files)
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
    ("l"   "Redisplay all marked files"  dired-do-redisplay)
    ("c"   "Change mark type"            dired-change-marks)
    ("k"   "Kill lines"                  dired-do-kill-lines)]]
  ["Change file attributes:"
   ("R"   "Change file's NAME"           dired-do-rename)
   ("G"   "Change file's GROUP"          dired-do-chgrp)
   ("M"   "Change file's MODE"           dired-do-chmod)
   ("O"   "Change file's OWNER"          dired-do-chown)
   ("T"   "Change file's TIMESTAMP"      dired-do-touch)])

;;;###autoload (autoload 'dirvish-file-info-menu "dirvish-menu" nil t)
(transient-define-prefix dirvish-file-info-menu ()
  "Gather file information."
  [:description
   (lambda ()
     (let ((title "get file information")
           (notes "C-u n: separate NAMEs into different lines
C-u p: separate PATHs into different lines "))
       (format "%s\n%s" (dirvish-menu--format-heading title)
               (propertize notes 'face 'font-lock-doc-face))))
   ("n"   "Copy file NAMEs"                dirvish-copy-file-name)
   ("p"   "Copy file PATHs"                dirvish-copy-file-path)
   ("d"   "Copy file DIRECTORY"            dirvish-copy-file-directory)
   ("l"   "Copy symlink's truename"        dirvish-copy-file-true-path
    :if (lambda () (file-symlink-p (dired-get-filename nil t))))
   ("L"   "Go to symlink's truename"       dirvish-find-file-true-path
    :if (lambda () (file-symlink-p (dired-get-filename nil t))))
   ("s"   "Get total size of marked files" dirvish-total-file-size)
   ("t"   "show file TYPE"                 dired-show-file-type)])

(transient-define-prefix dirvish-renaming-menu ()
  "Help Menu for file renaming in Dired."
  ["File renaming"
   ("u"   "Upper-case file name"          dired-upcase)
   ("l"   "Lower-case file name"          dired-downcase)
   ("_"   "Replace SPC with UNDERSCORE"   dirvish-rename-space-to-underscore :if-derived 'dirvish-mode)
   ("w"   "Enter wdired [writable dired]" wdired-change-to-wdired-mode :if-not-derived wdired-mode)])

(transient-define-prefix dirvish-epa-dired-menu ()
  "Help menu for `epa-dired-do-*' commands."
  ["GNUpg assistant"
   ("e" "Encrypt" epa-dired-do-encrypt)
   ("d" "Decrypt" epa-dired-do-decrypt)
   ("v" "Verify"  epa-dired-do-verify)
   ("s" "Sign"    epa-dired-do-sign)])

;;;###autoload (autoload 'dirvish-yank-menu "dirvish-menu" nil t)
(defcustom dirvish-menu-yank-keys
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
        [:if-derived 'dirvish-mode
                     "Select yank operation on marked files:" ,@v]
        (interactive)
        (if (derived-mode-p 'dirvish-mode)
            (transient-setup 'dirvish-yank-menu)
          (user-error "Not in a Dirvish buffer"))))))

;;;###autoload (autoload 'dirvish-setup-menu "dirvish-menu" nil t)
(defcustom dirvish-menu-setup-items
  '(("s"  file-size      attr     "File size")
    ("c"  collapse       attr     "Collapse unique nested paths"
     (or (not (dirvish-prop :tramp)) (tramp-local-host-p (dirvish-prop :tramp))))
    ("v"  vc-state       attr     "Version control state information"
     (dirvish-prop :vc-backend))
    ("m"  git-msg        attr     "Git commit messages"
     (and (dirvish-prop :vc-backend) (not (dirvish-prop :tramp))))
    ("d"  vc-diff        preview  "Version control diff in preview window")
    ("1" '(0 nil  0.4)   layout   "       | CURRENT | preview")
    ("2" '(0 nil  0.8)   layout   "       | current | PREVIEW")
    ("3" '(1 0.08 0.8)   layout   "parent | current | PREVIEW")
    ("4" '(1 0.1  0.6)   layout   "parent | current | preview"))
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
                  (layout-option (i) (list (car i) (nth 3 i)
                                           `(lambda () (interactive) (dirvish-switch-layout ,(nth 1 i)))
                                           :if `(lambda () ,(if (nth 4 i) `,@(nth 4 i) t)))))
        (mapc #'new-infix attr-alist)
        (mapc #'new-infix preview-alist)
        (eval
         `(transient-define-prefix dirvish-setup-menu ()
            "Configure current Dirvish session."
            [:description
             (lambda () (dirvish-menu--format-heading "Setup Dirvish"))
             ["Attributes:"
              ,@(mapcar #'expand-infix attr-alist)]]
            ["Preview:"
             :if (lambda () (and (not (dirvish-prop :tramp)) (dv-layout (dirvish-curr))))
             ,@(mapcar #'expand-infix preview-alist)]
            [:description
             (lambda ()
               (format "%s\n%s"
                       (propertize "Layout:" 'face 'transient-heading)
                       (propertize "Uppercased pane has the biggest size"
                                   'face 'font-lock-doc-face)))
             :if (lambda () (dv-layout (dirvish-curr)))
             ,@(mapcar #'layout-option layout-alist)]
            ["Actions:"
             ("RET" "Quit and revert buffer"
              (lambda () (interactive) (dirvish-build (dirvish-curr)) (revert-buffer)))]
            (interactive)
            (if (or (derived-mode-p 'dirvish-mode)
                    (bound-and-true-p dirvish-override-dired-mode))
                (transient-setup 'dirvish-setup-menu)
              (user-error "`dirvish-setup-menu' is for Dirvish only"))))))))

(provide 'dirvish-menu)
;;; dirvish-menu.el ends here

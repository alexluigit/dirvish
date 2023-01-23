;;; dirvish-quick-access.el --- Quick keys for frequently visited places -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This Dirvish extension allows the user to define a list of frequently visited
;; directories and a quick key to jump to the path.  `dirvish-quick-access' is
;; designed to be a complementary command to the bookmark system in Emacs.  One
;; can have as many as bookmarks they want, and jump to a particular one by the
;; help of their choice of completion framework or commands like
;; `consult-bookmark'.  But for those very frequently visited places in the file
;; system, the user would expect to access these directories with the shortest
;; key sequence, plus a mnemonic way to remember those keys.

;;; Code:

(require 'dirvish)

(defcustom dirvish-quick-access-function 'dirvish-dwim
  "Function used to access `dirvish-quick-access-entries'.
The function takes the entry as the sole argument."
  :group 'dirvish :type 'function)

;;;###autoload (autoload 'dirvish-quick-access "dirvish-quick-access" nil t)
(defcustom dirvish-quick-access-entries
  `(("h" "~/"                  "Home")
    ("e" ,user-emacs-directory "Emacs user directory"))
  "Quick access entries for command `dirvish-quick-access'.
A ENTRY is a (KEY PATH DOC) alist where KEY is the key to
invoke the navigation, PATH is the the argument for command
`dired-jump', DOC (optional) is its documentation string.

Here is a sample value for this variable.

\((\"h\"  \"~/\"                    \"Home\")
 (\"t\"  \"~/.local/share/Trash/\" \"Trashes\")
 (\"pa\" \"~/Code/proj-a/\"        \"Project A\")
 (\"pb\" \"~/Code/proj-b/\"        \"Project B\"))"
  :group 'dirvish :type 'alist
  :set
  (lambda (k v)
    (set k v)
    (when-let* ((desc-len (mapcar (lambda (i) (length (nth 2 i))) v))
                (max-desc-len (seq-max desc-len)))
      (eval
       `(transient-define-prefix dirvish-quick-access ()
          "Jump to Dirvish quick access entries."
          [:description
           (lambda () (dirvish--format-menu-heading "Go to Directory: "))
           ,@(cl-loop
              for (key path desc) in v
              collect
              (list key
                    (concat desc "  "
                            (make-string (- max-desc-len (length desc)) ?\ )
                            (propertize path 'face 'font-lock-comment-face))
                    `(lambda ()
                       (interactive)
                       (funcall dirvish-quick-access-function ,path))))]
          (interactive)
          (require 'dirvish-fd nil t)
          (transient-setup 'dirvish-quick-access))))))

(provide 'dirvish-quick-access)
;;; dirvish-quick-access.el ends here

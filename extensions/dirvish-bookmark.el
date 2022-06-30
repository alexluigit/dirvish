;;; dirvish-bookmark.el --- Bookmark support for Dired/Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.8.14
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Bookmark support for Dired/Dirvish.

;;; Code:

(require 'transient)

(define-obsolete-variable-alias 'dirvish-menu-bookmarks 'dirvish-bookmark-entries "Jun-08,2022")
;;;###autoload (autoload 'dirvish-bookmark-goto "dirvish-bookmark" nil t)
(defcustom dirvish-bookmark-entries
  '(("h" "~/"                          "Home")
    ("d" "~/Downloads/"                "Downloads")
    ("m" "/mnt/"                       "Drives")
    ("t" "~/.local/share/Trash/files/" "TrashCan"))
  "BOOKMARKs for command `dirvish-bookmark-goto'.
A BOOKMARK is a (KEY PATH DOC) alist where KEY is the key to
invoke the navigation, PATH is the the argument for command
`dirvish-find-file', DOC (optional) is the documentation string."
  :group 'dirvish :type 'alist
  :set
  (lambda (k v)
    (set k v)
    (let ((max-desc-len (seq-max (mapcar (lambda (i) (length (nth 2 i))) v))))
      (eval
       `(transient-define-prefix dirvish-bookmark-goto ()
          "Open frequently visited directories using dirvish."
          ["Go to Directory: "
           ,@(cl-loop
              for (key path desc) in v
              collect
              (list key
                    (concat desc "  "
                            (make-string (- max-desc-len (length desc)) ?\ )
                            (propertize path 'face 'font-lock-comment-face))
                    `(lambda ()
                       (interactive)
                       (dired-jump current-prefix-arg ,path))))])))))

(provide 'dirvish-bookmark)
;;; dirvish-bookmark.el ends here

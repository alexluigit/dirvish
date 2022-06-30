;;; dirvish-subtree.el --- Turn Dirvish into a tree browser -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.8.14
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This extension allows users to insert subdirectories in a tree-like fashion,
;; like `dired-subtree' or `treemacs', but simpler and faster.

;;; Code:

(declare-function all-the-icons-octicon "all-the-icons")
(require 'dirvish)

(defvar-local dirvish-subtree--overlays nil "Subtree overlays in this buffer.")

(defcustom dirvish-subtree-line-prefix "  "
  "A string put into each nested subtree.
The prefix is repeated \"depth\" times."
  :type 'string :group 'dirvish
  :set (lambda (k v)
         (set k v)
         (setq dirvish--subtree-prefix-len
               (cond ((bound-and-true-p dired-subtree-line-prefix)
                      (or (ignore-errors (length (bound-and-true-p dired-subtree-line-prefix))) 2))
                     (t (length v))))))

(defcustom dirvish-subtree-save-on-revert t
  "Non-nil means `revert-buffer' keeps all expanded subtrees."
  :type 'boolean :group 'dirvish
  :set (lambda (k v)
         (set k v)
         (funcall (if v #'add-hook #'remove-hook)
                  'dirvish-after-revert-hook #'dirvish-subtree--revert)))

(defcustom dirvish-subtree-always-show-state nil
  "Non-nil means always show the subtree state indicator."
  :type 'boolean :group 'dirvish)

(defvar dirvish-subtree--state-icons nil)
(defcustom dirvish-subtree-state-style 'chevron
  "Icon/string used for directory expanded state.
The value can be one of: `plus', `arrow', `chevron'."
  :group 'dirvish :type 'symbol
  :set
  (lambda (k v)
    (and (eq v 'chevron) (not (require 'all-the-icons nil t)) (setq v 'arrow))
    (set k v)
    (setq dirvish-subtree--state-icons
          (pcase (symbol-value k)
            ('plus (cons (propertize "-" 'face 'dirvish-subtree-state)
                         (propertize "+" 'face 'dirvish-subtree-state)))
            ('arrow (cons (propertize "▾" 'face 'dirvish-subtree-state)
                          (propertize "▸" 'face 'dirvish-subtree-state)))
            ('chevron
             (cons
              (all-the-icons-octicon
               "chevron-down"
               :height (* (or (bound-and-true-p dirvish-all-the-icons-height) 1) 0.8)
               :v-adjust 0.1 :face 'dirvish-subtree-state)
              (all-the-icons-octicon
               "chevron-right"
               :height (* (or (bound-and-true-p dirvish-all-the-icons-height) 1) 0.8)
               :v-adjust 0.1 :face 'dirvish-subtree-state)))))))

(defface dirvish-subtree-state
  '((t (:inherit font-lock-doc-face)))
  "Face used for `expanded-state' attribute."
  :group 'dirvish)

(defun dirvish-curr-dir-ad (fn &optional localp)
  "Advice for FN `dired-current-directory'.
LOCALP is the arg for `dired-current-directory', which see."
  (if-let ((parent (dirvish-subtree--parent))
           (dir (concat (overlay-get parent 'dired-subtree-name) "/")))
      (if localp (dired-make-relative dir default-directory) dir)
    (funcall fn localp)))

(defun dirvish-get-subdir-ad (&rest fn-args)
  "Advice for FN-ARGS `dired-get-subdir'."
  (unless (dirvish-subtree--parent) (apply fn-args)))

(add-to-list 'dirvish-advice-alist '(dired dired-current-directory dirvish-curr-dir-ad))
(add-to-list 'dirvish-advice-alist '(dired dired-get-subdir dirvish-get-subdir-ad))

(defun dirvish-subtree--parent (&optional p)
  "Get the parent subtree overlay at point P."
  (setq p (or p (point)))
  (cl-loop
   with (pov . max) = (cons nil 0)
   for ov in (overlays-at p)
   for depth = (or (overlay-get ov 'dired-subtree-depth) 0) do
   (when (> depth max) (setq pov ov) (setq max depth))
   finally return pov))

(defun dirvish-subtree--readin (dirname)
  "Readin the directory DIRNAME as a string."
  (let ((switches (or dired-actual-switches dired-listing-switches)))
    (with-temp-buffer
      (insert-directory dirname (concat switches " -A") nil t)
      (delete-char -1)
      (goto-char (point-min))
      (delete-region (point) (progn (forward-line 1) (point)))
      (goto-char (point-min))
      (unless (looking-at-p "  ")
        (let ((indent-tabs-mode nil))
          (indent-rigidly (point-min) (point-max) 2)))
      (buffer-string))))

(defun dirvish-subtree--insert ()
  "Insert subtree under this directory."
  (let* ((filename (dired-get-filename))
         (dirname (pcase (progn (back-to-indentation) (char-after)) ; first char in priv
                    (108 (let ((true (file-truename filename))) ; "l" = 108 = symlink
                           (prog1 true (unless (file-directory-p true)
                                         (user-error "Not a directory")))))
                    (100 filename) ; "d" = 100 = directory
                    (_ (user-error "Not a directory"))))
         (listing (dirvish-subtree--readin dirname))
         buffer-read-only beg end)
    (dirvish--print-directory (dirvish-prop :tramp) (current-buffer) dirname t)
    (save-excursion
      (setq beg (progn (move-end-of-line 1) (insert "\n") (point)))
      (setq end (progn (insert listing) (1+ (point)))))
    (let* ((ov (make-overlay beg end))
           (parent (dirvish-subtree--parent (1- beg)))
           (depth (or (and parent (1+ (overlay-get parent 'dired-subtree-depth))) 1)))
      (overlay-put ov 'line-prefix
                   (apply #'concat (make-list depth dirvish-subtree-line-prefix)))
      (overlay-put ov 'dired-subtree-name dirname)
      (overlay-put ov 'dired-subtree-depth depth)
      (overlay-put ov 'evaporate t)
      (push ov dirvish-subtree--overlays))))

(defun dirvish-subtree--remove ()
  "Remove subtree at point."
  (when-let* ((ov (dirvish-subtree--parent))
              (beg (overlay-start ov))
              (end (overlay-end ov)))
    (let ((inhibit-read-only t))
      (goto-char beg)
      (dired-previous-line 1)
      (cl-loop for o in (overlays-in (point-min) (point-max))
               when (and (overlay-get o 'dired-subtree-depth)
                         (>= (overlay-start o) beg)
                         (<= (overlay-end o) end))
               do (setq dirvish-subtree--overlays
                        (delq o dirvish-subtree--overlays)))
      (delete-region (overlay-start ov) (overlay-end ov)))))

(defun dirvish-subtree--revert ()
  "Put the `dired-subtree-overlays' again after buffer reverting."
  (cl-loop
   with st-alist = ()
   for ov in dirvish-subtree--overlays
   for depth = (overlay-get ov 'dired-subtree-depth)
   for name = (overlay-get ov 'dired-subtree-name) do
   (push (cons depth name) st-alist)
   finally do
   (let ((sorted (sort st-alist (lambda (a b) (< (car a) (car b))))))
     (setq dirvish-subtree--overlays nil)
     (cl-loop for (_depth . name) in sorted do
              (when (and (dirvish--goto-file name)
                         (not (dirvish--subtree-expanded-p)))
                (dirvish-subtree--insert))))
   (dirvish--goto-file (dirvish-prop :child))))

(dirvish-define-attribute subtree-state
  "A indicator for directory expanding state."
  (:if (and (eq (dv-root-window dv) (selected-window))
            (or dirvish-subtree-always-show-state
                dirvish-subtree--overlays
                (bound-and-true-p dired-subtree-overlays)))
       :width 1)
  (let ((state-str
         (propertize (if (eq (car f-type) 'dir)
                         (if (dirvish--subtree-expanded-p)
                             (car dirvish-subtree--state-icons)
                           (cdr dirvish-subtree--state-icons))
                       " ")))
        (ov (make-overlay (1+ l-beg) (1+ l-beg))))
    (when hl-face
      (add-face-text-property 0 1 hl-face t state-str))
    (overlay-put ov 'after-string state-str) ov))

(defalias 'dirvish-toggle-subtree #'dirvish-subtree-toggle
  "Insert subtree at point or remove it if it was not present.")
;;;###autoload
(defun dirvish-subtree-toggle ()
  "Insert subtree at point or remove it if it was not present."
  (interactive)
  (if (dirvish--subtree-expanded-p)
      (progn (dired-next-line 1) (dirvish-subtree--remove))
    (dirvish-subtree--insert)))

(provide 'dirvish-subtree)
;;; dirvish-subtree.el ends here

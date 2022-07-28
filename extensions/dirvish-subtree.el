;;; dirvish-subtree.el --- Turn Dirvish into a tree browser -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.9.23
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This extension allows users to insert subdirectories in a tree-like fashion,
;; like `dired-subtree' or `treemacs', but simpler and faster.

;;; Code:

(declare-function all-the-icons-octicon "all-the-icons")
(require 'dirvish)

(defvar dirvish-subtree--prefix-unit-len 2)
(defvar-local dirvish-subtree--overlays nil "Subtree overlays in this buffer.")

(defcustom dirvish-subtree-line-prefix "  "
  "A string put into each nested subtree.
The prefix is repeated \"depth\" times."
  :type 'string :group 'dirvish
  :set (lambda (k v)
         (set k v)
         (setq dirvish-subtree--prefix-unit-len
               (cond ((bound-and-true-p dired-subtree-line-prefix)
                      (or (ignore-errors (length (bound-and-true-p dired-subtree-line-prefix))) 2))
                     (t (length v))))))

(defcustom dirvish-subtree-save-on-revert t
  "Non-nil means `revert-buffer' keeps all expanded subtrees."
  :type 'boolean :group 'dirvish
  :set (lambda (k v)
         (set k v)
         (if v (add-hook 'dirvish-setup-hook #'dirvish-subtree--revert 10)
           (remove-hook 'dirvish-setup-hook #'dirvish-subtree--revert))))

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
  (if-let* ((parent (dirvish-subtree--parent))
            (dir (concat (overlay-get parent 'dired-subtree-name) "/")))
      (if localp (dired-make-relative dir default-directory) dir)
    (funcall fn localp)))

(defun dirvish-get-subdir-ad (&rest fn-args)
  "Advice for FN-ARGS `dired-get-subdir'."
  (unless (dirvish-subtree--parent) (apply fn-args)))

(setq dirvish-advice-alist
      (append dirvish-advice-alist
              '((advice dired-current-directory dirvish-curr-dir-ad   :around)
                (advice dired-get-subdir        dirvish-get-subdir-ad :around))))
(when dirvish-override-dired-mode
  (dirvish-override-dired-mode -1)
  (dirvish-override-dired-mode 1))

(defun dirvish-subtree--goto-file (filename)
  "Go to line describing FILENAME."
  (goto-char (point-min))
  (let (stop)
    (while (and (not stop)
                (= (forward-line) 0))
      (when (equal filename (dired-get-filename nil t))
        (setq stop t)
        (dired-move-to-filename)))
    stop))

(defun dirvish-subtree--prefix-length ()
  "Calculate subtree prefix length at point."
  (* dirvish-subtree--prefix-unit-len (dirvish-subtree--depth)))

(defun dirvish-subtree--depth ()
  "Get subtree depth at point."
  (let ((dps (cl-loop for ov in (overlays-at (point)) collect
                      (or (overlay-get ov 'dired-subtree-depth) 0))))
    (or (and dps (apply #'max dps)) 0)))

(defun dirvish-subtree--expanded-p ()
  "70x Faster version of `dired-subtree--is-expanded-p'."
  (save-excursion (< (dirvish-subtree--depth)
                     (progn (forward-line 1) (dirvish-subtree--depth)))))

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
      (insert-directory (file-name-as-directory dirname)
                        (concat switches " -A") nil t)
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
  (let* ((dirname (dired-get-filename))
         (listing (dirvish-subtree--readin dirname))
         buffer-read-only beg end)
    (dirvish--print-directory (dirvish-prop :tramp) (current-buffer) dirname t)
    (with-silent-modifications
      (save-excursion
        (setq beg (progn (move-end-of-line 1) (insert "\n") (point)))
        (setq end (progn (insert listing) (1+ (point))))))
    (let* ((ov (make-overlay beg end))
           (parent (dirvish-subtree--parent (1- beg)))
           (depth (or (and parent (1+ (overlay-get parent 'dired-subtree-depth))) 1)))
      (overlay-put ov 'line-prefix
                   (apply #'concat (make-list depth dirvish-subtree-line-prefix)))
      (overlay-put ov 'dired-subtree-name dirname)
      (overlay-put ov 'dired-subtree-depth depth)
      (overlay-put ov 'evaporate t)
      (push ov dirvish-subtree--overlays))))

(defun dirvish-subtree--revert (&optional clear)
  "Put the `dired-subtree-overlays' again after buffer reverting.
When CLEAR, remove all subtrees in the buffer."
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
              (when (dirvish-subtree--goto-file name)
                (cond (clear
                       (dired-next-line 1)
                       (dirvish-subtree-remove))
                      ((not (dirvish-subtree--expanded-p))
                       (dirvish-subtree--insert))))))
   (dirvish-subtree--goto-file (dirvish-prop :child))))

(dirvish-define-attribute subtree-state
  "A indicator for directory expanding state."
  (:if (and (eq (dv-root-window dv) (selected-window))
            (or dirvish-subtree-always-show-state
                dirvish-subtree--overlays
                (bound-and-true-p dired-subtree-overlays)))
       :width 1)
  (let ((state-str
         (propertize (if (eq (car f-type) 'dir)
                         (if (dirvish-subtree--expanded-p)
                             (car dirvish-subtree--state-icons)
                           (cdr dirvish-subtree--state-icons))
                       " ")))
        (ov (make-overlay (1+ l-beg) (1+ l-beg))))
    (when hl-face
      (add-face-text-property 0 1 hl-face t state-str))
    (overlay-put ov 'after-string state-str) ov))

;;;###autoload
(defun dirvish-subtree-up ()
  "Jump to beginning of current subtree."
  (interactive)
  (when-let ((ov (dirvish-subtree--parent)))
    (goto-char (overlay-start ov))
    (dired-previous-line 1)))

;;;###autoload
(defun dirvish-subtree-remove ()
  "Remove subtree at point."
  (interactive)
  (when-let* ((ov (dirvish-subtree--parent))
              (beg (overlay-start ov))
              (end (overlay-end ov)))
    (goto-char beg)
    (dired-previous-line 1)
    (cl-loop for o in (overlays-in (point-min) (point-max))
             when (and (overlay-get o 'dired-subtree-depth)
                       (>= (overlay-start o) beg)
                       (<= (overlay-end o) end))
             do (setq dirvish-subtree--overlays
                      (delq o dirvish-subtree--overlays)))
    (with-silent-modifications
      (delete-region (overlay-start ov) (overlay-end ov)))))

;;;###autoload
(defun dirvish-subtree-clear ()
  "Clear all subtrees in the buffer."
  (interactive)
  (dirvish-subtree--revert t)
  (goto-char (point-min)))

(defalias 'dirvish-toggle-subtree #'dirvish-subtree-toggle
  "Insert subtree at point or remove it if it was not present.")
;;;###autoload
(defun dirvish-subtree-toggle ()
  "Insert subtree at point or remove it if it was not present."
  (interactive)
  (if (dirvish-subtree--expanded-p)
      (progn (dired-next-line 1) (dirvish-subtree-remove))
    (dirvish-subtree--insert)))

(provide 'dirvish-subtree)
;;; dirvish-subtree.el ends here

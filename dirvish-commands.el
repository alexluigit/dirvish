;;; dirvish-commands.el --- Utility dirvish commands -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides extra utility commands in Dirvish.

;;; Code:

(require 'dirvish-builder)
(require 'ring)
(require 'dired)

(defun dirvish-find-file-true-path ()
  "Open truename of (maybe) symlink file under the cursor."
  (interactive)
  (dired-jump nil (file-truename (dired-get-filename))))

(defun dirvish-copy-file-name ()
  "Copy filename under the cursor."
  (interactive)
  (message "Copied file NAME: %s" (dired-copy-filename-as-kill)))

(defun dirvish-copy-file-path ()
  "Copy filename under the cursor."
  (interactive)
  (message "Copied file PATH: %s" (kill-new (dired-get-filename nil t))))

(defun dirvish-copy-file-directory ()
  "Copy the current directory's (`default-directory''s) absolute path."
  (interactive)
  (message "Copied file DIRECTORY: %s" (kill-new (expand-file-name default-directory))))

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

(defun dirvish-browse-all-directories ()
  "Browse all directories using `fd' command."
  (interactive)
  (unless (executable-find "fd") (user-error "Dirvish: install `fd' to use this command"))
  (let* ((command "fd -H -td -0 . /")
         (output (shell-command-to-string command))
         (files-raw (split-string output "\0" t))
         (files (dirvish--append-metadata 'file files-raw))
         (file (completing-read "Goto: " files)))
    (dired-jump nil file)))

(defun dirvish-show-history (&optional history)
  "Select a target directory from HISTORY and open it in Dirvish."
  (interactive)
  (setq history (or history (ring-elements dirvish-history-ring)))
  (let* ((history-w/metadata (dirvish--append-metadata 'file history))
         (result (completing-read "Recently visited: " history-w/metadata)))
      (when result (dirvish-find-file result))))

(defun dirvish-other-buffer ()
  "Switch to the most recently visited dirvish buffer."
  (interactive)
  (dirvish-find-file (ring-ref dirvish-history-ring 1)))

(defun dirvish-up-directory (&optional other-window)
  "Run Dirvish on parent directory of current directory.
If OTHER-WINDOW (the optional prefix arg), display the parent
directory in another window."
  (interactive "P")
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish--get-parent current)))
    (if (string= parent current)
        (user-error "Dirvish: you're in root directory")
      (if other-window
          (progn
            (and other-window (switch-to-buffer-other-window dirvish-temp-buffer))
            (dirvish-here parent :depth -1))
        (dirvish-find-file parent t)))))

(defun dirvish-sort-by-criteria (criteria)
  "Call `dired-sort-other' by different `CRITERIA'."
  (interactive
   (list
    (read-char-choice
     "Sort by (d/D)efault (e/E)xt (s/S)ize (t/T)ime (m/M)odified: "
     '(?q ?d ?D ?e ?E ?s ?S ?t ?T ?m ?M))))
  (when dired-sort-inhibit (user-error "Dirvish: cannot sort this buffer"))
  (unless (eq criteria ?q)
    (let* ((c (char-to-string criteria))
           (revp (string-equal c (upcase c)))
           (cc (downcase c))
           (sort-flag
            (cond
             ((string-equal cc "d") '("default" . ""))
             ((string-equal cc "m") '("modified" . " -c"))
             ((string-equal cc "e") '("ext" . " -X"))
             ((string-equal cc "t") '("time" . " -t"))
             ((string-equal cc "s") '("size" . " -S"))))
           (name (concat (car sort-flag) (when revp " [R]")))
           (order (concat (cdr sort-flag) (when revp " -r")))
           (dv (dirvish-curr)))
      (setf (dv-sort-criteria dv) (cons name order))
      (dired-sort-other (string-join (list (dv-ls-switches dv) order) " ")))))

(defun dirvish-toggle-fullscreen ()
  "Toggle fullscreen of current Dirvish."
  (interactive)
  (if-let* ((dv (dirvish-curr))
            (old-depth (dv-depth dv))
            (fs-depth (dv-fullscreen-depth dv))
            (new-depth (if (eq old-depth -1) fs-depth -1))
            (buf (current-buffer)))
      (progn
        (dirvish-drop)
        (if (dirvish-dired-p dv)
            (with-selected-window (dv-root-window dv)
              (let (quit-window-hook) (quit-window)))
          (set-window-configuration (dv-window-conf dv)))
        (setf (dv-depth dv) new-depth)
        (setf (dv-window-conf dv) (current-window-configuration))
        (with-selected-window (dirvish--create-root-window dv)
          (switch-to-buffer buf)
          (dirvish-reclaim)
          (dirvish-build)
          (dirvish-update-body-h)))
    (user-error "Dirvish: not in a dirvish buffer")))

(defun dirvish-find-file (&optional file ignore-hist)
  "Find file in dirvish buffer.

FILE can be a file or a directory, if nil then infer entry from
variable `buffer-file-name'.  If IGNORE-HIST is non-nil, do not
update `dirvish-history-ring'."
  (interactive)
  (let* ((entry (or file (dired-get-filename nil t)))
         (bname (buffer-file-name (current-buffer)))
         (curr-dir (expand-file-name default-directory))
         (dv (dirvish-curr))
         (dv-tran (dv-transient dv))
         (dv-depth (dv-depth dv)))
    (when entry
      (if (file-directory-p entry)
          (let* ((entry (file-name-as-directory (expand-file-name entry)))
                 (hist (directory-file-name entry))
                 enable-dir-local-variables)
            (unless ignore-hist
              (when (or (ring-empty-p dirvish-history-ring)
                        (not (eq hist (ring-ref dirvish-history-ring 0))))
                (ring-insert dirvish-history-ring hist)))
            (switch-to-buffer (dirvish--buffer-for-dir dv entry))
            (setq dirvish--child-entry (or bname curr-dir))
            (when (dirvish-p dv-tran)
              (dirvish-activate
               (dirvish-new
                 :depth dv-depth
                 :transient (dv-name dv-tran))))
            (dirvish-build))
        (find-file entry)))))

(defun dirvish-noselect (dir)
  "Return the Dirvish buffer at DIR, do not select it."
  (or dir (setq dir default-directory))
  (let ((dv (dirvish-activate (dirvish-new :depth -1))))
    (with-current-buffer (dirvish--buffer-for-dir dv dir)
      (dirvish-build)
      (current-buffer))))

(provide 'dirvish-commands)
;;; dirvish-commands.el ends here

;;; dirvish-commands.el --- Utility dirvish commands -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides extra utility commands in Dirvish.

;;; Code:

(require 'dirvish-builder)

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
            (switch-to-buffer-other-window (dirvish--ensure-temp-buffer))
            (dirvish-activate (dirvish-new :path (dirvish--ensure-path parent) :depth -1)))
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
        (setq-local dirvish--child-entry (dv-index-path dv))
        (with-selected-window (dirvish--create-root-window dv)
          (switch-to-buffer buf)
          (dirvish-reclaim)
          (dirvish-build)))
    (user-error "Dirvish: not in a dirvish buffer")))

(defun dirvish-find-file (&optional file ignore-hist)
  "Find file in dirvish buffer.
FILE can be a file or a directory, if nil then infer entry from
variable `buffer-file-name'.  If IGNORE-HIST is non-nil, do not
update `dirvish--history-ring'."
  (interactive)
  (let* ((entry (or file (dired-get-filename nil t)))
         (bname (buffer-file-name (current-buffer)))
         (dv (dirvish-curr))
         (dv-tran (dv-transient dv))
         (dv-depth (dv-depth dv)))
    (when entry
      (if (file-directory-p entry)
          (let* ((entry (file-name-as-directory (expand-file-name entry)))
                 (hist (directory-file-name entry))
                 enable-dir-local-variables)
            (when (and dirvish--history-ring (not ignore-hist))
              (when (or (ring-empty-p dirvish--history-ring)
                        (not (eq hist (ring-ref dirvish--history-ring 0))))
                (ring-insert dirvish--history-ring hist)))
            (switch-to-buffer (dirvish--buffer-for-dir dv entry))
            (setq dirvish--child-entry (or bname (expand-file-name default-directory)))
            (when (dirvish-p dv-tran)
              (dirvish-activate
               (dirvish-new
                 :depth dv-depth
                 :transient (dv-name dv-tran))))
            (dirvish-build))
        (find-file entry)))))

(provide 'dirvish-commands)
;;; dirvish-commands.el ends here

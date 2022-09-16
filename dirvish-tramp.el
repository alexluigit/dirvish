;;; dirvish-tramp.el --- Dirvish integration for TRAMP -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'dirvish)
(require 'tramp)

(defconst dirvish-tramp--preview-cmd
  "head -n 1000 %s 2>/dev/null || ls -Alh --group-directories-first %s 2>/dev/null")
(defvar dirvish-tramp-hosts '())

(defun dirvish-tramp--ls-parser (entry output)
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

(defun dirvish-tramp--gnuls-available-p (dir)
  "Check if GNU ls is available or not over the remote DIR."
  (with-temp-buffer
    (cl-letf (((symbol-function 'display-message-or-buffer) #'ignore))
      (let ((default-directory dir))
        (= (tramp-handle-shell-command "ls --version") 0)))))

(defun dirvish-tramp--noselect (fn dir flags remote)
  "Return the Dired buffer at DIR with listing FLAGS.
Save the REMOTE host to `dirvish-tramp-hosts'.
FN is the original `dired-noselect' closure."
  (let* ((r-flags (cdr (assoc remote dirvish-tramp-hosts #'equal)))
         (ftp (tramp-ftp-file-name-p dir))
         (short-flags "-Alh")
         (gnu? t)
         (buffer (apply fn (list dir (if ftp short-flags (or r-flags flags))))))
    (unless (or r-flags ftp)
      (setq gnu? (dirvish-tramp--gnuls-available-p dir))
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
            (dirvish-tramp--ls-parser dir data)
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

(cl-defmethod dirvish-readin-dir
  (dir &context ((dirvish-prop :remote) string) &optional flags)
  "DIR FLAGS DIRVISH-PROP."
  (let* ((ftp (tramp-ftp-file-name-p (dirvish-prop :tramp)))
         (flags (if ftp "-Alh"
                  (or flags dired-actual-switches dired-listing-switches))))
    (with-temp-buffer
      (insert-directory (file-name-as-directory dir) flags nil t)
      (delete-char -1)
      (unless ftp
        (delete-region (goto-char (point-min))
                       (progn (forward-line 1) (point))))
      (unless (looking-at-p "  ")
        (let ((indent-tabs-mode nil))
          (indent-rigidly (point-min) (point-max) 2)))
      (buffer-string))))

(defun dirvish-tramp--preview-handler (dv file vec)
  "Preview handler for remote FILE in DV.
VEC is the tramp file name structure for current directory."
  (if (dirvish-tramp--async-p vec)
      (let ((process-connection-type nil)
            (localname (file-remote-p file 'localname))
            (buf (dirvish--util-buffer 'preview dv nil t)) proc)
        (when-let ((proc (get-buffer-process buf))) (delete-process proc))
        (setq proc (start-file-process-shell-command
                    (buffer-name buf) buf
                    (format dirvish-tramp--preview-cmd localname localname)))
        (set-process-sentinel
         proc (lambda (proc _sig)
                (when (memq (process-status proc) '(exit signal))
                  (shell-command-set-point-after-cmd (process-buffer proc)))))
        (set-process-filter
         proc (lambda (proc str)
                (with-current-buffer (process-buffer proc)
                  (fundamental-mode)
                  (insert str))))
        `(buffer . ,buf))
    '(info . "File preview is not supported in current TRAMP connection")))

(provide 'dirvish-tramp)
;;; dirvish-tramp.el ends here

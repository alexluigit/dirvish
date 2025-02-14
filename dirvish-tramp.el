;;; dirvish-tramp.el --- Dirvish tramp integration  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.1.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;  Handle directory opening, file attributes retrieving and file preview on
;;  TRAMP connections within Dirvish.  This library is only loaded after a TRAMP
;;  connection is initiated, which speeds up the package loading.

;;; Code:

(require 'dirvish)
(require 'tramp)

(defconst dirvish-tramp-preview-cmd
  "head -n 1000 %s 2>/dev/null || ls -Alh --group-directories-first %s 2>/dev/null")
(defvar dirvish-tramp-hosts '())

(defun dirvish-tramp-noselect (fn dir flags remote)
  "Return the Dired buffer at DIR with listing FLAGS.
Save the REMOTE host to `dirvish-tramp-hosts'.
FN is the original `dired-noselect' closure."
  (let* ((saved-flags (cdr (assoc remote dirvish-tramp-hosts #'equal)))
         (ftp? (tramp-ftp-file-name-p dir))
         (short-flags "-Alh")
         (default-directory dir)
         (dired-buffers nil)
         (buffer (cond (ftp? (funcall fn dir short-flags))
                       (saved-flags (funcall fn dir saved-flags))
                       ((= (process-file "ls" nil nil nil "--version") 0)
                        (push (cons remote flags) dirvish-tramp-hosts)
                        (funcall fn dir flags))
                       (t (push (cons remote short-flags) dirvish-tramp-hosts)
                          (funcall fn dir short-flags)))))
    (with-current-buffer buffer
      (dirvish-prop :tramp (tramp-dissect-file-name dir))
      buffer)))

(defun dirvish-tramp--async-p (vec)
  "Return t if tramp connection VEC support async commands."
  (or (tramp-local-host-p vec) ; localhost
      ;; the connection support `direct-async-process' and no password needed
      (and (stringp (tramp-get-connection-property
                     vec "first-password-request" nil))
           (tramp-get-method-parameter vec 'tramp-direct-async)
           (tramp-get-connection-property vec "direct-async-process" nil))))

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

(defun dirvish-tramp-dir-data-proc-s (proc _exit)
  "Sentinel for `dirvish-data-for-dir''s process PROC."
  (unwind-protect
      (pcase-let* ((`(,dir ,buf ,inhibit-setup) (process-get proc 'meta))
                   (str (with-current-buffer (process-buffer proc)
                          (substring-no-properties (buffer-string))))
                   (data (split-string str "\n")))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (dirvish-tramp--ls-parser dir data)
            (unless inhibit-setup (run-hooks 'dirvish-setup-hook))
            (unless (derived-mode-p 'wdired-mode) (dirvish-update-body-h)))))
    (dirvish--kill-buffer (process-buffer proc))))

(cl-defmethod dirvish-data-for-dir
  (dir buffer inhibit-setup &context ((dirvish-prop :remote) string))
  "Fetch data for DIR in BUFFER.
It is called when DIRVISH-PROP has key `:remote' as a string, which
means DIR is in a remote host.  Run `dirvish-setup-hook' after data
parsing unless INHIBIT-SETUP is non-nil."
  (when (dirvish-tramp--async-p (dirvish-prop :tramp))
    (let* ((process-connection-type nil)
           (buf (dirvish--util-buffer (make-temp-name "dir-tramp-")))
           (cmd (format "ls -1lahi %s" (file-local-name dir)))
           (proc (start-file-process-shell-command (buffer-name buf) buf cmd)))
      (process-put proc 'meta (list dir buffer inhibit-setup))
      (set-process-sentinel proc #'dirvish-tramp-dir-data-proc-s))))

(dirvish-define-preview tramp (file _ dv)
  "Preview files with `ls' or `head' for tramp files."
  (let ((vec (dirvish-prop :tramp)))
    (if (not (dirvish-tramp--async-p vec))
        '(info . "File preview is not supported in current connection")
      (let ((process-connection-type nil)
            (localname (file-remote-p file 'localname))
            (buf (dirvish--util-buffer 'preview dv nil t)) proc)
        (when-let* ((proc (get-buffer-process buf))) (delete-process proc))
        (setq proc (start-file-process-shell-command
                    (buffer-name buf) buf
                    (format dirvish-tramp-preview-cmd localname localname)))
        (set-process-sentinel
         proc (lambda (proc _sig)
                (when (memq (process-status proc) '(exit signal))
                  (shell-command-set-point-after-cmd (process-buffer proc)))))
        (set-process-filter
         proc (lambda (proc str)
                (with-current-buffer (process-buffer proc)
                  (fundamental-mode)
                  (insert str))))
        `(buffer . ,buf)))))

(provide 'dirvish-tramp)
;;; dirvish-tramp.el ends here

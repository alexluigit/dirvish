;;; dirvish-tramp.el --- Dirvish tramp integration  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.3.0
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

;; TODO: we don't have to use -Alh if the connection has GNU ls
(defconst dirvish-tramp-preview-cmd
  "head -n 1000 %s 2>/dev/null || ls -Alh %s 2>/dev/null")
(defvar dirvish-tramp-hosts '())

(defun dirvish-tramp-noselect (fn dir flags remote local-dispatchers)
  "Return the Dired buffer at DIR with listing FLAGS.
Save the REMOTE host to `dirvish-tramp-hosts'.
FN is the original `dired-noselect' closure."
  (let* ((saved-flags (cdr (assoc remote dirvish-tramp-hosts #'equal)))
         (short-flags "-Alh")
         (default-directory dir)
         (vec (tramp-dissect-file-name dir))
         (async-type (dirvish-tramp--async-p vec))
         (gnuls "ls")
         (dired-buffers nil) ; disable reuse from `dired'
         (buffer (cond ((eq async-type 'local) (funcall fn dir flags))
                       (saved-flags (funcall fn dir saved-flags)) ; skip
                       ((= (or (process-file gnuls nil nil nil "--version") 1) 0)
                        (push (cons remote flags) dirvish-tramp-hosts)
                        (funcall fn dir flags))
                       (t (setq gnuls nil)
                          (push (cons remote short-flags) dirvish-tramp-hosts)
                          (funcall fn dir short-flags)))))
    (with-current-buffer buffer
      (dirvish-prop :gnuls gnuls)
      (cond ((eq async-type 'local)
             (dirvish-prop :sudo 1)
             (dirvish-prop :preview-dps local-dispatchers))
            ((eq async-type 'async)
             (dirvish-prop :remote-async 1)
             (dirvish-prop :preview-dps '(dirvish-tramp-dp)))
            (t (dirvish-prop :preview-dps '(dirvish-tramp-unsupported-dp))))
      (dirvish-prop :tramp vec)
      buffer)))

(defun dirvish-tramp--async-p (vec)
  "Return t if tramp connection VEC support async commands."
  (cond ((tramp-local-host-p vec) 'local) ; the connection is either localhost
        ;; or it's a remote host that supports `direct-async'
        ((tramp-direct-async-process-p) 'async)))

(defun dirvish-tramp--ls-parser (entry output)
  "Parse ls OUTPUT for ENTRY and store it in `dirvish--dir-data'."
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
        (puthash (secure-hash 'md5 (expand-file-name f-name entry))
                 `(:builtin ,(list f-type lnum user group nil
                                   f-mtime nil size priv nil inode)
                            :type ,(cons (if f-dirp 'dir 'file) f-truename))
                 dirvish--dir-data)))))

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
            (dirvish--redisplay))))
    (dirvish--kill-buffer (process-buffer proc))))

(cl-defmethod dirvish-data-for-dir
  (dir buffer inhibit-setup
       &context ((dirvish-prop :remote-async) number)
       &context ((dirvish-prop :gnuls) string))
  "Fetch data for DIR in BUFFER.
It is called when DIRVISH-PROP has key `:remote-aysnc' and `:gnuls',
which means DIR is opened over a remote host that supports
`direct-async' and comes with valid gnuls executable.  Run
`dirvish-setup-hook' after data parsing unless INHIBIT-SETUP is non-nil."
  (let* ((process-connection-type nil)
         (buf (get-buffer-create (make-temp-name "tramp-data-")))
         (cmd (format "%s -1lahi %s" (dirvish-prop :gnuls)
                      (file-local-name dir)))
         (proc (start-file-process-shell-command (buffer-name buf) buf cmd)))
    (process-put proc 'meta (list dir buffer inhibit-setup))
    (set-process-sentinel proc #'dirvish-tramp-dir-data-proc-s)))

(dirvish-define-preview tramp-unsupported ()
  "Preview files with `ls' or `head' for tramp files."
  (let ((msg "File preview is not supported in this connection.
  1. Please check if you have GNU ls installed over remote host.
  2. Adjust your `direct-async' tramp settings, for example:

    ;; set `tramp-direct-async-process' locally in all ssh connections
    (connection-local-set-profile-variables
     'remote-direct-async-process
     '((tramp-direct-async-process . t)))
    (connection-local-set-profiles
     '(:application tramp :protocol \"ssh\")
     'remote-direct-async-process)

  See (info \"(tramp) Improving performance of asynchronous remote processes\") for details."))
    `(info . ,msg)))

(dirvish-define-preview tramp (file _ dv)
  "Preview files with `ls' or `head' for tramp files."
  (let ((process-connection-type nil)
        (buf (dirvish--special-buffer 'preview dv t)) proc)
    (when-let* ((proc (get-buffer-process buf))) (delete-process proc))
    (setq proc (start-file-process-shell-command
                (buffer-name buf) buf
                (format dirvish-tramp-preview-cmd file file)))
    (set-process-sentinel
     proc (lambda (proc _sig)
            (when (memq (process-status proc) '(exit signal))
              (shell-command-set-point-after-cmd (process-buffer proc)))))
    (set-process-filter
     proc (lambda (proc str)
            (when-let* ((b (process-buffer proc)) ((buffer-live-p b)))
              (with-current-buffer b (let (buffer-read-only) (insert str))))))
    `(buffer . ,buf)))

(provide 'dirvish-tramp)
;;; dirvish-tramp.el ends here

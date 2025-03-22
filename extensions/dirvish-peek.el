;;; dirvish-peek.el --- Minibuffer file preview powered by Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.2.7
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This extension introduces `dirvish-peek-mode', a minor mode that enables file
;; previews within the minibuffer as you narrow down candidates.  By leveraging
;; `dirvish.el' for its core functionality, it delivers a seamless and
;; consistent preview experience.

;;; Code:

(declare-function vertico--candidate "vertico")
(declare-function ivy-state-current "ivy")
(defvar ivy-last)
(require 'dirvish)
(require 'find-func)

(defcustom dirvish-peek-candidate-fetcher nil
  "Function to get current candidate in minibuffer.
If this value is nil, a candidate fetcher function is
automatically choosed according to your completion framework
being used at runtime."
  :group 'dirvish :type '(choice function (const nil)))

(defcustom dirvish-peek-categories '(file project-file library)
  "Minibuffer metadata categories to show file preview.
For now only `file', `project-file' and `library' are supported.

  - `file':         preview files on `find-file' command and friends.
  - `project-file': preview files on `project-find-file' command and friends.
  - `library':      preview files on `find-library' command.

Notice that the `dirvish-preview-dispatchers' option is respected across
all categories."
  :group 'dirvish :type '(repeat :tag "each item can be 'file 'project-file 'library" symbol))

(defun dirvish-peek--prepare-cand-fetcher ()
  "Set candidate fetcher according to current completion framework."
  (dirvish-prop :peek-fetcher
    (cond (dirvish-peek-candidate-fetcher dirvish-peek-candidate-fetcher)
          ((bound-and-true-p vertico-mode) #'vertico--candidate)
          ((bound-and-true-p ivy-mode) (lambda () (ivy-state-current ivy-last)))
          ((bound-and-true-p icomplete-mode)
           (lambda () (car completion-all-sorted-completions))))))

(dirvish-define-preview peek-exception (file)
  "Handle exceptions when peek files."
  (cond ((string-prefix-p "LIB_EXCEPTION:::" file)
         (pcase-let ((`(_ ,cand ,err) (split-string file ":::"))
                     (fmt "Caught exception peeking [ %s ]\n    Error: %s"))
           `(info . ,(format fmt cand err))))
        ((string-prefix-p "FILE_REMOTE_EXCEPTION:::" file)
         (pcase-let ((`(_ ,cand) (split-string file ":::")))
           `(info . ,(format "Unable to peek remote file: [ %s ]" cand))))))

(defun dirvish-peek-setup-h ()
  "Create dirvish minibuffer preview window.
The window is created only when metadata in current minibuffer is
one of categories in `dirvish-peek-categories'."
  (let* ((meta (ignore-errors
                 (completion-metadata
                  (buffer-substring-no-properties (field-beginning) (point))
                  minibuffer-completion-table
                  minibuffer-completion-predicate)))
         (category (completion-metadata-get meta 'category))
         (p-category (and (memq category dirvish-peek-categories) category))
         (dv (dirvish--get-session 'curr-layout 'any))
         (win (and dv (dv-preview-window dv))) new-dv)
    (dirvish-prop :peek-category p-category)
    (when p-category
      (dirvish-peek--prepare-cand-fetcher)
      (add-hook 'post-command-hook #'dirvish-peek-update-h 90 t)
      (add-hook 'minibuffer-exit-hook #'dirvish-peek-exit-h nil t)
      (setq new-dv (dirvish--new :type 'peek))
      (dirvish--init-special-buffers new-dv)
      ;; `dirvish-image-dp' needs this.
      (setf (dv-index new-dv) (cons default-directory (current-buffer)))
      (setf (dv-preview-window new-dv)
            (or (and (window-live-p win) win)
                (minibuffer-selected-window) (next-window)))
      (cl-loop for (k v) on dirvish--scopes by 'cddr
               do (dirvish-prop k (and (functionp v) (funcall v))))
      (dirvish-prop :dv (dv-id new-dv))
      (dirvish-prop :preview-dps
        (append '(dirvish-peek-exception-dp) (dv-preview-dispatchers new-dv))))))

(defun dirvish-peek-update-h ()
  "Hook for `post-command-hook' to update peek window."
  (when-let* ((category (dirvish-prop :peek-category))
              (cand-fetcher (dirvish-prop :peek-fetcher))
              (cand (funcall cand-fetcher))
              ((not (string= cand (dirvish-prop :peek-last)))))
    (dirvish-prop :peek-last cand)
    (pcase category
      ('file
       (let ((fname (expand-file-name cand)))
         (if (file-remote-p fname)
             (setq cand (format "FILE_REMOTE_EXCEPTION:::%s" fname))
           (setq cand fname))))
      ('project-file
       (setq cand (expand-file-name cand (dirvish--vc-root-dir))))
      ('library
       (condition-case err
           (setq cand (file-truename (find-library-name cand)))
         (error (setq cand (format "LIB_EXCEPTION:::%s:::%s" cand
                                   (error-message-string err)))))))
    (dirvish-prop :index cand)
    (unless (file-remote-p cand)
      (dirvish-debounce nil
        (dirvish--preview-update (dirvish-curr) cand)))))

(defun dirvish-peek-exit-h ()
  "Hook for `minibuffer-exit-hook' to destroy peek session."
  (when-let* ((dv (dirvish--get-session 'type 'peek)))
    (dirvish--clear-session dv)
    (remhash (dv-id dv) dirvish--sessions))
  (dirvish-prop :peek-last nil))

;;;###autoload
(define-minor-mode dirvish-peek-mode
  "Show file preview when narrowing candidates using minibuffer."
  :group 'dirvish :global t
  (if dirvish-peek-mode
      (add-hook 'minibuffer-setup-hook #'dirvish-peek-setup-h)
    (remove-hook 'minibuffer-setup-hook #'dirvish-peek-setup-h)))

(provide 'dirvish-peek)
;;; dirvish-peek.el ends here

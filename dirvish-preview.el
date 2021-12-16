;;; dirvish-preview.el --- File preview for Dirvish. -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Create a file preview window in dirvish.

;;; Code:

(declare-function image-get-display-property "image-mode")
(require 'ansi-color)
(require 'mailcap)
(require 'dirvish-vars)
(eval-when-compile (require 'subr-x))

(defvar dirvish-preview-update-timer)

(cl-defun dirvish-preview-match-mime (file)
  "To determine if FILE can be matched by `dirvish-preview-cmd-alist'."
  (pcase-dolist (`(,re-or-exts ,cmd) dirvish-preview-cmd-alist)
    (if (listp re-or-exts)
        (let ((ext (file-name-extension file)))
          (when (member ext re-or-exts)
            (cl-return-from dirvish-preview-match-mime cmd)))
      (when (string-match re-or-exts (or (mailcap-file-name-to-mime-type file) ""))
        (cl-return-from dirvish-preview-match-mime cmd)))))

(cl-defun dirvish--preview-entry (entry)
  "Create the preview buffer for ENTRY."
  (unless (file-readable-p entry)
    (cl-return-from dirvish--preview-entry
      (dirvish--preview-get-create "File Not Readable")))
  (when (file-directory-p entry)
    (cl-return-from dirvish--preview-entry
      (dirvish--preview-get-create "directory" "exa" (list "--color=always" "-al" entry))))
  (let ((match (dirvish-preview-match-mime entry))
        (enable-local-variables nil)
        (inhibit-modification-hooks t)
        (auto-save-default nil)
        (delay-mode-hooks t)
        (inhibit-message t))
    (if match
        (let ((cmd (car match)) (args (cdr match)))
          (when (functionp cmd)
            (cl-return-from dirvish--preview-entry (apply cmd entry args)))
          (dirvish--preview-get-create entry cmd args))
      (let ((threshold (or large-file-warning-threshold 10000000))
            (filesize (file-attribute-size (file-attributes entry))))
        (if (> filesize threshold)
            (dirvish--preview-get-create (concat entry " too big for literal preview"))
          (find-file-noselect entry t nil))))))

(cl-defun dirvish--preview-get-create (entry &optional cmd args)
  "Get corresponding preview buffer for ENTRY.

Optionally, a shell command CMD and its ARGS can be passed."
  (let ((buf (frame-parameter nil 'dirvish-preview-buffer))
        (process-connection-type nil)
        (size (number-to-string (or (and (boundp 'dirvish-minibuf-preview--width)
                                         dirvish-minibuf-preview--width)
                                    dirvish-width-img)))
        cache)
    (with-current-buffer buf
      (erase-buffer) (remove-overlays)
      (unless cmd (insert entry) (cl-return-from dirvish--preview-get-create buf))
      (when (string= cmd "*Preview Disable*")
        (insert (format "Preview Disabled for %s." entry))
        (cl-return-from dirvish--preview-get-create buf))
      (unless (executable-find cmd)
        (insert (format "Install `%s' to preview %s" cmd entry))
        (cl-return-from dirvish--preview-get-create buf))
      (when (or (member "%T" args) (member "%t" args)) (setq cache t))
      (cl-dolist (fmt `((,entry . "%i") (,size . "%s")))
        (setq args (cl-substitute (car fmt) (cdr fmt) args :test 'string=)))
      (unless cache
        (let* ((process-connection-type nil)
               (default-directory "~") ; Avoid "Setting current directory" error after deleting dir
               (res-buf (get-buffer-create " *Dirvish preview result*"))
               (proc (apply #'start-process "dirvish-preview-process" res-buf cmd args)))
          (with-current-buffer res-buf (erase-buffer) (remove-overlays))
          (set-process-sentinel proc 'dirvish--preview-process-sentinel))
        (cl-return-from dirvish--preview-get-create buf))
      (save-excursion
        ;; FIXME: a better way to deal with gif?
        (when (string= (mailcap-file-name-to-mime-type entry) "image/gif")
          (let ((gif-buf (find-file-noselect entry t nil))
                (callback (lambda (buf)
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (image-animate (image-get-display-property)))))))
            (run-with-idle-timer 1 nil callback gif-buf)
            (cl-return-from dirvish--preview-get-create gif-buf)))
        (let* ((target-raw (concat dirvish-cache-dir size entry))
               (target-ext (concat target-raw ".jpg"))
               (target (if (and (string= cmd "convert")
                                (< (nth 7 (file-attributes entry)) (* 1024 1024 0.5)))
                           entry target-ext)))
          (if (file-exists-p target)
              (let ((img (create-image target nil nil :max-width dirvish-width-img)))
                (put-image img 0) (cl-return-from dirvish--preview-get-create buf))
            (make-directory (file-name-directory target-raw) t)
            (cl-dolist (format `((,target-raw . "%t") (,target-ext . "%T")))
              (setq args (cl-substitute (car format) (cdr format) args :test 'string=)))
            (let ((proc (apply #'start-process "dirvish-preview-process" buf cmd args)))
              (set-process-sentinel proc (lambda (_p _e) (dirvish-preview-update))))
            (insert "[Cache] Generating thumbnail..."))))
      buf)))

(cl-defun dirvish-preview-build ()
  "Build dirvish preview window."
  (when-let ((one-window (frame-parameter nil 'dirvish-one-window)))
    (cl-return-from dirvish-preview-build))
  (when dirvish-enable-preview
    (let* ((inhibit-modification-hooks t)
           (buf (frame-parameter nil 'dirvish-preview-buffer))
           (win-alist `((side . right) (window-width . ,dirvish-preview-width)))
           (fringe 30)
           (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
      (set-window-fringes new-window fringe fringe nil t)
      (setq dirvish-width-img (window-width new-window t))
      (set-frame-parameter nil 'dirvish-preview-window new-window))))

(defun dirvish--preview-process-sentinel (proc _)
  "Dirvish preview process sentinel.

When PROC finishes, fill `dirvish-preview-buffer' with process
result string."
  (let ((buf (frame-parameter nil 'dirvish-preview-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer (frame-parameter nil 'dirvish-preview-buffer)
        (erase-buffer) (remove-overlays)
        (let ((result-str (with-current-buffer (process-buffer proc) (buffer-string))))
          (insert result-str)
          (ansi-color-apply-on-region
           (point-min) (progn (goto-char (point-min)) (forward-line (frame-height)) (point))))))))

(defun dirvish-preview-update (&optional preview-window)
  "Update dirvish preview window.

Only take effect when `dirvish-enable-preview' or PREVIEW-WINDOW is not nil."
  (when (or dirvish-enable-preview preview-window)
    (let* ((orig-buffer-list (buffer-list))
           (index (or (frame-parameter nil 'dirvish-index-path) ""))
           (preview-buffer (dirvish--preview-entry index))
           (preview-window (or preview-window (frame-parameter nil 'dirvish-preview-window))))
      (when (window-live-p preview-window)
        (set-window-buffer preview-window preview-buffer))
      (unless (memq preview-buffer orig-buffer-list)
        (push preview-buffer dirvish-preview-buffers))
      (with-current-buffer preview-buffer (run-hooks 'dirvish-preview-setup-hook)))))

(when (require 'pdf-tools nil t)
  (custom-reevaluate-setting 'dirvish-preview-cmd-alist))
  
(provide 'dirvish-preview)

;;; dirvish-preview.el ends here

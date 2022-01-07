;;; dirvish-preview.el --- File preview for Dirvish -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Create a file preview window in dirvish.

;;; Code:

(declare-function image-get-display-property "image-mode")
(require 'ansi-color)
(require 'so-long)
(require 'mailcap)
(require 'dirvish-structs)
(require 'dirvish-vars)
(require 'dirvish-helpers)
(eval-when-compile (require 'subr-x))

(defun dirvish--get-image-cache-for-file (file size &optional ext)
  "Get image cache filepath for FILE.
SIZE is window pixelwise width of current dirvish preview window.
A optional extension EXT, such as \".jpg\", can be given to the
cache image."
  (let ((cache (concat dirvish-cache-dir (number-to-string size) file)))
    (unless (file-exists-p cache)
      (make-directory (file-name-directory cache) t))
    (concat cache ext)))

(defun dirvish--preview-process-fill-str-sentinel (proc _exitcode)
  "A sentinel for dirvish preview process.

When PROC finishes, fill `dv-preview-buffer' with process
result string."
  (when-let ((buf (and (dirvish-curr)
                       (dv-preview-buffer (dirvish-curr)))))
    (with-current-buffer buf
      (erase-buffer) (remove-overlays)
      (let ((result-str (with-current-buffer (process-buffer proc) (buffer-string))))
        (insert result-str)
        (ansi-color-apply-on-region
         (point-min) (progn (goto-char (point-min)) (forward-line (frame-height)) (point)))))))

(defun dirvish--preview-process-update-sentinel (_proc _exitcode)
  "A process sentinel for updating dirvish preview window."
  (dirvish-preview-update))

(defun dirvish-preview-disable-dispatcher (file _dv)
  "A dispatcher function for `dirvish-preview-dispatchers'.
FILE matched by this dispatcher do not display a preview."
  (when (or (not (file-exists-p file))
            (not (file-readable-p file))
            (member (file-name-extension file)
                    '("iso" "bin" "exe" "gpg" "elc" "eln")))
    `(info . ,(format "File %s is not readable or in the preview blacklist." file))))

(defun dirvish-preview-directory-exa-dispatcher (file _dv)
  "A dispatcher function for `dirvish-preview-dispatchers'.
FILE matched by this uses `exa' to generate directory preview."
  (when (file-directory-p file)
    `(shell . ("exa" "--color=always" "-al" ,file))))

(defun dirvish-preview-directory-dired-dispatcher (file _dv)
  "A dispatcher function for `dirvish-preview-dispatchers'.
FILE matched by this returns a Dired buffer as preview."
  (when (file-directory-p file)
    `(buffer . (dired-noselect ,file))))

(defun dirvish-preview-text-dispatcher (file _dv)
  "A dispatcher function for `dirvish-preview-dispatchers'.
FILE matched by this returns a regular file buffer as preview."
  (when (string-match "text/" (or (mailcap-file-name-to-mime-type file) ""))
    `(buffer . (find-file-noselect ,file t nil))))

(defun dirvish-preview-gif-dispatcher (file _dv)
  "A dispatcher function for `dirvish-preview-dispatchers'.
FILE matched by this display an animated image as preview."
  (when (string= (mailcap-file-name-to-mime-type file) "image/gif")
    (let ((gif-buf (find-file-noselect file t nil))
          (callback (lambda (buf)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (image-animate (image-get-display-property)))))))
      (run-with-idle-timer 1 nil callback gif-buf)
      `(buffer . ,gif-buf))))

(defun dirvish-preview-image-dispatcher (file dv)
  "A dispatcher function for `dirvish-preview-dispatchers'.
FILE matched by this dispatcher display a image with its width
less than `dv-preview-pixel-width' of instance DV."
  (when (string-match "image/" (or (mailcap-file-name-to-mime-type file) ""))
    (let* ((size (dv-preview-pixel-width dv))
           (cache (dirvish--get-image-cache-for-file file size ".jpg")))
      (cond ((file-exists-p cache)
             `(image . (put-image ,(create-image cache nil nil :max-width size) 0)))
            ((< (nth 7 (file-attributes file)) dirvish-preview-image-threshold)
             `(image . (put-image ,(create-image file nil nil :max-width size) 0)))
            (t `(image-cache . ("convert" "-resize" ,(number-to-string size) ,file ,cache)))))))

(defun dirvish-preview-video-dispatcher (file dv)
  "A dispatcher function for `dirvish-preview-dispatchers'.
FILE matched by this dispatcher display a video thumbnail with
its width less than `dv-preview-pixel-width' of instance DV."
  (when (string-match "video/" (or (mailcap-file-name-to-mime-type file) ""))
    (let* ((size (dv-preview-pixel-width dv))
           (cache (dirvish--get-image-cache-for-file file size ".jpg")))
      (if (file-exists-p cache)
          `(image . (put-image ,(create-image cache nil nil :max-width size) 0))
        `(image-cache . ("ffmpegthumbnailer" "-i" ,file "-o" ,cache "-s 0"))))))

(defun dirvish-preview-epub-dispatcher (file dv)
  "A dispatcher function for `dirvish-preview-dispatchers'.
FILE matched by this dispatcher display a epub thumbnail with its
width less than `dv-preview-pixel-width' of instance DV."
  (when (string= (file-name-extension file) "epub")
    (let* ((size (dv-preview-pixel-width dv))
           (cache (dirvish--get-image-cache-for-file file size ".jpg")))
      (if (file-exists-p cache)
          `(image . (put-image ,(create-image cache nil nil :max-width size) 0))
        `(image-cache . ("ffmpegthumbnailer" "-i" ,file "-o" ,cache "-s 0"))))))

(defun dirvish-preview-pdf-preface-dispatcher (file dv)
  "A dispatcher function for `dirvish-preview-dispatchers'.
FILE matched by this dispatcher display a pdf thumbnail with its
width less than `dv-preview-pixel-width' of instance DV."
  (when (string= (file-name-extension file) "pdf")
    (let* ((size (dv-preview-pixel-width dv))
           (cache (dirvish--get-image-cache-for-file file size))
           (cache-jpg (concat cache ".jpg")))
      (if (file-exists-p cache-jpg)
          `(image . (put-image ,(create-image cache-jpg nil nil :max-width size) 0))
        `(image-cache . ("pdftoppm" "-jpeg" "-f" "1" "-singlefile" ,file ,cache))))))

(defun dirvish-preview-pdf-tools-dispatcher (file _dv)
  "A dispatcher function for `dirvish-preview-dispatchers'.
FILE matched by this returns a regular file buffer as preview."
  (when (string= (file-name-extension file) "pdf")
    `(buffer . (find-file-noselect ,file t nil))))

(defun dirvish-preview-archive-dispatcher (file _dv)
  "A dispatcher function for `dirvish-preview-dispatchers'.
FILE matched by this returns a regular file buffer as preview."
  (cond ((string= (file-name-extension file) "zip")
         `(shell . ("zipinfo" ,file)))
        ((member (file-name-extension file) '("tar" "zst"))
         `(shell . ("tar" "-tvf" ,file)))))

(defun dirvish-preview-default-dispatcher (file _dv)
  "A dispatcher function for `dirvish-preview-dispatchers'.
FILE matched by this either display a regular file buffer or a
warning as preview depending on whether this file is too large or
containing very long lines."
  (let ((threshold (or large-file-warning-threshold 10000000))
        (filesize (file-attribute-size (file-attributes file)))
        (enable-local-variables nil))
    (if (> filesize threshold)
        `(info ,(concat "File " file " is too big for literal preview."))
      (let ((buf (find-file-noselect file t nil)))
        (with-current-buffer buf
          (if (so-long-detected-long-line-p)
              (progn
                (kill-buffer buf)
                `(info . ,(format "File %s contains very long lines, preview skipped." file)))
            `(buffer . ,buf)))))))

(defun dirvish-get-preview-buffer (file &optional dispatchers)
  "Create the preview buffer for FILE.
If DISPATCHERS is not given, defaults to
`dirvish-preview-dispatchers'."
  (unless dispatchers (setq dispatchers dirvish-preview-dispatchers))
  (cl-loop for dispatcher in dispatchers
           for (dv-type . payload) = (funcall dispatcher file (dirvish-curr))
           for buffer = (dirvish-preview-dispatch dv-type payload)
           until dv-type
           finally return buffer))

(defun dirvish-preview-dispatch (preview-type payload)
  "Execute dispatcher PAYLOAD according to PREVIEW-TYPE.
This function apply the payloads provided by the first
matched preview dispatcher to the preview buffer, and finally
return the buffer.
A PAYLOAD is can be either:

- a buffer which is displayed inside of preview window.

- a (CMD . ARGS) cons where CMD can be a elisp function or a
shell command.  In either case, ARGS holds a list of arguments
for them.

- a string which is displayed directly in preview buffer.  Need
  to use in conjunction with `info' PREVIEW-TYPE.

A PREVIEW-TYPE can be one of following values:

- `info', which means insert PAYLOAD string to preview buffer.

- `buffer', meaning either PAYLOAD itself is a buffer
  or `(apply CMD ARGS)' return a buffer directly as preview
  buffer.

- `image', meaning `(apply CMD ARGS)' should return a image to be
  inserted to preview buffer.

- `shell', indicates CMD is a shell command, the result of CMD
  and ARGS will be inserted in preview buffer as content when the
  shell process exits successfully.

- `image-cache', similar to `shell', but the CMD should generate
  a cache image, and when the process exits, it fires up a
  preview update."
  (let ((buf (dv-preview-buffer (dirvish-curr)))
        (cmd (car-safe payload))
        (args (cdr-safe payload))
        (process-connection-type nil)
        (enable-local-variables nil)
        (inhibit-modification-hooks t)
        (auto-save-default nil)
        (delay-mode-hooks t)
        (dirvish-show-icons nil))
    (when (and (stringp cmd) (not (executable-find cmd)))
      (setq preview-type 'info
            payload (format "Install `%s' to preview this file." cmd)))
    (with-current-buffer buf
      (erase-buffer) (remove-overlays)
      (cl-case preview-type
        ('info (insert payload))
        ('buffer (setq buf (if cmd (apply cmd args) payload)))
        ('image (apply cmd args))
        ('image-cache
         (let ((proc (apply #'start-process "dirvish-preview-process" buf cmd args)))
           (set-process-sentinel proc #'dirvish--preview-process-update-sentinel)
           (insert " [Dirvish] Generating image cache...")))
        ('shell
         (let* ((res-buf (get-buffer-create " *Dirvish preview result*"))
                (proc (apply #'start-process "dirvish-preview-process" res-buf cmd args)))
           (with-current-buffer res-buf (erase-buffer) (remove-overlays))
           (set-process-sentinel proc 'dirvish--preview-process-fill-str-sentinel))))
      buf)))

(cl-defun dirvish-preview-build ()
  "Build dirvish preview window."
  (when-let ((one-window-p (dv-one-window-p (dirvish-curr))))
    (cl-return-from dirvish-preview-build))
  (when dirvish-enable-preview
    (let* ((inhibit-modification-hooks t)
           (buf (dv-preview-buffer (dirvish-curr)))
           (win-alist `((side . right) (window-width . ,dirvish-preview-width)))
           (fringe 30)
           (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
      (set-window-fringes new-window fringe fringe nil t)
      (setf (dv-preview-pixel-width (dirvish-curr)) (window-width new-window t))
      (setf (dv-preview-window (dirvish-curr)) new-window))))

(defun dirvish-preview-update ()
  "Update dirvish preview."
  (when-let* ((curr-dv (dirvish-curr))
              (preview-window (dv-preview-window curr-dv))
              dirvish-enable-preview)
      (let* ((orig-buffer-list (buffer-list))
             (index (or (dv-index-path curr-dv) ""))
             (preview-buffer (dirvish-get-preview-buffer index)))
        (when (window-live-p preview-window)
          (set-window-buffer preview-window preview-buffer))
        (unless (memq preview-buffer orig-buffer-list)
          (push preview-buffer (dv-preview-buffers curr-dv)))
        (with-current-buffer preview-buffer (run-hooks 'dirvish-preview-setup-hook)))))

(provide 'dirvish-preview)

;;; dirvish-preview.el ends here

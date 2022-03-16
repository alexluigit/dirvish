;;; dirvish-preview.el --- File preview for Dirvish -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides utilities for managing file preview in Dirvish.

;;; Code:

(require 'so-long)
(require 'mailcap)
(require 'image-mode)
(require 'dirvish-core)
(eval-when-compile (require 'subr-x))
(mailcap-parse-mimetypes)

(defun dirvish--preview-image-size (window &optional height)
  "Get corresponding image width or HEIGHT in WINDOW."
  (floor (* dirvish--preview-img-scale (funcall (if height #'window-pixel-height #'window-pixel-width) window))))

(defun dirvish--get-image-cache-for-file (file size &optional ext no-mkdir)
  "Get image cache filepath for FILE.
SIZE is window pixelwise width of current dirvish preview window.
A optional extension EXT, such as \".jpg\", can be given to the
cache image. A new directory is created unless NO-MKDIR."
  (let ((cache (concat dirvish-cache-dir (number-to-string size)
                       (when dirvish--os-windows-p "/")
                       (replace-regexp-in-string ":" "" file))))
    (and (not no-mkdir) (not (file-exists-p cache))
         (make-directory (file-name-directory cache) t))
    (concat cache ext)))

(defun dirvish-preview--insert-image (image dv)
  "Insert IMAGE at preview window of DV."
  (insert " ")
  (add-text-properties 1 2 `(display ,image rear-nonsticky t keymap ,image-map))
  (cl-destructuring-bind (i-width . i-height) (image-size (image-get-display-property))
    (let* ((p-window (dv-preview-window dv))
           (w-offset (max (round (/ (- (window-width p-window) i-width) 2)) 0))
           (h-offset (max (round (/ (- (window-height p-window) i-height) 2)) 0)))
      (goto-char 1)
      (insert (make-string h-offset ?\n) (make-string w-offset ?\s)))))

(defun dirvish-clean-preview-images (fileset)
  "Clean image cache for FILESET."
  (let ((win (dv-preview-window (dirvish-curr))) size)
    (when (window-live-p win)
      (setq size (dirvish--preview-image-size win))
      (dolist (file fileset)
        (mapc #'delete-file (file-expand-wildcards
                             (dirvish--get-image-cache-for-file file size ".*" t) t))))))

(defun dirvish--preview-process-fill-str-sentinel (proc _exitcode)
  "A sentinel for dirvish preview process.
When PROC finishes, fill preview buffer with process result."
  (when-let ((dv (dirvish-curr)))
    (with-current-buffer (dirvish--get-util-buffer dv 'preview)
      (erase-buffer) (remove-overlays)
      (let ((result-str (with-current-buffer (process-buffer proc) (buffer-string)))
            (p-min (point-min)))
        (insert result-str)
        (ansi-color-apply-on-region
         p-min (progn (goto-char p-min) (forward-line (frame-height)) (point)))))))

(dirvish-define-preview disable (file)
  "Disable preview in some cases."
  (when (or (not (file-exists-p file))
            (not (file-readable-p file))
            (member (file-name-extension file) dirvish-preview-disabled-exts))
    `(info . ,(format "File %s is not readable or in the preview blacklist." file))))

(dirvish-define-preview text (file)
  "Open FILE with `find-file-noselect'."
  (when (string-match "text/" (or (mailcap-file-name-to-mime-type file) ""))
    `(buffer . ,(find-file-noselect file t nil))))

(dirvish-define-preview gif (file)
  "Display an animated image FILE."
  (when (string= (mailcap-file-name-to-mime-type file) "image/gif")
    (let ((gif-buf (find-file-noselect file t nil))
          (callback (lambda (buf)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (image-animate (image-get-display-property)))))))
      (run-with-idle-timer 1 nil callback gif-buf)
      `(buffer . ,gif-buf))))

(dirvish-define-preview image (file dv)
  "Display a image with width of DV's preview window."
  (when (string-match "image/" (or (mailcap-file-name-to-mime-type file) ""))
    (let* ((p-win (dv-preview-window dv))
           (width (dirvish--preview-image-size p-win))
           (height (dirvish--preview-image-size p-win 'height))
           (cache (dirvish--get-image-cache-for-file file width ".jpg")))
      (cond ((file-exists-p cache)
             `(image . ,(create-image cache nil nil :max-width width :max-height height)))
            ((or (< (nth 7 (file-attributes file)) dirvish--preview-img-threshold)
                 (string-prefix-p (expand-file-name dirvish-cache-dir) file))
             `(image . ,(create-image file nil nil :max-width width :max-height height)))
            (t `(image-cache . ("convert" "-resize" ,(number-to-string width) ,file ,cache)))))))

(dirvish-define-preview video (file dv)
  "Display a video thumbnail with width of DV's preview window."
  (when (string-match "video/" (or (mailcap-file-name-to-mime-type file) ""))
    (let* ((p-win (dv-preview-window dv))
           (width (dirvish--preview-image-size p-win))
           (height (dirvish--preview-image-size p-win 'height))
           (cache (dirvish--get-image-cache-for-file file width ".jpg")))
      (if (file-exists-p cache)
          `(image . ,(create-image cache nil nil :max-width width :max-height height))
        `(image-cache . ("ffmpegthumbnailer" "-i" ,file "-o" ,cache "-s" ,(number-to-string width) "-m"))))))

(dirvish-define-preview audio (file)
  "Use output of `mediainfo' shell command as preview."
  (when (string-match "audio/" (or (mailcap-file-name-to-mime-type file) ""))
    `(shell . ("mediainfo" ,file))))

(dirvish-define-preview epub (file dv)
  "Display a epub thumbnail with width of DV's preview window."
  (when (string= (file-name-extension file) "epub")
    (let* ((p-win (dv-preview-window dv))
           (width (dirvish--preview-image-size p-win))
           (height (dirvish--preview-image-size p-win 'height))
           (cache (dirvish--get-image-cache-for-file file width ".jpg")))
      (if (file-exists-p cache)
          `(image . ,(create-image cache nil nil :max-width width :max-height height))
        `(image-cache . ("epub-thumbnailer" ,file ,cache ,(number-to-string width)))))))

(dirvish-define-preview pdf-preface (file dv)
  "Display a pdf preface image with width of DV's preview window."
  (when (string= (file-name-extension file) "pdf")
    (let* ((p-win (dv-preview-window dv))
           (width (dirvish--preview-image-size p-win))
           (height (dirvish--preview-image-size p-win 'height))
           (cache (dirvish--get-image-cache-for-file file width))
           (cache-jpg (concat cache ".jpg")))
      (if (file-exists-p cache-jpg)
          `(image . ,(create-image cache-jpg nil nil :max-width width :max-height height))
        `(image-cache . ("pdftoppm" "-jpeg" "-f" "1" "-singlefile" ,file ,cache))))))

(dirvish-define-preview pdf-tools (file)
  "Open FILE with `find-file-noselect'."
  (when (string= (file-name-extension file) "pdf")
    `(buffer . ,(find-file-noselect file t nil))))

(dirvish-define-preview archive (file)
  "Display output from corresponding unarchive shell commands."
  (cond ((string= (file-name-extension file) "zip")
         `(shell . ("zipinfo" ,file)))
        ((member (file-name-extension file) '("tar" "zst"))
         `(shell . ("tar" "-tvf" ,file)))))

(dirvish-define-preview default (file)
  "Default preview dispatcher."
  (let ((threshold (or large-file-warning-threshold 10000000))
        (filesize (file-attribute-size (file-attributes file)))
        (enable-local-variables nil))
    (cond ((file-directory-p file) ; in case user did not specify a directory dispatcher
           `(buffer . ,(dired-noselect file)))
          ((> filesize threshold) ; do not preview too large files
           `(info . ,(format "File %s is too big for literal preview." file)))
          (t
           (let ((buf (find-file-noselect file t nil)))
             (with-current-buffer buf
               (if (so-long-detected-long-line-p) ; do not preview files containing too long lines
                   (progn
                     (kill-buffer buf)
                     `(info . ,(format "File %s contains very long lines, preview skipped." file)))
                 `(buffer . ,buf))))))))

(defun dirvish-preview-dispatch (preview-type payload dv)
  "Execute dispatcher's PAYLOAD according to PREVIEW-TYPE.
This function apply the payloads provided by the first
matched preview dispatcher to the preview buffer, and finally
return the buffer.

PREVIEW-TYPE can be one of following values:

- `info', meaning PAYLOAD is a string.
- `buffer', meaning PAYLOAD is a buffer.
- `image', meaning PAYLOAD is a image.
- `image-cache', meaning PAYLOAD is a (IMAGE-CMD . ARGS) cons.
- `shell', meaning PAYLOAD is a (TEXT-CMD . ARGS) cons.

According to the PAYLOAD, one of these action is applied:

- A string/image PAYLOAD is inserted to the default preview buffer.
- A buffer PAYLOAD is used as preview buffer directly.
- A subprocess for IMAGE/TEXT-CMD is issued.  When the subprocess
finishes, the content in preview buffer is filled with the result
string of TEXT-CMD or the generated cache image of IMAGE-CMD."
  (let ((buf (dirvish--get-util-buffer (dirvish-curr) 'preview))
        (cmd (car-safe payload))
        (args (cdr-safe payload))
        (process-connection-type nil))
    (when (and (memq preview-type '(shell image-cache))
               (not (executable-find cmd)))
      (setq preview-type 'info
            payload (format "Install `%s' to preview this file." cmd)))
    (with-current-buffer buf
      (erase-buffer) (remove-overlays)
      (cl-case preview-type
        ('info (insert payload))
        ('buffer (setq buf payload))
        ('image (dirvish-preview--insert-image payload dv))
        ('image-cache
         (let ((proc (apply #'start-process "dirvish-preview-process" buf cmd args)))
           (set-process-sentinel
            proc (lambda (&rest _) (dirvish-debounce layout (dirvish-preview-update))))
           (insert " [Dirvish] Generating image cache...")))
        ('shell
         (let* ((res-buf (get-buffer-create " *Dirvish preview result*"))
                (proc (apply #'start-process "dirvish-preview-process" res-buf cmd args)))
           (with-current-buffer res-buf (erase-buffer) (remove-overlays))
           (set-process-sentinel proc 'dirvish--preview-process-fill-str-sentinel))))
      buf)))

(defun dirvish-get-preview-buffer (file)
  "Create the preview buffer for FILE."
  (and (file-directory-p file) (setq file (file-name-as-directory file)))
  (cl-loop with dv = (dirvish-curr)
           for dispatcher in (dv-preview-dispatchers dv)
           for (dv-type . payload) = (funcall dispatcher file dv)
           for buffer = (dirvish-preview-dispatch dv-type payload dv)
           until dv-type
           finally return buffer))

(defun dirvish-preview-update ()
  "Update dirvish preview."
  (when-let* ((curr-dv (dirvish-curr))
              (preview-window (dv-preview-window curr-dv)))
    (when (window-live-p preview-window)
      (let* ((orig-buffer-list (buffer-list))
             (index (or (dv-index-path curr-dv) ""))
             (preview-buffer (dirvish-get-preview-buffer index)))
        (setq other-window-scroll-buffer preview-buffer)
        (set-window-buffer preview-window preview-buffer)
        (unless (memq preview-buffer orig-buffer-list)
          (push preview-buffer (dv-preview-buffers curr-dv)))
        (with-current-buffer preview-buffer (run-hooks 'dirvish-preview-setup-hook))))))

(provide 'dirvish-preview)
;;; dirvish-preview.el ends here

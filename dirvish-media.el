;;; dirvish-media.el --- Preview media files smoothly  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fast and non-blocking preview for media files (image, video, etc.) in
;; Dirvish.  This extension is loaded automatically by default.  To change this
;; behavior, you can customize the `dirvish-preview-dispatchers' option.

;;; Code:

(require 'dirvish)

(defvar dirvish-media--cache-pool '())
(defvar dirvish-media--auto-cache-timer nil)
(add-to-list 'dirvish--no-update-preview-cmds 'dirvish-media-properties)

(dolist (sym-h '((dirvish-after-revert-hook . dirvish-media-clean-caches-h)
                 (dirvish-setup-hook . dirvish-media-cache-imgs-h)))
  (add-hook (car sym-h) (cdr sym-h)))

(defcustom dirvish-media-auto-cache-threshold '(500 . 4)
  "Generate cache images automatically.
The value should be a cons cell (FILES . PROCS).  Directories
that include number of files less than FILES are cached
automatically, set it to 0 disables auto caching.  PROCS is the
max number of cache processes."
  :group 'dirvish
  :type '(cons (integer :tag "Max number of directory files")
               (integer :tag "Max number of cache process"))
  :set (lambda (k v)
         (set k v)
         (and (timerp dirvish-media--auto-cache-timer)
              (cancel-timer dirvish-media--auto-cache-timer))
         (unless (eq (car v) 0)
           (setq dirvish-media--auto-cache-timer
                 (run-with-timer 0 0.25 #'dirvish-media--autocache)))))

(defcustom dirvish-media-auto-properties
  (and (executable-find "mediainfo") (executable-find "pdfinfo"))
  "Show media properties automatically in preview window."
  :group 'dirvish :type 'boolean)

(defface dirvish-media-info-heading
  '((t :inherit (dired-header bold)))
  "Face used for heading of media property groups."
  :group 'dirvish)

(defface dirvish-media-info-property-key
  '((t :inherit (italic)))
  "Face used for emerge group title."
  :group 'dirvish)

(defconst dirvish-media--cache-img-fns
  (cl-loop for dp in '(image video epub) collect (intern (format "dirvish-%s-preview-dp" dp))))
(defconst dirvish-media--embedded-video-thumb
  (string-match "prefer embedded image" (shell-command-to-string "ffmpegthumbnailer -h")))
(defconst dirvish-media--img-max-width 2400)
(defconst dirvish-media--img-scale-h 0.75)
(defconst dirvish-media--img-scale-w 0.92)
(defconst dirvish-media--info
  "General;(Full-name . \"\"%FileName%\"\")(Format . \"\"%Format%\"\")(File-size . \"\"%FileSize/String1%\"\")(Duration . \"\"%Duration/String3%\"\")
Image;(Width . \"\"%Width/String%\"\")(Height . \"\"%Height/String%\"\")(Bit-depth . \"\"%BitDepth/String%\"\")(Color-space . \"\"%ColorSpace%\"\")(Chroma-subsampling . \"\"%ChromaSubsampling%\"\")(Compression-mode . \"\"%Compression_Mode/String%\"\")
Video;(Resolution . \"\"%Width% x %Height%\"\")(Video-codec . \"\"%CodecID%\"\")(Framerate . \"\"%FrameRate%\"\")(Video-bitrate . \"\"%BitRate/String%\"\")
Audio;(Audio-codec . \"\"%CodecID%\"\")(Audio-bitrate . \"\"%BitRate/String%\"\")(Audio-sampling-rate . \"\"%SamplingRate/String%\"\")(Audio-channels . \"\"%ChannelLayout%\"\")")

(defun dirvish-media--cache-path (file &optional base ext no-mkdir)
  "Get FILE's cache path.
BASE is a string indicating the subdir of `dirvish-cache-dir' to
use.  EXT is a suffix such as \".jpg\" that is attached to FILE.
A new directory is created unless NO-MKDIR."
  (let* ((file (if (memq system-type '(windows-nt ms-dos))
                   (concat "/" (replace-regexp-in-string ":" "" file)) file))
         (cache (concat dirvish-cache-dir base file)))
    (and (not no-mkdir) (not (file-exists-p cache))
         (make-directory (file-name-directory cache) t))
    (concat cache ext)))

(defun dirvish-media--cache-sentinel (proc _exitcode)
  "Sentinel for image cache process PROC."
  (when-let* ((dv (or (dirvish-curr) dirvish--this))
              (path (dirvish-prop :index)))
    (and (equal path (process-get proc 'path))
         (dirvish-debounce nil (dirvish-preview-update dv)))))

(defun dirvish-media--autocache ()
  "Pop and run the cache tasks in `dirvish-media--cache-pool'."
  (when (and dirvish-media--cache-pool
             (< (length (process-list))
                (cdr dirvish-media-auto-cache-threshold)))
    (let (process-connection-type proc)
      (pcase-let* ((`(,procname . (,path ,_width ,cmd ,args))
                    (pop dirvish-media--cache-pool)))
        (when path
          (setq proc (apply #'start-process procname
                            (dirvish--util-buffer "img-cache") cmd args))
          (process-put proc 'path path)
          (set-process-sentinel proc #'dirvish-media--cache-sentinel))))))

(defun dirvish-media--group-heading (group-titles)
  "Format media group heading in Dirvish preview buffer.
GROUP-TITLES is a list of group titles."
  (let ((prefix (propertize "    " 'face
                            '(:inherit dirvish-media-info-heading
                                       :strike-through t)))
        (title (propertize
                (format " %s " (mapconcat #'concat group-titles " & "))
                'face 'dirvish-media-info-heading))
        (suffix (propertize " " 'display '(space :align-to right)
                            'face '(:inherit dirvish-media-info-heading
                                             :strike-through t))))
    (format "%s%s%s\n\n" prefix title suffix)))

(defun dirvish-media--metadata-from-mediainfo (file)
  "Return result string from command `mediainfo' for FILE."
  (read (format "(%s)" (shell-command-to-string
                        (format "mediainfo --Output=$'%s' %s"
                                dirvish-media--info
                                (shell-quote-argument file))))))

(defun dirvish-media--metadata-from-pdfinfo (file)
  "Return result string from command `pdfinfo' for FILE."
  (cl-loop with out = (shell-command-to-string
                       (format "pdfinfo %s" (shell-quote-argument file)))
           with lines = (remove "" (split-string out "\n"))
           for line in lines
           for (title content) = (split-string line ":\s+")
           concat (format "       %s:\t%s\n"
                          (propertize title 'face 'dirvish-media-info-property-key)
                          content)))

(defun dirvish-media--format-metadata (mediainfo properties)
  "Return a formatted string of PROPERTIES from MEDIAINFO."
  (cl-loop for prop in properties
           for p-name = (replace-regexp-in-string
                            "-" " " (format "%s" prop))
           for info = (alist-get prop mediainfo)
           concat (format "       %s:\t%s\n"
                          (propertize p-name 'face 'dirvish-media-info-property-key)
                          info)))

(cl-defgeneric dirvish-media-metadata (file)
  "Get media file FILE's metadata.")

(cl-defmethod dirvish-media-metadata ((file (head image)))
  "Get metadata for image FILE."
  (let ((minfo (dirvish-media--metadata-from-mediainfo (cdr file))))
    (format "%s%s\n%s%s"
            (dirvish-media--group-heading '("General"))
            (dirvish-media--format-metadata minfo '(Full-name Format File-size))
            (dirvish-media--group-heading '("Image"))
            (dirvish-media--format-metadata
             minfo '(Width Height Color-space Chroma-subsampling Bit-depth Compression-mode)))))

(cl-defmethod dirvish-media-metadata ((file (head video)))
  "Get metadata for video FILE."
  (let ((minfo (dirvish-media--metadata-from-mediainfo (cdr file))))
    (format "%s%s\n%s%s\n%s%s"
            (dirvish-media--group-heading '("General"))
            (dirvish-media--format-metadata
             minfo '(Full-name Format File-size Duration))
            (dirvish-media--group-heading '("Video"))
            (dirvish-media--format-metadata
             minfo '(Resolution Video-codec Framerate Video-bitrate))
            (dirvish-media--group-heading '("Audio"))
            (dirvish-media--format-metadata
             minfo '(Audio-codec Audio-bitrate Audio-sampling-rate Audio-channels)))))

(cl-defmethod dirvish-media-metadata ((file (head pdf)))
  "Get metadata for pdf FILE."
  (format "%s%s" (dirvish-media--group-heading '("PDF info"))
          (dirvish-media--metadata-from-pdfinfo (cdr file))))

(defun dirvish-media--type (ext)
  "Return media file type from file name extension EXT."
  (cond ((member ext dirvish-image-exts) 'image)
        ((member ext dirvish-video-exts) 'video)
        ((and (memq 'pdf-preface dirvish-preview-dispatchers)
              (equal ext "pdf") 'pdf))
        (t (user-error "Not a media file"))))

(defun dirvish-media-properties ()
  "Display media file's metadata in preview window."
  (interactive)
  (let* ((file (or (dirvish-prop :index)
                   (user-error "No file under the cursor")))
         (ext (downcase (or (file-name-extension file) "")))
         (type (dirvish-media--type ext))
         (buf (dirvish--util-buffer 'preview (dirvish-curr) t t)))
    (with-current-buffer buf
      (let ((pivot (dirvish-prop :mediainfo-pivot)) beg)
        (when (eq pivot 0) (user-error "Media properties already displayed"))
        (when (> pivot 2) (delete-region 2 pivot))
        (goto-char (point-max))
        (insert "\n\n\n")
        (setq beg (point))
        (insert (dirvish-media-metadata (cons type file)))
        (align-regexp beg (point) "\\(\\\t\\)[^\\\t\\\n]+" 1 4 t)
        (dirvish-prop :mediainfo-pivot 0)))))

(cl-defmethod dirvish-preview-dispatch ((recipe (head media-img)) dv)
  "Insert RECIPE as an image at preview window of DV."
  (let ((buf (dirvish--util-buffer 'preview dv nil t))
        (img (cdr recipe)))
    (with-current-buffer buf
      (erase-buffer) (remove-overlays)
      (font-lock-mode -1)
      (insert " ")
      (add-text-properties 1 2 `(display ,img rear-nonsticky t keymap ,image-map))
      (pcase-let ((`(,iw . ,ih) (image-size img)))
        (let* ((p-window (dv-preview-window dv))
               (w-offset (max (round (/ (- (window-width p-window) iw) 2)) 0))
               (h-offset (max (round (/ (- (window-height p-window) ih) 2)) 0)))
          (and dirvish-media-auto-properties (setq h-offset 2))
          (goto-char 1)
          (insert (make-string h-offset ?\n))
          (dirvish-prop :mediainfo-pivot
            (if dirvish-media-auto-properties 0 (point-marker)))
          (insert (make-string w-offset ?\s))
          (when dirvish-media-auto-properties
            (let* ((beg (progn (goto-char (point-max)) (point)))
                   (file (with-current-buffer (cdr (dv-index dv))
                           (dirvish-prop :index)))
                   (type (dirvish-media--type
                          (downcase (or (file-name-extension file) "")))))
              (insert "\n\n\n")
              (insert (dirvish-media-metadata (cons type file)))
              (align-regexp beg (point) "\\(\\\t\\)[^\\\t\\\n]+" 1 4 t))
            (goto-char 1))))
      buf)))

(cl-defmethod dirvish-preview-dispatch ((recipe (head media-cache)) dv)
  "Generate cache image according to RECIPE and session DV."
  (let* ((path (dirvish-prop :index))
         (buf (dirvish--util-buffer 'preview dv nil t))
         (name (format "%s-%s-img-cache" path
                       (window-width (dv-preview-window dv)))))
    (unless (get-process name)
      (setq dirvish-media--cache-pool
            (delete (assoc name dirvish-media--cache-pool) dirvish-media--cache-pool))
      (let ((proc (apply #'start-process
                         name (dirvish--util-buffer "img-cache")
                         (cadr recipe) (cddr recipe))))
        (process-put proc 'path path)
        (set-process-sentinel proc #'dirvish-media--cache-sentinel)))
    (with-current-buffer buf
      (erase-buffer) (remove-overlays)
      (insert " [Dirvish] Generating image cache...") buf)))

(defun dirvish-media--img-size (window &optional height)
  "Get corresponding image width or HEIGHT in WINDOW."
  (let ((size (if height (* dirvish-media--img-scale-h (window-pixel-height window))
                (min (* dirvish-media--img-scale-w (window-pixel-width window))
                     dirvish-media--img-max-width))))
    (floor size)))

(defun dirvish-media-cache-imgs-h ()
  "Cache image/video-thumbnail for index directory."
  (when-let* ((dv (dirvish-curr))
              ((not (dirvish-prop :remote)))
              ((dv-layout dv))
              (win (dv-preview-window dv))
              ((window-live-p win))
              (width (window-width win))
              (md5s (hash-table-keys dirvish--attrs-hash))
              ((< (length md5s) (car dirvish-media-auto-cache-threshold))))
    (cl-loop
     for file in (directory-files default-directory t)
     for ext = (downcase (or (file-name-extension file) ""))
     for (cmd . args) = (cl-loop
                         for fn in dirvish-media--cache-img-fns
                         for (type . payload) = (funcall fn file ext win dv)
                         thereis (and (eq type 'media-cache) payload))
     when cmd do (push (cons (format "%s-%s-img-cache" file width)
                             (list file width cmd args))
                       dirvish-media--cache-pool))))

(defun dirvish-media-clean-caches-h ()
  "Clean cache files for marked files."
  (when-let* ((has-gui (display-graphic-p))
              (win (dv-preview-window (dirvish-curr)))
              (size (and (window-live-p win)
                         (dirvish-media--img-size win))))
    (clear-image-cache)
    (setq size (dirvish-media--img-size win))
    (dolist (file (dired-get-marked-files))
      (mapc #'delete-file (file-expand-wildcards
                           (dirvish-media--cache-path
                            file (format "images/%s" size) ".*" t)
                           t)))))

(dirvish-define-preview audio (file ext)
  "Preview audio files by printing its metadata.
Require: `mediainfo' (executable)"
  :require ("mediainfo")
  (when (member ext dirvish-audio-exts) `(shell . ("mediainfo" ,file))))

(dirvish-define-preview image (file ext preview-window)
  "Preview image files.
Require: `convert' (executable from `imagemagick' suite)"
  :require ("convert")
  (when (member ext dirvish-image-exts)
    (let* ((width (dirvish-media--img-size preview-window))
           (height (dirvish-media--img-size preview-window 'height))
           (cache (dirvish-media--cache-path file (format "images/%s" width) ".jpg")))
      (if (file-exists-p cache)
          `(media-img . ,(create-image cache nil nil :max-width width :max-height height))
        `(media-cache . ("convert" ,file "-define" "jpeg:extent=300kb" "-resize"
                         ,(number-to-string width) ,cache))))))

(dirvish-define-preview gif (file ext)
  "Preview gif images with animations."
  (when (equal ext "gif")
    (let ((gif (dirvish--find-file-temporarily file))
          (callback (lambda (rcp)
                      (when-let* ((buf (cdr rcp)) ((buffer-live-p buf)))
                        (with-current-buffer buf
                          (image-animate (get-char-property 1 'display)))))))
      (run-with-idle-timer 1 nil callback gif) gif)))

(dirvish-define-preview video (file ext preview-window)
  "Preview video files.
Require: `ffmpegthumbnailer' (executable)"
  :require ("ffmpegthumbnailer")
  (when (member ext dirvish-video-exts)
    (let* ((width (dirvish-media--img-size preview-window))
           (height (dirvish-media--img-size preview-window 'height))
           (cache (dirvish-media--cache-path file (format "images/%s" width) ".jpg")))
      (if (file-exists-p cache)
          `(media-img . ,(create-image cache nil nil :max-width width :max-height height))
        `(media-cache . ("ffmpegthumbnailer" "-i" ,file "-o" ,cache "-s"
                         ,(number-to-string width)
                         ,(if dirvish-media--embedded-video-thumb "-m" "")))))))

(dirvish-define-preview epub (file preview-window)
  "Preview epub files.
Require: `epub-thumbnailer' (executable)"
  :require ("epub-thumbnailer")
  (when (equal ext "epub")
    (let* ((width (dirvish-media--img-size preview-window))
           (height (dirvish-media--img-size preview-window 'height))
           (cache (dirvish-media--cache-path file (format "images/%s" width) ".jpg")))
      (if (file-exists-p cache)
          `(media-img . ,(create-image cache nil nil :max-width width :max-height height))
        `(media-cache . ("epub-thumbnailer" ,file ,cache ,(number-to-string width)))))))

(dirvish-define-preview pdf (file ext)
  "Preview pdf files.
Require: `pdf-tools' (Emacs package)"
  (when (equal ext "pdf")
    (if (featurep 'pdf-tools) (dirvish--find-file-temporarily file)
      '(info . "Emacs package 'pdf-tools' is required to preview pdf documents"))))

(dirvish-define-preview pdf-preface (file ext preview-window)
  "Display the preface image as preview for pdf files."
  :require ("pdftoppm")
  (when (equal ext "pdf")
    (let* ((width (dirvish-media--img-size preview-window))
           (height (dirvish-media--img-size preview-window 'height))
           (cache (dirvish-media--cache-path file (format "images/%s" width)))
           (cache-jpg (concat cache ".jpg")))
      (if (file-exists-p cache-jpg)
          `(media-img . ,(create-image cache-jpg nil nil :max-width width :max-height height))
        `(media-cache . ("pdftoppm" "-jpeg" "-f" "1" "-singlefile" ,file ,cache))))))

(dirvish-define-preview archive (file ext)
  "Preview archive files.
Require: `zipinfo' (executable)
Require: `tar' (executable)"
  :require ("zipinfo" "tar")
  (cond ((equal ext "zip") `(shell . ("zipinfo" ,file)))
        ((member ext '("tar" "zst" "bz2" "bz" "gz" "xz" "tgz"))
         `(shell . ("tar" "-tvf" ,file)))))

(provide 'dirvish-media)
;;; dirvish-media.el ends here

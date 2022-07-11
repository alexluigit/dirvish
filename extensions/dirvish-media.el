;;; dirvish-media.el --- Preview media files smoothly  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.8.14
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

(defcustom dirvish-media-metadata t
  "Doc."
  :group 'dirvish :type 'boolean)

(defconst dirvish-media--cache-img-fns
  (cl-loop for dp in '(image video epub) collect (intern (format "dirvish-%s-preview-dp" dp))))
(defconst dirvish-media--embedded-video-thumb
  (string-match "prefer embedded image" (shell-command-to-string "ffmpegthumbnailer -h")))
(defconst dirvish-media--img-scale 0.92)

(defun dirvish-media--cache-img-path (file type &optional ext no-mkdir)
  "Get FILE's cache path.
TYPE is either a string indicating the subdir of
`dirvish-cache-dir' to use or a number indicating the subdir is
\"images/TYPE\".  The EXT, such as \".jpg\", is attached to FILE.
A new directory is created unless NO-MKDIR."
  (let* ((base (if (numberp type) (concat "images/" (number-to-string type)) type))
         (file (if dirvish--os-windows-p
                   (concat "/" (replace-regexp-in-string ":" "" file)) file))
         (cache (concat dirvish-cache-dir base file)))
    (and (not no-mkdir) (not (file-exists-p cache))
         (make-directory (file-name-directory cache) t))
    (concat cache ext)))

(defun dirvish-media--cache-sentinel (proc _exitcode)
  "Sentinel for image cache process PROC."
  (when-let* ((dv (dirvish-curr))
              (path (dirvish-prop :child)))
    (and (equal path (process-get proc 'path))
         (dirvish-debounce layout (dirvish-preview-update dv)))))

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

(defun dirvish-media--cache-imgs (&optional dv)
  "Cache image/video-thumbnail for index directory in DV."
  (setq dv (or dv (dirvish-curr)))
  (with-current-buffer (window-buffer (dv-root-window dv))
    (when (and (< (length (dirvish-prop :files)) (car dirvish-media-auto-cache-threshold))
               (not (dirvish-prop :tramp)))
      (cl-loop
       with win = (dv-preview-window dv)
       with width = (window-width win)
       for file in (dirvish-prop :files)
       for ext = (downcase (or (file-name-extension file) ""))
       for (cmd . args) = (cl-loop
                           for fn in dirvish-media--cache-img-fns
                           for (type . payload) = (funcall fn file ext win dv)
                           thereis (and (eq type 'media-cache) payload))
       when cmd do (push (cons (format "%s-%s-img-cache" file width)
                               (list file width cmd args))
                         dirvish-media--cache-pool)))))

(cl-defmethod dirvish-preview-dispatch ((recipe (head media-img)) dv)
  "Insert RECIPE as an image at preview window of DV."
  (let ((buf (dirvish--util-buffer 'preview dv))
        (img (cdr recipe)))
    (with-current-buffer buf
      (erase-buffer) (remove-overlays)
      (insert " ")
      (add-text-properties 1 2 `(display ,img rear-nonsticky t keymap ,image-map))
      (pcase-let ((`(,iw . ,ih) (image-size img)))
        (let* ((p-window (dv-preview-window dv))
               (w-offset (max (round (/ (- (window-width p-window) iw) 2)) 0))
               (h-offset (max (round (/ (- (window-height p-window) ih) 2)) 0)))
          (goto-char 1)
          (insert (make-string h-offset ?\n) (make-string w-offset ?\s))))
      buf)))

(cl-defmethod dirvish-preview-dispatch ((recipe (head media-cache)) dv)
  (let* ((path (dirvish-prop :child))
         (buf (dirvish--util-buffer 'preview dv))
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
  (floor (* dirvish-media--img-scale
            (funcall (if height #'window-pixel-height #'window-pixel-width) window))))

(defun dirvish-media--clean-caches ()
  "Clean cache files for marked files."
  (clear-image-cache)
  (let ((win (dv-preview-window (dirvish-curr))) size)
    (when (window-live-p win)
      (setq size (dirvish-media--img-size win))
      (dolist (file (dired-get-marked-files))
        (mapc #'delete-file (file-expand-wildcards
                             (dirvish-media--cache-img-path file size ".*" t) t))))))

(add-hook 'dirvish-after-revert-hook #'dirvish-media--clean-caches)
(add-hook 'dirvish-setup-hook #'dirvish-media--cache-imgs)

(dirvish-define-preview audio (file ext)
  "Use output of `mediainfo' command for FILE as preview."
  (when (member ext dirvish-audio-exts) `(shell . ("mediainfo" ,file))))

(dirvish-define-preview image (file ext preview-window)
  "Display a image FILE in PREVIEW-WINDOW."
  (when (member ext dirvish-image-exts)
    (let* ((width (dirvish-media--img-size preview-window))
           (height (dirvish-media--img-size preview-window 'height))
           (cache (dirvish-media--cache-img-path file width ".jpg")))
      (if (file-exists-p cache)
          `(media-img . ,(create-image cache nil nil :max-width width :max-height height))
        `(media-cache . ("convert" ,file "-define" "jpeg:extent=300kb" "-resize"
                         ,(number-to-string width) ,cache))))))

(dirvish-define-preview gif (file ext)
  "Display an animated image FILE."
  (when (equal ext "gif")
    (let ((gif-buf (find-file-noselect file t))
          (callback (lambda (buf)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (image-animate (get-char-property 1 'display)))))))
      (run-with-idle-timer 1 nil callback gif-buf)
      `(buffer . ,gif-buf))))

(dirvish-define-preview video (file ext preview-window)
  "Display a video thumbnail for FILE in PREVIEW-WINDOW."
  (when (member ext dirvish-video-exts)
    (let* ((width (dirvish-media--img-size preview-window))
           (height (dirvish-media--img-size preview-window 'height))
           (cache (dirvish-media--cache-img-path file width ".jpg")))
      (if (file-exists-p cache)
          `(media-img . ,(create-image cache nil nil :max-width width :max-height height))
        `(media-cache . ("ffmpegthumbnailer" "-i" ,file "-o" ,cache "-s"
                         ,(number-to-string width)
                         ,(if dirvish-media--embedded-video-thumb "-m" "")))))))

(dirvish-define-preview epub (file preview-window)
  "Display a epub thumbnail for FILE in PREVIEW-WINDOW."
  (when (equal ext "epub")
    (let* ((width (dirvish-media--img-size preview-window))
           (height (dirvish-media--img-size preview-window 'height))
           (cache (dirvish-media--cache-img-path file width ".jpg")))
      (if (file-exists-p cache)
          `(media-img . ,(create-image cache nil nil :max-width width :max-height height))
        `(media-cache . ("epub-thumbnailer" ,file ,cache ,(number-to-string width)))))))

(dirvish-define-preview pdf (file ext)
  "Open FILE with `find-file-noselect'."
  (when (equal ext "pdf")
    (if (featurep 'pdf-tools) `(buffer . ,(find-file-noselect file t nil))
      '(info . "Emacs package 'pdf-tools' is required to preview pdf documents"))))

(dirvish-define-preview archive (file ext)
  "Display output of corresponding unarchive commands for FILE."
  (cond ((equal ext "zip") `(shell . ("zipinfo" ,file)))
        ((member ext '("tar" "zst")) `(shell . ("tar" "-tvf" ,file)))))

(provide 'dirvish-media)
;;; dirvish-media.el ends here

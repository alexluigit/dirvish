;;; dirvish-structs.el --- Dirvish data structures -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; This file contains data structures for dirvish.

;;; Code:

(require 'dirvish-vars)
(require 'recentf)

(defun dirvish--header-buffer-default ()
  "Return a buffer for dirvish header with sensible settings."
  (with-current-buffer
      (get-buffer-create
       (format " *Dirvish Header-%s*"
               (number-to-string (length dirvish-frame-list))))
    (setq-local face-font-rescale-alist nil)
    (current-buffer)))

(defun dirvish--preview-buffer-default ()
  "Return a buffer for dirvish preview with sensible settings."
  (with-current-buffer
      (get-buffer-create
       (format " *Dirvish Preview-%s*"
               (number-to-string (length dirvish-frame-list))))
    (setq-local mode-line-format nil)
    (current-buffer)))

(cl-defstruct (dirvish
               (:conc-name dirvish-)
               (:constructor make--dirvish))
  "Return a dirvish struct.

It has following fields:

ROOT-WINDOW is the main dirvish window who is adjacent to preview window.

ONE-WINDOW-P indicate if current dirvish is a single window instance.

HEADER-FRAME is the frame created by `posframe-show' for header display.

HEADER-BUFFER is a buffer created by
`dirvish--header-buffer-default'.

HEADER-WIDTH is the calculated header frame width.

PREVIEW-WINDOW is the window display file preview.

PREVIEW-BUFFER is a buffer created by
`dirvish--preview-buffer-default'.

PREVIEW-PIXEL-WIDTH is the pixelwise width of preview window.

WINDOW-CONF is the window configuration given by
`current-window-configuration'.

INDEX-PATH is the file path under cursor in ROOT-WINDOW.

SORT-CRITERIA is the sorting flag passed to `ls'.

SAVED-RECENTF is a backup of original `recentf-list'."
  root-window
  one-window-p
  header-frame
  (header-buffer (dirvish--header-buffer-default))
  header-width
  preview-window
  (preview-buffer (dirvish--preview-buffer-default))
  preview-pixel-width
  window-conf
  index-path
  (sort-criteria (cons "default" ""))
  (saved-recentf recentf-list))

(provide 'dirvish-structs)

;;; dirvish-structs.el ends here

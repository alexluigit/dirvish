;;; dirvish-structs.el --- Dirvish data structures -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; This file contains data structures defined in dirvish.

;;; Code:

(require 'dirvish-vars)

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

WINDOW-CONF is the window configuration given by
`current-window-configuration'.

HEADER-BUFFER is a buffer created by
`dirvish--header-buffer-default'.

PREVIEW-BUFFER is a buffer created by
`dirvish--preview-buffer-default'.
"
  window-conf
  (header-buffer (dirvish--header-buffer-default))
  (preview-buffer (dirvish--preview-buffer-default)))

(provide 'dirvish-structs)

;;; dirvish-structs.el ends here

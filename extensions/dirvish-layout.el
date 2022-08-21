;;; dirvish-layout.el --- Manage window layouts of Dirvish sessions -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.9.23
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This extension provided several commands that can help with managing the
;; window layout of Dirvish sessions.

;;; Code:

(require 'dirvish)

(defcustom dirvish-layout-recipes
  '((0 0    0.4)   ;        | CURRENT | preview
    (0 0    0.8)   ;        | current | PREVIEW
    (1 0.08 0.8)   ; parent | current | PREVIEW
    (1 0.11 0.55)) ; parent | current | preview
  "Layout RECIPEs for `dirvish-layout-switch' command.
RECIPE has the same form as `dirvish-default-layout'."
  :group 'dirvish
  :type '(repeat (list (integer :tag "number of parent windows")
                       (float :tag "max width of parent windows")
                       (float :tag "width of preview window"))))

;;;###autoload
(define-obsolete-function-alias 'dirvish-toggle-fullscreen #'dirvish-layout-toggle "Jul 22, 2022")
;;;###autoload
(defun dirvish-layout-toggle ()
  "Toggle layout of current Dirvish session.
A session with layout means it has a companion preview window and
possibly one or more parent windows."
  (interactive)
  (let* ((dv (dirvish-curr))
         (old-layout (dv-layout dv))
         (new-layout (unless old-layout (dv-last-fs-layout dv)))
         (buf (current-buffer)))
    (if old-layout
        (set-window-configuration (dv-window-conf dv))
      (with-selected-window (dv-root-window dv) (quit-window)))
    (setf (dv-layout dv) new-layout)
    (dirvish--save-env dv)
    (with-selected-window (dirvish--create-root-window dv)
      (dirvish-with-no-dedication (switch-to-buffer buf))
      (dirvish--build dv)
      (dirvish-debounce nil (dirvish-preview-update dv)))))

;;;###autoload
(define-obsolete-function-alias 'dirvish-switch-layout #'dirvish-layout-switch "Jul 22, 2022")
;;;###autoload
(defun dirvish-layout-switch (&optional recipe)
  "Switch Dirvish layout according to RECIPE.
If RECIPE is not provided, switch to the recipe next to the
current layout defined in `dirvish-layout-recipes'."
  (interactive)
  (cl-loop
   with dv = (let ((dv (dirvish-curr)))
               (unless dv (user-error "Not in a Dirvish session"))
               (unless (dv-layout dv)
                 (dirvish-layout-toggle)
                 (user-error "Dirvish: entering fullscreen")) dv)
   with old-recipe = (dv-layout dv)
   with recipes = (if recipe (list recipe) dirvish-layout-recipes)
   with l-length = (length recipes)
   for idx from 1
   for recipe in recipes
   when (or (eq idx l-length) (equal old-recipe recipe))
   return
   (let* ((new-idx (if (> idx (1- l-length)) 0 idx))
          (new-recipe (nth new-idx recipes)))
     (setf (dv-layout dv) new-recipe)
     (setf (dv-last-fs-layout dv) new-recipe)
     (dirvish--build dv))))

;;;###autoload
(defun dirvish-dwim (&optional path)
  "Start a Dirvish session with optional PATH.
The session takes the whole frame when `one-window-p'."
  (interactive (list (and current-prefix-arg (read-directory-name "Dirvish: "))))
  (let ((path (expand-file-name (or path default-directory)))
        (layout (and (one-window-p) dirvish-default-layout))
        (dv (dirvish-prop :dv)))
    (if (and dv (dv-layout dv))
        (dirvish-find-entry-ad path)
      (or (dirvish--reuse-session path layout)
          (dirvish-new :path path :layout layout)))))

(provide 'dirvish-layout)
;;; dirvish-layout.el ends here

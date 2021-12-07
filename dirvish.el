;;; dirvish.el --- a modern file manager based on dired mode. -*- lexical-binding: t -*-
;; Copyright (C) 2021 Alex Lu

;; Author : Alex Lu <https://github.com/alexluigit>
;; Package-Version: 0.7.0
;; Keywords: dirvish, ranger, file, dired
;; Homepage: https://github.com/alexluigit/dirvish.el
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "26") (posframe "1.1.2"))

;;; Commentary:

;; `dirvish.el' is a minimalistic file manager based on `dired-mode'.  It is inspired by ranger (see
;; https://github.com/ranger/ranger), which is a terminal file manager that shows a stack of the
;; parent directories, and updates its parent buffers while navigating the file system with an
;; optional preview window at side showing the content of the selected file.

;; Unlike `ranger.el', which tries to become an all-around emulation of ranger, dirvish.el is more
;; bare-bone, meaning it does NOT try to port all "goodness" from ranger, instead, it tries to:
;;
;;   - provides a better Dired UI
;;   - make some Dired commands more intuitive
;;   - keep all your Dired key bindings
;;
;; The name `dirvish' is a tribute to `vim-dirvish'.

;;; Code:

(require 'dirvish-commands)

(provide 'dirvish)

;;; dirvish.el ends here

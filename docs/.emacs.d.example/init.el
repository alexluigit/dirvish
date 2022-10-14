;;; init.el --- Emacs init file -*- lexical-binding: t -*-

;;;; install packages

(straight-use-package 'bind-key)
(straight-use-package 'dirvish)
(straight-use-package 'all-the-icons)
(straight-use-package 'diredfl)
(straight-use-package 'vertico)
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'ef-themes)
(let ((cache (expand-file-name
              "all-the-icons-font-installed" user-emacs-directory)))
  (unless (file-exists-p cache)
    (all-the-icons-install-fonts t)
    (with-temp-buffer (write-file cache))))

;;;; setup packages

;;;;; appearance
(require 'ef-themes)
(load-theme 'ef-night t)
(add-hook 'dired-mode-hook 'diredfl-mode)
(add-hook 'dirvish-directory-view-mode-hook 'diredfl-mode)
(with-eval-after-load 'diredfl
  (set-face-attribute 'diredfl-dir-name nil :bold t))

;;;;; minibuffer
(vertico-mode 1)
(setq completion-styles '(orderless))
(setq orderless-component-separator #'orderless-escapable-split-on-space)
(setq orderless-matching-styles
      '(orderless-initialism orderless-prefixes orderless-regexp))

;;;;; dirvish
(dirvish-override-dired-mode)
(dirvish-peek-mode)
(dirvish-side-follow-mode)
(setq dirvish-attributes
      '(vc-state file-size git-msg subtree-state all-the-icons collapse file-time))
(setq dirvish-mode-line-format '(:left (sort symlink) :right (vc-info yank index)))
(setq dirvish-header-line-height '(25 . 35))
(setq dirvish-side-width 38)
(setq dirvish-header-line-format '(:left (path) :right (free-space)))
(setq dired-listing-switches
      "-l --almost-all --human-readable --group-directories-first --no-group")
(bind-keys ("C-c f" . dirvish-fd)
           :map 'dirvish-mode-map
           ;; left click for expand/collapse dir or open file
           ("<mouse-1>" . dirvish-subtree-toggle-or-open)
           ;; middle click for opening file / entering dir in other window
           ("<mouse-2>" . dired-mouse-find-file-other-window)
           ;; right click for opening file / entering dir
           ("<mouse-3>" . dired-mouse-find-file)
           ([remap dired-sort-toggle-or-edit] . dirvish-quicksort)
           ([remap dired-do-redisplay] . dirvish-ls-switches-menu)
           ([remap dired-do-copy] . dirvish-yank-menu)
           ("?"   . dirvish-dispatch)
           ("q"   . dirvish-quit)
           ("a"   . dirvish-quick-access)
           ("f"   . dirvish-file-info-menu)
           ("x"   . dired-do-delete)
           ("X"   . dired-do-flagged-delete)
           ("y"   . dirvish-yank-menu)
           ("s"   . dirvish-quicksort)
           ("TAB" . dirvish-subtree-toggle)
           ("M-t" . dirvish-layout-toggle)
           ("M-b" . dirvish-history-go-backward)
           ("M-f" . dirvish-history-go-forward)
           ("M-n" . dirvish-narrow)
           ("M-m" . dirvish-mark-menu)
           ("M-s" . dirvish-setup-menu)
           ("M-e" . dirvish-emerge-menu))

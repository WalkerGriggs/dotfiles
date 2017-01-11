;; ----------------------------------------------------------------------
;; file:        ~/.emacs
;; author:      WalkerGriggs     www.walkergriggs.com
;; date:        10/03/2016
;; ----------------------------------------------------------------------

;; Melpa
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'default-frame-alist '(font . "Inconsolata-13"))

;; Tabs
(setq-default indent-tabs-mode nil) ;; Always spaces
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; General Necessities
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq-default truncate-lines 0)
(show-paren-mode 1)

;; General Key Bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)

;; Menu Bars
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Clipboard and Selection Mode
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Prog Hook
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Window Splitting Made Easy
(global-set-key [f1] 'split-window-horizontally)
(global-set-key [f2] 'split-window-vertically)
(global-set-key [f3] 'delete-window)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'ample t)

;; Powerline
(use-package powerline
  :ensure t
  :config (powerline-default-theme))

;; Autopair
(use-package autopair
  :init
  (dolist (hook '(prog-mode-hook conf-mode-hook))
    (add-hook hook #'autopair-mode)))

;; Whitespace
(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (global-whitespace-mode 1) ;; Whitespace ON.
  (setq whitespace-line-column 80) ;; Set indent limit.
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (setq whitespace-display-mappings
        '(
          (space-mark 32 [183] [46])
          (newline-mark 10 [172 10])
          (tab-mark 9 [9655 9] [92 9])
          )))

;; Fringe
(use-package fringe
  :config
  (setq-default left-fringe-width  10)
  (setq-default right-fringe-width  10))

;; Scroll Bar
(use-package scroll-bar
  :config
  (scroll-bar-mode -1)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil))

;; Linum
(use-package linum
  :config
  (global-linum-mode t)
  (setq linum-format "%d")
  (setq column-number-mode t))

;; Smex
;;(use-package smex
;;  :ensure t
 ;; :bind
 ;; (("M-x" . smex))
 ;; (("M-X" . smex-major-mode-commands))
 ;; (("C-c C-c M-x" . execute-extended-command)))

;; Helm
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :config
  (setq helm-split-window-in-side-p t ;; opens helm inside window
        helm-scroll-amount          8)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (setq helm-mode-fuzzy-match t))

;; Flycheck
(use-package flycheck
  :init
  (dolist (hook '(org-mode-hook))
    (add-hook hook #'flyspell-mode)))

;; Org-Bullets
(use-package org-mode
  :config (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))

;; Idle Highlight
(use-package idle-highlight-mode
  :ensure t
  :config (idle-highlight-mode 1)
  :init
  (dolist (hook '(prog-mode-hook conf-mode-hook))
    (add-hook hook #'idle-highlight-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-create-file-auto-open t)
 '(neo-dont-be-alone nil)
 '(neo-keymap-style (quote concise))
 '(neo-show-header nil)
 '(neo-show-hidden-files t)
 '(neo-theme (quote ascii)
 '(neo-window-width 20))
 '(org-startup-truncated t))

;; NeoTree
(use-package neotree
  :ensure t
  :init (neotree-show)
  :bind (([f4] . neotree-toggle)))

;; Cide
(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

;; Useful definitions

(defun lorem-ipsum ()
  "Insert a lorem ipsum."
    (interactive)
      (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
              "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
              "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
              "aliquip ex ea commodo consequat. Duis aute irure dolor in "
              "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
              "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
              "culpa qui officia deserunt mollit anim id est laborum."))

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%m_%d_%y" (current-time))))

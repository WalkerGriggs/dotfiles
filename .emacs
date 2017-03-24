;; ------------------------------------------------------------
;; file:        ~/.emacs
;; author:      WalkerGriggs     www.walkergriggs.com
;; date:        01_14_17
;; ------------------------------------------------------------

;; Melpa
;; Make sure to use httpS archives.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)

;; Lets secure out editor just a bit
;; Found at: https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(setq tls-checktrust t)

;; Because Emacs no longer trusts https, we need to distribute trust root certs
;; I'm using PyPi 'certifi' which can be installed in pip.
;; I'm also using gnutls-cli (Debian: gnutls-bin. OSX: gnutls)
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

(add-to-list 'default-frame-alist '(font . "Inconsolata-13"))

;; Tabs
(setq-default indent-tabs-mode nil) ;; Always spaces
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; UTF8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; General Necessities
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq-default truncate-lines 0)
(show-paren-mode 1)

;; General Key Bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)

;; Window Splitting Made Easy
(global-set-key [f1] 'split-window-horizontally)
(global-set-key [f2] 'split-window-vertically)
(global-set-key [f3] 'delete-window)

;; Menu Bars
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Clipboard and Selection Mode
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

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

;; Idle Highlight
(use-package idle-highlight-mode
  :ensure t
  :config (idle-highlight-mode 1)
  :init
  (dolist (hook '(prog-mode-hook conf-mode-hook))
    (add-hook hook #'idle-highlight-mode)))

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

;; Whitespace
(use-package whitespace
  :bind (("C-c C-w" . whitespace-mode))
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (global-whitespace-mode t) ;; Whitespace ON.
  (setq whitespace-global-modes '(not org-mode))
  (setq whitespace-line-column 80) ;; Set indent limit.
  (setq whitespace-display-mappings
        '(
          (space-mark 32 [183] [46])
          (newline-mark 10 [172 10])
          (tab-mark 9 [9655 9] [92 9]))))

;; Org
(use-package org
  :ensure t
  :mode (("\\.org$" . org-mode))
  :bind (("C-c C-x C-i" . org-clock-in)
         ("C-c C-x C-o" . org-clock-out)
         ("C-c C-x C-j" . org-clock-goto)
         ("C-c C-x C-r" . org-clock-report))
  :config
  (progn
    (define-key org-mode-map "\M-q" 'toggle-truncate-lines)
    (setq org-directory "~/org")
    (setq org-clock-persist t)
    (setq org-clock-mode-line-total 'current)
    ;; config stuff
    ))

;; Org-Bullets
(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Helm
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :config
  (setq helm-split-window-in-side-p        t ;; opens helm inside window
        helm-move-to-line-cycle-in-source  t
        helm-autoresize-min-height         20
        helm-autoresize-max-height         35
        helm-scroll-amount                 8)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (setq helm-mode-fuzzy-match t))

(use-package magit
  :ensure t
  :bind (("C-c s" . magit-status)
         ("C-c d" . magit-diff)))

;; Smex
;;(use-package smex
;;  :ensure t
 ;; :bind
 ;; (("M-x" . smex))
 ;; (("M-X" . smex-major-mode-commands))
;; (("C-c C-c M-x" . execute-extended-command)))

;; Multiple Cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; Rainbow Mode
(use-package rainbow-mode
  :ensure t
  :bind (("C-c C-r" . rainbow-mode))
  :init
  (dolist (hook '(prog-mode-hook html-mode-hook sass-mode-hook scss-mode-hook))
    (add-hook hook 'rainbow-mode)))

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
 '(neo-theme (quote ascii))
 '(org-startup-truncated t))

;; NeoTree
(use-package neotree
  :ensure t
  :bind (([f4] . neotree-toggle)))

;; Cider
(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

;; Python
(add-hook 'python-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil)
    (setq tab-width 4)
    (setq python-indent 4)))

;; Web Mode
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)))

;; Flycheck
(use-package flycheck
  :init
  (dolist (hook '(org-mode-hook))
    (add-hook hook #'flyspell-mode)))

;; Latex
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'Latex-mode-hook #'flyspell-buffer)
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil)))

;; Flymake
(use-package flymake :disabled t)

;; Flyspell
(use-package flyspell
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)
    (add-hook 'text-mode-hook #'flyspell-mode)))

;; MySQL
(use-package sqlup-mode
  :ensure t)

(use-package sql-mysql
  :init
  (progn
    (add-hook 'sql-mode-hook 'sqlup-mode)
    (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
    (add-hook 'sql-interactive-mode-hook
              (lambda ()
                (toggle-truncate-lines t))))
  :config
  (global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region))

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

(defun insert-header (filename &optional args)
  "Insert custom header onto file. (File, author, and date)"
  (interactive "*fInsert file name: \nP")
  (insert "------------------------------------------------------------\n")
  (insert "file:        ")
  (cond ((eq '- args)
           (insert (file-relative-name filename)))
          ((not (null args))
           (insert (expand-file-name filename)))
          (t
           (insert filename)))
  (insert "\nauthor:      WalkerGriggs     www.walkergriggs.com\n")
  (insert "date:        ")
  (insert (format-time-string "%m_%d_%y" (current-time)))
  (insert "\n------------------------------------------------------------\n"))
(global-set-key "\C-c\C-i" 'insert-header)

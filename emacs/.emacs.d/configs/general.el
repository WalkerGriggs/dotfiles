;; Doom Theme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t)) ; if nil, italics is universally disabled

;; Theme / Font
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'default-frame-alist '(font . "SourceCodePro-11"))
(load-theme 'doom-city-lights t)

;; Doom Configs
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
(doom-themes-org-config)

;; Startup
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell nil)

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
(global-set-key "\C-cr" 'rgrep)

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

;; Break Lines (for Dashboard)
(use-package page-break-lines
  :ensure t
  :config
  (page-break-lines-mode))

;; Startup Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title ""
        dashboard-startup-banner 3
        dashboard-items '((bookmarks . 5)
                          (recents . 10))))

;; Recentf
(use-package recentf
  :config ;; remove agenda files from list.
  (setq recentf-exclude '("work.org"
                          "school.org"
                          "life.org")
        recentf-max-saved-items 500
        recentf-max-menu-items 15))

;; Powerline
(use-package powerline
  :ensure t
  :init (progn
          (setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
          (powerline-default-theme)))

;; Autopair
(use-package autopair
  :config
  (dolist (hook '(prog-mode-hook conf-mode-hook))
    (add-hook hook #'autopair-mode)))

;;Idle Highlight
(use-package idle-highlight-mode
  :ensure t

  :config (idle-highlight-mode 1)
  :init
  (dolist (hook '(prog-mode-hook conf-mode-hook))
    (add-hook hook #'idle-highlight-mode)))

;; Fringe
(use-package fringe
  :ensure nil
  :config
  (setq-default left-fringe-width  20
                right-fringe-width  10))

;; Scroll Bar
(use-package scroll-bar
  :ensure nil
  :config
  (scroll-bar-mode -1)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
        mouse-wheel-progressive-speed nil))

;; Linum
(use-package linum
  :config
  (global-linum-mode t)
  (setq linum-format "%d"
        column-number-mode t))

;; Flycheck
(use-package flycheck
  :init
  (dolist (hook '(org-mode-hook))
    (add-hook hook #'flyspell-mode)))

;; Flymake
(use-package flymake :disabled t)

;; Flyspell
(use-package flyspell
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)
    (add-hook 'text-mode-hook #'flyspell-mode)))

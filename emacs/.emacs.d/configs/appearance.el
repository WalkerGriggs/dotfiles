;; Doom Theme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t)) ; if nil, italics is universally disabled

;; Theme / Font
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'default-frame-alist '(font . "SourceCodePro-11"))
(load-theme 'gruvbox-dark-medium t)

;; Doom Configs
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
(doom-themes-org-config)

;; Menu Bars
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Powerline
(use-package powerline
  :ensure t
  :init (progn
          (setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
          (powerline-default-theme)))

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

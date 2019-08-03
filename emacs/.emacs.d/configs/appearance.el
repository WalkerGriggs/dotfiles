;; Menu Bars
(tool-bar-mode -1)
(menu-bar-mode -1)

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

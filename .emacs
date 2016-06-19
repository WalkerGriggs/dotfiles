;; ----------------------------------------------------------------------
;; file:        ~/.emacs
;; author: 	WalkerGriggs	- www.walkergriggs.com
;; date: 	06/18/2016
;; ----------------------------------------------------------------------

(when (>= emacs-major-version 24)
    (require 'package)
    (package-initialize)
    (add-to-list 'package-archives '("melpa". "http://melpa.milkbox.net/packages/")t))

    (when (not package-archive-contents)
        (package-refresh-contents))

;; Org-Bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))

;; White Space
(require 'whitespace)
(setq-default indent-tabs-mode nil)
(setq whitespace-line-column 80) ;; Set line limit/
(global-set-key [f3] 'whitespace-mode)

;; Linum Mode
(global-linum-mode t)
(setq linum-format "%3d ")
(set-fringe-mode 0)

;; Scroll Bar
(scroll-bar-mode -1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Menu Bar
(menu-bar-mode -1)
(global-set-key [f1] 'menu-bar-mode)

;; Tool Bar
(tool-bar-mode -1)
(global-set-key [f2] 'tool-bar-mode)

;; Jade Mode
(add-to-list 'load-path "~/.emacs.d/jade-mode")
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl\\'" . sws-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(neo-show-hidden-files t)
 '(neo-theme (quote ascii)))

;; NeoTree
(require 'neotree)
(global-set-key [f4] 'neotree-toggle)
(neotree-show)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-dir-link-face ((t (:foreground "DeepSkyBlue")))))

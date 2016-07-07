;; ----------------------------------------------------------------------
;; file:        ~/.emacs
;; author:      WalkerGriggs     www.walkergriggs.com
;; date:        06/18/2016
;; ----------------------------------------------------------------------

;; Sources referenced:
;; http://aaronbedra.com/emacs.d/#start-up-options

;; Melpa
(when (>= emacs-major-version 24)
    (require 'package)
    (package-initialize)
    (add-to-list 'package-archives '("mepla" . "https://mepla.milkbox.net/packages/")t))

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/spacegray")
(load-theme 'spacegray t)

;; Load Path
(add-to-list 'load-path "~/.emacs.d/lisp/")

     ;; General Necessities
(setq tab-width 4)
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq-default truncate-lines 0)

;; General Key Bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)

;; Fringe
(setq-default left-fringe-width  10)
(setq-default right-fringe-width  10)

;; Menu Bars
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Scroll Bar
(scroll-bar-mode -1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; White Space
(require 'whitespace)
(global-whitespace-mode 1) ;; Whitespace ON.
(setq-default indent-tabs-mode nil) ;; Always spaces.
(setq whitespace-line-column 80) ;; Set indent limit.
(add-hook 'prog-mode-hook 'whitespace-mode)

(setq whitespace-display-mappings
  ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)g
  '(
    (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (newline-mark 10 [172 10]) ; 10 LINE FEED
    (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
    ))

(set-face-attribute 'whitespace-space nil :background nil :foreground "gray30")
(set-face-attribute 'whitespace-newline nil :background nil :foreground "gray30")

;; Clipboard and Selection Mode
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Org-Bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))

;; Linum Mode
(global-linum-mode t)
(setq linum-format "%d")
(setq column-number-mode t)

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
 '(minimap-always-recenter nil)
 '(minimap-automatically-delete-window nil)
 '(minimap-dedicated-window t)
 '(minimap-hide-fringes t)
 '(minimap-highlight-line t)
 '(minimap-recenter-type (quote free))
 '(minimap-width-fraction 0.08)
 '(minimap-window-location (quote right))
 '(neo-create-file-auto-open t)
 '(neo-dont-be-alone nil)
 '(neo-keymap-style (quote concise))
 '(neo-show-header nil)
 '(neo-show-hidden-files t)
 '(neo-theme (quote ascii))
 '(neo-window-width 20))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-active-region-background ((t (:background "gray30")))))

;; NeoTree
(require 'neotree)
(global-set-key [f4] 'neotree-toggle)
(neotree-show)

;; MiniMap
(require 'minimap)
(minimap-mode t)

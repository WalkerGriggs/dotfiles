;; ----------------------------------------------------------------------
;; file:        ~/.emacs
;; author:      WalkerGriggs     www.walkergriggs.com
;; date:        09/01/2016
;; ----------------------------------------------------------------------

;; Sources referenced:
;; http://aaronbedra.com/emacs.d/#start-up-options

;; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Refresh Packages
(when (not package-archive-contents)
  (package-refresh-contents))

;; My Packages
(defvar my-packages
  '(
    cider
    org-bullets
    smex
    neotree
    autopair
    idle-highlight-mode
  ))

;; Install my packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Font
(add-to-list 'default-frame-alist '(font . "Inconsolata-12"))

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'kooten t)

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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

;; Idle Highlight Mode
(defun idle-highlight-hook ()
  (idle-highlight-mode 1))

(add-hook 'emacs-lisp-mode-hook 'idle-highlight-hook)
(add-hook 'clojure-mode-hook 'idle-highlight-hook)
(add-hook 'python-mode-hook 'idle-highlight-hook)
(add-hook 'java-mode-hook 'idle-highlight-hook)
(add-hook 'js-mode-hook 'idle-highlight-hook)

;; Clipboard and Selection Mode
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Linum Mode
(global-linum-mode t)
(setq linum-format "%d")
(setq column-number-mode t)

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
 '(neo-window-width 20))

;; Org-Bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))

;; NeoTree
(require 'neotree)
(global-set-key [f4] 'neotree-toggle)
(neotree-show)

;; LANGUAGES

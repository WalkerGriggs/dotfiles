;; Startup
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell nil)

;; Tabs
(setq-default indent-tabs-mode nil) ;; Always spaces

;; UTF8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; General Necessities
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq-default truncate-lines 0)
(show-paren-mode 1)
(setq create-lockfiles nil)

;; General Key Bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; Clipboard and Selection Mode
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Window navigation
(global-set-key (kbd "C-x h") 'windmove-left)
(global-set-key (kbd "C-x j") 'windmove-down)
(global-set-key (kbd "C-x k") 'windmove-up)
(global-set-key (kbd "C-x l") 'windmove-right)

;; Text mode hooks
(add-hook 'text-mode-hook 'visual-line-mode)

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

;; Flycheck
(use-package flycheck
  :defer package-defer-time
  :config
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

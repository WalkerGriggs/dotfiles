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
        helm-scroll-amount                 8
        helm-mode-fuzzy-match t)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action))

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

;; NeoTree
(use-package neotree
  :ensure t
  :bind (([f4] . neotree-toggle)))

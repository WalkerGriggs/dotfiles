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

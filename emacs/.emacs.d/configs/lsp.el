(use-package lsp-mode
  :ensure t
  :defer package-defer-time ;; kick the load-time can
  :config
  ;;(add-hook 'python-mode-hook #'lsp)
  ;;(add-hook 'rust-mode-hook #'lsp)
  (add-hook 'ruby-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'lsp))

(use-package company
  :ensure t
  :defer package-defer-time
  :config
  (global-company-mode 1)
  (global-set-key (kbd "C-<tab>") 'company-complete))

(use-package company-lsp
  :requires company
  :defer package-defer-time
  :config
  (push 'company-lsp company-backends)

  (setq company-lsp-enable-snippet nil
        company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

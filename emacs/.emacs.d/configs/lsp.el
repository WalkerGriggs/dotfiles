;;; configs/lsp.el -*- lexical-binding: t; -*-
;;
;; Keywords: convenience, configs
;; This file is not part of GNU Emacs.

;;; Code

(use-package lsp-mode
  :defer package-defer-time ;; kick the load-time can
  :config
  ;;(add-hook 'python-mode-hook #'lsp)
  ;;(add-hook 'rust-mode-hook #'lsp)
  (add-hook 'ruby-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'lsp))

(use-package company
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
;;; lsp.el ends here

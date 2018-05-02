;; Cider
(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

;; Docker
(use-package dockerfile-mode
  :mode "Dockerfile.*\\'")

;; Nginx
(use-package nginx-mode
  :mode ("nginx\\.conf\\'" "/nginx/.+\\.conf\\'"))

;; Python
(add-hook 'python-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil
          tab-width 4
          python-indent 4)))

;; Go-mode
(add-hook 'before-save-hook #'gofmt-before-save)
(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save)
              (setq tab-width 2)
              (setq indent-tabs-mode 1))))

;; Terraform
(use-package terraform-mode
  :ensure t
  :mode "\\.tf$"
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

;; Web Mode
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)))

;; Latex
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'Latex-mode-hook #'flyspell-buffer)
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil)))

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

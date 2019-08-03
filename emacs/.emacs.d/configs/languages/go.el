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

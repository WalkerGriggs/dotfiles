(use-package rust-mode
  :ensure t)

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

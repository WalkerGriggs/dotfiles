;; Whitespace
(use-package whitespace
  :bind (("C-c C-w" . whitespace-mode))
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-global-modes '(not org-mode LaTeX-mode latex-mode)
        whitespace-line-column 100 ;; Set indent limit.
        whitespace-display-mappings
        '(
          (space-mark 32 [183] [46])
          (newline-mark 10 [172 10])
          (tab-mark 9 [9655 9] [92 9]))))

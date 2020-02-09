;; Magit
(use-package magit
  :ensure t
  :bind (("C-c s" . magit-status)
         ("C-c d" . magit-diff)))

(use-package diff-hl
  :ensure t
  :init (global-diff-hl-mode)
  :config (setq diff-hl-side 'right))

;; Git Modes
(use-package gitattributes-mode
  :mode ("/\\.gitattributes\\'"
         "/info/attributes\\'"
         "/git/attributes\\'"))

(use-package gitconfig-mode
  :mode ("/\\.gitconfig\\'"
         "/\\.git/config\\'"
         "/modules/.*/config\\'"
         "/git/config\\'"
         "/\\.gitmodules\\'"
         "/etc/gitconfig\\'"))

(use-package gitignore-mode
  :mode ("/\\.gitignore\\'"
         "/info/exclude\\'"
         "/git/ignore\\'"))

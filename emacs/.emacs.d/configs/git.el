;; Magit
(use-package magit
  :ensure t
  :bind (("C-c s" . magit-status)
         ("C-c d" . magit-diff)))

;; Git Gutter
(use-package git-gutter-fringe+
  :ensure t
  :init (global-git-gutter+-mode))

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

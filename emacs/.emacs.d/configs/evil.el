(use-package evil
  :init
  (progn
    (setq evil-default-cursor t)

    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (progn
        (setq evil-leader/in-all-states t)
        ;; keyboard shortcuts
        (evil-leader/set-key
          "a" 'ag-project
          "A" 'ag
          "b" 'ido-switch-buffer
          "c" 'mc/mark-next-like-this
          "C" 'mc/mark-all-like-this
          "e" 'er/expand-region
          "E" 'mc/edit-lines
          "f" 'ido-find-file
          "g" 'magit-status
          "i" 'idomenu
          "j" 'ace-jump-mode
          "k" 'kill-buffer
          "K" 'kill-this-buffer
          "o" 'occur
          "p" 'magit-find-file-completing-read
          "r" 'recentf-ido-find-file
          "s" 'ag-project
          "t" 'bw-open-term
          "T" 'eshell
          "w" 'save-buffer
          "x" 'smex
          )))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t))

    ;; boot evil by default
  (evil-mode 1))

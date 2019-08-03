(use-package evil-leader
  :ensure t
  :defer t
  :config
  (global-evil-leader-mode))

(use-package evil-surround
  :ensure t
  :defer t
  :config
  (global-evil-surround-mode))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  (define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)

  (require 'evil-leader)
  (require 'evil-surround)

  (setq evil-default-cursor t)

  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "g" 'magit-status
    "k" 'kill-buffer
    "K" 'kill-this-buffer
    "l" 'kill-whole-line
    "o" 'occur
    "t" 'ansi-term
    "w" 'save-buffer
    ))

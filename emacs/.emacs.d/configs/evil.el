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

  (require 'evil-leader)
  (require 'evil-surround)

  (setq evil-default-cursor t
        evil-normal-state-cursor 'box
        evil-visual-state-cursor 'hollow)

  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "/") 'swiper)
    (define-key evil-normal-state-map (kbd "SPC SPC") 'counsel-M-x))

  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "s" 'magit-status
    "k" 'kill-buffer
    "K" 'kill-this-buffer
    "t" 'ansi-term
    "w" 'save-buffer
    ))

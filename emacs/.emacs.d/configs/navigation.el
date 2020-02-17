;;; configs/navigation.el -*- lexical-binding: t; -*-
;;
;; Keywords: convenience, configs
;; This file is not part of GNU Emacs.

;;; Code

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(global-set-key (kbd "C-x h") 'windmove-left)
(global-set-key (kbd "C-x j") 'windmove-down)
(global-set-key (kbd "C-x k") 'windmove-up)
(global-set-key (kbd "C-x l") 'windmove-right)

(use-package ivy
  :demand
  ;; :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy)
        ("M-x" . counsel-M-x))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format ""
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))))

(use-package counsel-projectile
  :bind
  (:map ivy-mode-map
        ("C-c f" . counsel-projectile-find-file)))

(use-package vimish-fold
  :commands (vimish-fold-toggle
             vimish-fold)
  :bind
    (("C-c C-f" . vimish-fold)
     ("C-c C-u" . vimish-fold-unfold)
     ("C-c C-t" . vimish-fold-toggle)
     ("C-c C-r" . vimish-fold-delete)))

;; hl-region
;; github.com/walkergriggs/hl-region
(use-package hl-region
  :load-path "hl-region" ;; or wherever it's cloned
  :commands (hl-region
             hl-region-remove)
  :bind
  (("C-c h h" . hl-region)
   ("C-c h r" . hl-region-remove)
   ("C-c h R" . hl-region-remove-all)
   ("C-c h n" . hl-region-next-highlight)
   ("C-c h p" . hl-region-prev-highlight)))

(use-package dumb-jump
  :bind (("C-c j g" . dumb-jump-go)
         ("C-c j b" . dumb-jump-back)
         ("C-c j q" . dumb-jump-quick-look)
         ("C-c j p" . dumb-jump-prompt))
  :config (setq dumb-jump-selector 'ivy
                dumb-jump-aggressive t
                dumb-jump-confirm-jump-to-modified-file t))
;;; navigation.el ends here

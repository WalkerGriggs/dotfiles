;; Latex
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook #'LaTeX-math-setup)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'Latex-mode-hook #'flyspell-buffer)
    (add-hook 'LaTeX-mode-hook #'visual-line-mode)
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil)))

(use-package latex-preview-pane
  :ensure t
  :bind (("C-c l p" . latex-preview-pane-mode)
         ("C-c l u" . latex-preview-pane-update))
  :init
  (progn
    (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (bind-keys :map pdf-view-mode-map
             ("\\" . hydra-pdftools/body)
             ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
             ("g"  . pdf-view-first-page)
             ("G"  . pdf-view-last-page)
             ("l"  . image-forward-hscroll)
             ("h"  . image-backward-hscroll)
             ("j"  . pdf-view-next-page)
             ("k"  . pdf-view-previous-page)
             ("e"  . pdf-view-goto-page)
             ("u"  . pdf-view-revert-buffer)
             ("al" . pdf-annot-list-annotations)
             ("ad" . pdf-annot-delete)
             ("aa" . pdf-annot-attachment-dired)
             ("am" . pdf-annot-add-markup-annotation)
             ("at" . pdf-annot-add-text-annotation)
             ("y"  . pdf-view-kill-ring-save)
             ("i"  . pdf-misc-display-metadata)
             ("s"  . pdf-occur)
             ("b"  . pdf-view-set-slice-from-bounding-box)
             ("r"  . pdf-view-reset-slice))
  (use-package org-pdfview
    :ensure t))

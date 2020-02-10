;;; configs/whitespace.el -*- lexical-binding: t; -*-
;;
;; Keywords: convenience, configs
;; This file is not part of GNU Emacs.

;;; Code

;; Whitespace
(use-package whitespace
  :bind (("C-c C-w" . whitespace-mode))
  :init
  (dolist (hook '(prog-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 100 ;; Set indent limit.
        whitespace-display-mappings
        '(
          (space-mark 32 [183] [46])
          (newline-mark 10 [172 10])
          (tab-mark 9 [9655 9] [92 9]))))
;;; whitespace.el ends here

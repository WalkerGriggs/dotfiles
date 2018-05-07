(use-package hungry-delete
  :bind (("C-c C-d" . global-hungry-delete-mode))
  :config
  (progn
    (setq hungry-delete-chars-to-skip " \t\r\f\v")

    ;; `hungry-delete-skip-ws-backward'; do not delete back-slashes at EOL.
    (defun hungry-delete-skip-ws-forward ()
      "Skip over any whitespace following point.
This function skips over horizontal and vertical whitespace."
      (skip-chars-forward hungry-delete-chars-to-skip)
      (while (get-text-property (point) 'read-only)
        (backward-char)))

    (defun hungry-delete-skip-ws-backward ()
      "Skip over any whitespace preceding point.
This function skips over horizontal and vertical whitespace."
      (skip-chars-backward hungry-delete-chars-to-skip)
      (while (get-text-property (point) 'read-only)
        (forward-char)))

    (defun modi/turn-off-hungry-delete-mode ()
      "Turn off hungry delete mode."
      (hungry-delete-mode -1))

    ;; Enable `hungry-delete-mode' everywhere ..
    (global-hungry-delete-mode)

    ;; Except ..
    (add-hook 'minibuffer-setup-hook #'modi/turn-off-hungry-delete-mode)))

(provide 'setup-hungry-delete)

;; Whitespace
(use-package whitespace
  :bind (("C-c C-w" . whitespace-mode))
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (global-whitespace-mode t) ;; Whitespace ON.
  (setq whitespace-global-modes '(not org-mode)
        whitespace-line-column 100 ;; Set indent limit.
        whitespace-display-mappings
        '(
          (space-mark 32 [183] [46])
          (newline-mark 10 [172 10])
          (tab-mark 9 [9655 9] [92 9]))))

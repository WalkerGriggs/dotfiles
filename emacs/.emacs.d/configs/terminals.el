(use-package bash-completion
  :ensure t
  :config (bash-completion-setup))

(use-package ansi-color
  :ensure t
  :init
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'term-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
  (defadvice display-message-or-buffer (before ansi-color activate)
    "Process ANSI color codes in shell output."
    (let ((buf (ad-get-arg 0)))
      (and (bufferp buf)
           (string= (buffer-name buf) "*Shell Command Output*")
           (with-current-buffer buf
             (ansi-color-apply-on-region (point-min) (point-max)))))))

;; Ansi-Term
;; No use-package here because it wasn't playing nice with ansi-term.

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; Close buffer with 'exit'
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Always use UTF-8
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

;; Paste with C-y
(defun my-term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

(defun my-term-hook ()
  (goto-address-mode)               ; Clickable URLs
  (bind-key "C-y" #'my-term-paste term-raw-map))
(add-hook 'term-mode-hook 'my-term-hook)

(add-hook 'term-mode-hook (lambda()
        (setq yas-dont-activate t)))
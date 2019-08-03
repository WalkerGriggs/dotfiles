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

(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)))

(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

(add-hook 'term-exec-hook 'set-no-process-query-on-exit)

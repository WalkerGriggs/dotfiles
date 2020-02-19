;;; configs/erc.el -*- lexical-binding: t; -*-
;;
;; Keywords: convenience, configs
;; This file is not part of GNU Emacs.

;;; Code

(use-package erc
  :defer t
  :bind (("C-c n" . erc-next-channel-buffer))
  :init
  (defun erc-count-users ()
    "Displays the number of users connected on the current channel."
    (interactive)
    (if (get-buffer "irc.freenode.net:6667")
        (let ((channel (erc-default-target)))
          (if (and channel (erc-channel-p channel))
              (message "%d users are online in %s"
                       (hash-table-count erc-channel-users)
                       channel)
            (user-error "The current buffer is not a channel")))
      (user-error "You must first start ERC")))

  (defun erc-preprocess (string)
    "Avoids channel flooding by remove hidden newlines."
    (setq str
          (string-trim
           (replace-regexp-in-string "\n+" " " str))))

  (defvar erc-channel-stack nil
    "Channels that have not yet been visited by erc-next-channel-buffer")

  (defun erc-next-channel-buffer ()
    "Switch current window to the next, univisted channel buffer."
    (interactive)
    (when (null erc-channel-stack)
      (setq erc-channel-stack
            (remove (current-buffer) (erc-channel-list nil))))
    (let ((target (pop erc-channel-stack)))
      (if target
          (switch-to-buffer target))))

  :config
  (erc-track-mode t)
  (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules))))

  (setq erc-kill-buffer-on-part        t
        erc-kill-queries-on-quit       t
        erc-kill-server-buffer-on-quit t

        erc-fill-column                98 ;; defaults to 78
        erc-fill-function              #'erc-fill-static
        erc-fill-static-center         18

        erc-track-excludes-types       '("JOIN" "NICK" "PART" "QUIT" "MODE")
        erc-server-coding-system       '(utf-8 . utf-8)

        erc-prompt (lambda () (concat "" (buffer-name) "]")))

(use-package erc-hl-nicks
  :after erc)

(use-package erc-image
  :after erc
  :init (with-eval-after-load 'erc
          (require 'erc-image)
          (add-to-list 'erc-modules 'image)))

(use-package erc-yt
  :after erc
  :init (with-eval-after-load 'erc
          (require 'erc-youtube)
          (add-to-list 'erc-modules 'youtube)))

;;; erc.el ends here

;; ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;; ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;; █████╗  ██╔████╔██║███████║██║     ███████╗
;; ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;; ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;; ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;
;; Walker Griggs
;; walker@walkergriggs.com
;; github.com/WalkerGriggs


;; Load all configs
(defconst toc:emacs-config-dir "~/.emacs.d/configs/" "")

(defun toc:load-config-file (filelist)
  (dolist (file filelist)
    (load (expand-file-name
           (concat toc:emacs-config-dir file)))
    (message "Loaded config file:%s" file)
    ))

(toc:load-config-file '("packages" ;; always bootsrap packages first
                        "general"
                        "whitespace"
                        "platform"
                        "org"
                        "languages"
                        "git"
                        "terminals"
                        ))

(add-hook 'emacs-startup-hook
          (lambda ()
            (split-window-horizontally)
            ;; (ansi-term "/bin/bash")
            ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("242ed4611e9e78142f160e9a54d7e108750e973064cee4505bfcfc22cc7c61b1" "31cda5d49c1b7d595b7bafa9fc73ad5db51fd93077423224d3e1f9beb7446118" default)))
 '(package-archives
   (quote
    (("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("org" . "https://orgmode.org/elpa/"))))
 '(use-package-always-ensure t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

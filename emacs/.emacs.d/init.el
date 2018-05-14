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
                        "evil"
                        "general"
                        "platform"
                        "org"
                        "git"
                        "terminals"
                        "languages"
                        "latex"
                        "whitespace"
                        "appearance"
                        "custom-set-variables"
                        ))

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; (split-window-horizontally)
            ;; (ansi-term "/bin/bash")
            ))

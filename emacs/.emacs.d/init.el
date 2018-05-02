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
                        "platform"
                        "org"
                        "languages"
                        ))

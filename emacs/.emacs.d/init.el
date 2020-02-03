(defvar start-time (current-time))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'no-littering nil :noerror)

(setq user-full-name "Walker Griggs")
(setq package-defer-time 3)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; The default value is 0.8MB, which is just too small.
(setq gc-cons-threshold 50000000)

(defconst toc:emacs-config-dir "~/.emacs.d/configs/" "")

;; Set the file for custom set variables
(setq custom-file (expand-file-name "custom.el" "~/.emacs.d/"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load all configs
(defun toc:load-config-file (filelist)
  (dolist (file filelist)
    (load (expand-file-name
           (concat toc:emacs-config-dir file)))
    (message "Loaded config file:%s" file)))

(toc:load-config-file '("packages" ;; always bootsrap packages first
                        "general"
                        "appearance"
                        "navigation"
                        "evil"
                        "git"
                        "terminals"
                        "whitespace"
                        "languages/ruby"
                        "languages/go"
                        "languages/python"
                        "languages/rust"
                        "lsp")) ;; load the heaviest (and defered) package last

(defvar init-duration (float-time
                       (time-since
                        start-time)))

;; Welcome message
(setq initial-scratch-message
      (format "%s\
;; %d packages loaded in %.3fs.
;;
;;  ____________________________
;; | Welcome to Emacs %s,
;; |      %s
;;  ----------------------------
;;         \\   ^__^
;;          \\  (oo)\\_______
;;             (__)\\       )\\/\\
;;                 ||----w |
;;                 ||     ||
"
              initial-scratch-message
              (length package-alist)
              init-duration
              emacs-version
              user-full-name))

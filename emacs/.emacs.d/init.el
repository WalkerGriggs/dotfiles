(defvar init-start-time (current-time))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(defvar base-dir (file-name-directory load-file-name)
  "The base directory for configuration files.")

(defvar startup-dir (expand-file-name "startup/" base-dir)
  "The directory to store elisp scripts that runs at the very beginning.")

(defvar config-dir (expand-file-name "configs/" base-dir)
  "The directory to place configurations.")

;; Load custom-set-variable file
(setq custom-file (expand-file-name "custom.el" base-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Init sequence
(add-to-list 'load-path startup-dir)
(require '01-globals)
(require '02-defuns)
(require '03-packages)

(defvar init-duration (float-time
                       (time-since
                        init-start-time)))

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

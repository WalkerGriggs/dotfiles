;;; init.el --- Emacs config initializer
;;
;;     .-''-.  ,---.    ,---.   ____        _______      .-'''-.
;;   .'_ _   \ |    \  /    | .'  __ `.    /   __  \    / _     \
;;  / ( ` )   '|  ,  \/  ,  |/   '  \  \  | ,_/  \__)  (`' )/`--'
;; . (_ o _)  ||  |\_   /|  ||___|  /  |,-./  )       (_ o _).
;; |  (_,_)___||  _( )_/ |  |   _.-`   |\  '_ '`)      (_,_). '.
;; '  \   .---.| (_ o _) |  |.'   _    | > (_)  )  __ .---.  \  :
;;  \  `-'    /|  (_,_)  |  ||  _( )_  |(  .  .-'_/  )\    `-'  |
;;   \       / |  |      |  |\ (_ o _) / `-'`-'     /  \       /
;;    `'-..-'  '--'      '--' '.(_,_).'    `._____.'    `-...-'
;;
;; Copyright © 2016–present Walker Griggs <walker@walkergriggs.com>
;;
;; Author: Walker Griggs <walker@walkergriggs.com>
;; URL: https://github.com/walkergriggs/dotfiles/emacs
;; Keywords: convenience, configuration
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code

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
;;; init.el ends here

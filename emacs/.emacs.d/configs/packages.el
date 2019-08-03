;; Melpa
;; Make sure to use httpS archives.
(require 'package)
(customize-set-variable
 'package-archives
 '(("melpa" . "https://melpa.org/packages/")
   ("melpa-stable" . "https://stable.melpa.org/packages/")
   ("gnu" . "https://elpa.gnu.org/packages/")
   ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Always ensure packages
(customize-set-variable 'use-package-always-ensure t)
(setq use-package-verbose t)

;; Always update packges
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

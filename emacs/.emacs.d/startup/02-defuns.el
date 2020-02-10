;;; startup/02-defuns.el -*- lexical-binding: t; -*-
;;
;; Keywords: convenience, startup
;; This file is not part of GNU Emacs.

;;; Code

;; Load all configs
(defun load-config-file (filelist)
  (dolist (file filelist)
    (load (expand-file-name
           (concat config-dir file)))
    (message "Loaded config file:%s" file)))

(provide '02-defuns)
;;; 02-defuns.el ends here

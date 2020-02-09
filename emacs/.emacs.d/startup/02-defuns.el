;; Load all configs
(defun load-config-file (filelist)
  (dolist (file filelist)
    (load (expand-file-name
           (concat config-dir file)))
    (message "Loaded config file:%s" file)))

(provide '02-defuns)

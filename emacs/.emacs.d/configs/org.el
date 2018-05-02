;; Org
(use-package org
  :ensure t
  :mode (("\\.org$" . org-mode))
  :bind (("C-c C-x C-i" . org-clock-in)
         ("C-c C-x C-o" . org-clock-out)
         ("C-c C-x C-j" . org-clock-goto)
         ("C-c C-x C-r" . org-clock-report))
  :config
  (progn
    (define-key org-mode-map "\M-q" 'toggle-truncate-lines)
    (setq org-directory "~/org"
          org-clock-persist t
          org-clock-mode-line-total 'current
          org-agenda-deadline-warning-days 4
          org-agenda-files (list "~/.org/work.org"
                                 "~/.org/school.org"
                                 "~/.org/life.org"
                                 "/ssh:wpgriggs@walkergriggs.com:/home/wpgriggs/.org/life.org")
          org-agenda-custom-commands
          '(("c" "Simple agenda view"
             (
              (tags "PRIORITY=\"A\""
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-overriding-header "High-priority unfinished tasks:")))
              (agenda "")
              (alltodo ""
                       ((org-agenda-view-columns-initially t)
                        (org-agenda-overriding-header "Global TODO:")))
              )
             ))
          )
    (define-key global-map "\C-ca" 'org-agenda)))

;; Org-Bullets
(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

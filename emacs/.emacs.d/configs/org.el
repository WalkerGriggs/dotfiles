;;; configs/org.el -*- lexical-binding: t; -*-
;;
;; Keywords: convenience, configs
;; This file is not part of GNU Emacs.

;;; Code

(use-package org
  :mode (("\\.org$" . org-mode))
  :config
  (setq org-indent-mode              t
        org-list-allow-alphabetical  t
        org-src-tabs-act-natively    t
        org-src-preserve-indentation t
        org-src-fontify-natively     t
        org-clock-persist            t
        org-clock-mode-line-total    'current)

  (add-to-list 'org-structure-template-alist
               '("s" "#+BEGIN_SRC ? \n\n#+END_SRC"))

  (add-to-list 'org-structure-template-alist
               '("q" "#+BEGIN_QUOTE ? \n\n#+END_QUOTE"))

  (defun org-italcs-region-or-point ()
    "Italicize the region, word or character at point."
    (interactive)
    (org-markup-region-or-point 'italics "/" "/"))

  (defun org-bold-region-or-point ()
    "Bold the region, word or character at point."
    (interactive)
    (org-markup-region-or-point 'bold "*" "*"))

  (defun org-underline-region-or-point ()
    "Underline the region, word or character at point."
    (interactive)
    (org-markup-region-or-point 'underline "_" "_"))

  (defun org-code-region-or-point ()
    "Mark the region, word or character at point as code."
    (interactive)
    (org-markup-region-or-point 'underline "~" "~"))

  (defun org-verbatim-region-or-point ()
    "Mark the region, word or character at point as verbatim."
    (interactive)
    (org-markup-region-or-point 'underline "=" "="))

  (defun org-markup-region-or-point (type beginning-marker end-marker)
    "Apply the markup TYPE with BEGINNING-MARKER and END-MARKER to region, word or point."
    (cond
     ;; We have an active region we want to apply
     ((region-active-p)
      (let* ((bounds (list (region-beginning) (region-end)))
             (start (apply 'min bounds))
             (end (apply 'max bounds))
             (lines))
        (unless (memq type '(subscript superscript))
          (save-excursion
            (goto-char start)
            (unless (looking-at " \\|\\<")
              (backward-word)
              (setq start (point)))
            (goto-char end)
            (unless (or (looking-at " \\|\\>")
                        (looking-back "\\>" 1))
              (forward-word)
              (setq end (point)))))
        (setq lines
              (s-join "\n" (mapcar
                            (lambda (s)
                              (if (not (string= (s-trim s) ""))
                                  (concat beginning-marker
                                          (s-trim s)
                                          end-marker)
                                s))
                            (split-string
                             (buffer-substring start end) "\n"))))
        (setf (buffer-substring start end) lines)
        (forward-char (length lines))))
     ;; We are on a word with no region selected
     ((thing-at-point 'word)
      (cond
       ;; beginning of a word
       ((looking-back " " 1)
        (insert beginning-marker)
        (re-search-forward "\\>")
        (insert end-marker))
       ;; end of a word
       ((looking-back "\\>" 1)
        (insert (concat beginning-marker end-marker))
        (backward-char (length end-marker)))
       ;; not at start or end so we just sub/sup the character at point
       ((memq type '(subscript superscript))
        (insert beginning-marker)
        (forward-char (- (length beginning-marker) 1))
        (insert end-marker))
       ;; somewhere else in a word and handled sub/sup. mark up the
       ;; whole word.
       (t
        (re-search-backward "\\<")
        (insert beginning-marker)
        (re-search-forward "\\>")
        (insert end-marker))))
     ;; not at a word or region insert markers and put point between
     ;; them.
     (t
      (insert (concat beginning-marker end-marker))
      (backward-char (length end-marker))))))


(use-package org-bullets
  :commands (org-bullets-mode)
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;; org.el ends here

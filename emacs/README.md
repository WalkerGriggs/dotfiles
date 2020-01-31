# .emacs.d

I was given some pretty solid (albeit a wee bit off-color) advice my freshman year of college.

> Choose your text editor wisely. Chances are, you'll be with it longer than your spouse.

I can't possibly tell you what came over me in that moment, but I chose Emacs. I've tried a number of times over the past few years to jump ship to a more "modern" editor, but Emacs keeps dragging me back. More specifically, my config is doing the dragging. Here's a little bit more about it...


## Structure

The file strcture is quite flat. The `init.el` file at the root bootstraps each 'module' in the config directory. Each config module is named pretty clearly; appearance contains theme/font settings, whitespace contains whitespace settings, evil contains evil mode... and so on).

```
.
├── configs
│   ├── appearance.el
│   ├── evil.el
│   ├── general.el
│   ├── git.el
│   ├── languages
│   │   ├── go.el
│   │   ├── markdown.el
│   │   ├── python.el
│   │   ├── ruby.el
│   │   └── rust.el
│   ├── lsp.el
│   ├── navigation.el
│   ├── packages.el
│   ├── terminals.el
│   └── whitespace.el
└── init.el
```

## Notable configs

I generally try to keep the configs simple. My rule of thumb: if I don't already use it / have a need for it, I probably shouldn't configure it.

### Evil Mode

Emacs with Vim bindings is the way to go, and I much prefer a modal editor. My evil config is actually quite tame compared to the defaults enabeld by Doom, but this is all the non-default behavior I care to define. I also run `evil-leader-mode` and `evil-surround-mode` globally.

```lisp
(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  (define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)

  (require 'evil-leader)
  (require 'evil-surround)

  (setq evil-default-cursor t)

  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "/") 'swiper))

  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "g" 'magit-status
    "k" 'kill-buffer
    "K" 'kill-this-buffer
    "l" 'kill-whole-line
    "t" 'ansi-term
    "w" 'save-buffer
    ))
```

### Whitespace

I generally like to keep whitespace mode enabled. Some people think it looks cluttered, but I had a nasty habbit or leaving trailing whitespace scattered around, which this config promply broke me of. I also want to avoid mixing tabs and spaces -- yuck.

```lisp
;; Whitespace
(use-package whitespace
  :bind (("C-c C-w" . whitespace-mode))
  :init
  (dolist (hook '(prog-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-global-modes '(not org-mode LaTeX-mode latex-mode)
        whitespace-line-column 100 ;; Set indent limit.
        whitespace-display-mappings
        '(
          (space-mark 32 [183] [46])
          (newline-mark 10 [172 10])
          (tab-mark 9 [9655 9] [92 9]))))
```

### Ivy

I recently switched over from Helm to Ivy, and it has really made a world of difference. Virtual buffers and fuzzy regex for file searching has improved my workflow dramatically.

```lisp
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy)
        ("M-x" . counsel-M-x))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format ""
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))))
```

I also install counsel-projectile for fuzzy finding files across an entire project in Ivy mode. Again, a game changer.

```lisp
(use-package counsel-projectile
  :ensure t
  :bind
  (:map ivy-mode-map
        ("C-x f" . counsel-projectile-find-file)))
```


### Terms

My terminal configs are perhaps the most complicated part of my config, and, frankly, the least used. Emacs just doesn't hold a candle to a lot of the modern terminal emulators out there. That said, I do crack open `ansi-term` from time to time, and would like it to feel at least somewhat familiar when I do.

The first step was proper 256 color support:

```lisp
(use-package ansi-color
  :ensure t
  :init
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'term-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
  (defadvice display-message-or-buffer (before ansi-color activate)
    "Process ANSI color codes in shell output."
    (let ((buf (ad-get-arg 0)))
      (and (bufferp buf)
           (string= (buffer-name buf) "*Shell Command Output*")
           (with-current-buffer buf
             (ansi-color-apply-on-region (point-min) (point-max)))))))
```

I force UTF-8...

```lisp
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)
```

disable process query on exit...

```lisp
(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))
```

and actually close the buffer with `exit`. I'm surprised that isn't already a sensible default.

```lisp
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)
```

(require 'package)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(use-package my :load-path "lisp/")

(setq
 custom-file (locate-user-emacs-file "custom.el")
 inhibit-startup-screen t
 make-backup-files nil
 auto-save-default nil
 sentence-end-double-space nil
 ring-bell-function 'ignore
 org-edit-src-content-indentation 0)
(load custom-file 'noerror)
(setq-default truncate-lines t
              indent-tabs-mode nil
              cursor-type 'bar)
(global-display-line-numbers-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(my-font-change-size 0)
(use-package modus-themes :ensure t)
(use-package standard-themes :ensure t)
(use-package ef-themes :ensure t)
(use-package doric-themes :ensure t)
(modus-themes-select 'standard-light-tinted)
(use-package spacious-padding
  :ensure t
  :config (spacious-padding-mode 1))
(use-package loon-line
  :load-path "lisp/"
  :config (setq-default mode-line-format loon-line))
(use-package prot-ptyxis :load-path "lisp/")

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t))
(use-package consult :ensure t)
(use-package marginalia
  :ensure t
  :config (marginalia-mode 1))
(use-package vertico
  :ensure t
  :config (vertico-mode 1))
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map ("C-<backspace>" . vertico-directory-delete-word)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package evil
  :ensure t
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-define-key
    '(normal motion visual) 'global
    (kbd "q") 'bury-buffer))
(use-package general
  :after evil
  :ensure t
  :config
  (general-create-definer leader-def :prefix "SPC")
  (leader-def
    :states '(normal motion visual)
    :keymaps 'override
    "SPC" #'execute-extended-command
    "u" #'universal-argument
    "k" #'kill-current-buffer
    "e" #'eshell
    "v" #'vterm
    "-" (lambda () (interactive) (my-font-change-size -10))
    "=" (lambda () (interactive) (my-font-change-size +10))
    "x" (my-key-prefix "C-x ")
    "f" (my-key-prefix "C-x C-")
    "c" (my-key-prefix "C-c ")
    "j" (my-key-prefix "C-c C-")
    "h" (my-key-prefix "C-h ")
    "p" (my-key-prefix "C-x p ")
    "r" (my-key-prefix "C-x r ")
    "b" #'consult-buffer
    "l" #'consult-line
    "a" #'my-arrange-window
    "s" #'other-window
    "d" #'delete-window))
(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

(use-package vterm
  :ensure t
  :defer t
  :custom (vterm-shell "fish"))
(use-package tramp
  :defer t
  :custom (tramp-histfile-override t)
  :config (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
(use-package dired
  :defer t
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-dwim-target t)
  :hook (dired-mode . hl-line-mode))
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  :hook (prog-mode . corfu-mode)
  :bind (:map corfu-map ("RET" . nil)))
(use-package cape :ensure t)
(use-package eglot
  :custom (eglot-autoshutdown t)
  :hook (prog-mode . eglot-ensure)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)
  :bind
  (:map eglot-mode-map
        ("C-c r" . 'eglot-rename)
        ("C-c f" . 'eglot-format)
        ("C-c a" . 'eglot-code-actions)))
(use-package magit
  :ensure t
  :defer t
  :config (define-key transient-map (kbd "<escape>") 'transient-quit-one))
(use-package forge
  :after magit
  :ensure t
  :custom (auth-sources '("~/.ssh/authinfo")))
(use-package yasnippet
  :ensure t
  :custom (yas-prompt-functions '(yas-no-prompt))
  :config (yas-global-mode 1))
(use-package elec-pair
  :custom (electric-pair-skip-self nil)
  :hook (prog-mode . electric-pair-local-mode))
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(my-install-languages
 auctex!
 dockerfile
 fish
 go
 just
 markdown
 meson
 nix
 rust
 toml
 zig)

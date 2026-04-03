(require 'package)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

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
(defconst font-name-mono "IBM Plex Mono")
(defconst font-name-sans "IBM Plex Sans")
(defvar font-size 140)
(defun font-change-size (increment)
  (setq font-size (+ font-size increment))
  (set-face-attribute
   'default nil
   :family font-name-mono
   :height font-size)
  (set-face-attribute
   'variable-pitch nil
   :family font-name-sans
   :height font-size)
  (message "Set font size to %d" font-size))
(font-change-size 0)
(use-package doric-themes
  :ensure t
  :config (doric-themes-select 'doric-oak))
(use-package spacious-padding
  :ensure t
  :config (spacious-padding-mode 1))
(use-package loon-line
  :load-path "lisp/"
  :config (setq-default mode-line-format loon-line))

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

(use-package vterm
  :ensure t
  :custom (vterm-shell "fish"))
(use-package tramp
  :custom (tramp-histfile-override t)
  :config (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(defmacro zucchini (prefix)
  "Returns a lambda that will execute an input sequence in the form \"Pe\".
\"P\" is the argument PREFIX. \"e\" is the first input event recorded
after the invocation of the lambda."
  `(lambda ()
     (interactive)
     (let (message-log-max) (message "%s" ,prefix))
     (let* ((next-key (single-key-description (read-event)))
            (sequence (concat ,prefix next-key))
            (command (key-binding (kbd sequence))))
       (if (and command (not (keymapp command)))
           (call-interactively command)
         (message "%s is undefined" sequence)))))
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
(defun arrange-window ()
  (interactive)
  (select-window
   (if (< (window-pixel-height) (window-pixel-width))
       (split-window-horizontally)
     (split-window-vertically))))
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
    "v" (lambda () (interactive) (vterm))
    "-" (lambda () (interactive) (font-change-size -10))
    "=" (lambda () (interactive) (font-change-size +10))
    "x" (zucchini "C-x ")
    "f" (zucchini "C-x C-")
    "c" (zucchini "C-c ")
    "j" (zucchini "C-c C-")
    "h" (zucchini "C-h ")
    "p" (zucchini "C-x p ")
    "r" (zucchini "C-x r ")
    "b" #'consult-buffer
    "l" #'consult-line
    "a" #'arrange-window
    "s" #'other-window
    "d" #'delete-window))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-forge-setup)
  (evil-collection-init))

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
(use-package yasnippet
  :ensure t
  :custom (yas-prompt-functions '(yas-no-prompt))
  :config (yas-global-mode 1))
(use-package magit
  :ensure t
  :defer t
  :config (define-key transient-map (kbd "<escape>") 'transient-quit-one))
(use-package forge
  :ensure t
  :custom (auth-sources '("~/.ssh/authinfo")))
(use-package elec-pair
  :custom (electric-pair-skip-self nil)
  :hook (prog-mode . electric-pair-local-mode))
(use-package dired
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-dwim-target t)
  :hook (dired-mode . hl-line-mode))
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))
(use-package markdown-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package nix-mode :ensure t)
(use-package go-mode :ensure t)
(use-package meson-mode :ensure t)
(use-package toml-mode :ensure t)
(use-package zig-mode :ensure t)
(use-package haskell-mode :ensure t)
(use-package tuareg :ensure t)
(use-package fish-mode :ensure t)
(use-package rust-mode :ensure t)
(use-package just-mode :ensure t)
(use-package auctex :ensure t)

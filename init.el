(setq auto-save-default nil)
(setq make-backup-files nil)
(set-default 'truncate-lines t)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq-default tab-width 4)
(set-frame-font "Jetbrains Mono-14" nil t)

;; Setup package
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Setup theme
(use-package doom-themes :config (load-theme 'doom-gruvbox t))

;; Setup evil
(use-package undo-fu)
(use-package
 evil
 :config (evil-mode 1)
 :init (setq evil-undo-system 'undo-fu))

;; Setup electric-pairs
(setq electric-pair-pairs '((?\" . ?\") (?\{ . ?\})))
(electric-pair-mode 1)

;; Setup go-mode
(use-package go-mode)

;; Setup vertico
(use-package
 orderless
 :custom
 (completion-styles '(orderless basic))
 (completion-category-defaults nil)
 (completion-category-overrides
  '((file (styles partial-completion)))))
(use-package vertico :config (vertico-mode 1))

;; Setup yasnippet
(use-package
 yasnippet
 :config
 (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
 (yas-global-mode 1))

;; Setup corfu
(use-package cape)
(use-package
 corfu
 :custom (corfu-auto t)
 :config
 (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
 (advice-add
  'eglot-completion-at-point
  :around #'cape-wrap-noninterruptible)
 :bind
 (:map corfu-map ([tab] . corfu-next) ([backtab] . corfu-previous))
 :hook
 ((c++-mode go-mode java-mode emacs-lisp-mode) . corfu-mode))

;; Setup eglot
(use-package eglot :hook ((go-mode java-mode c++-mode) . eglot-ensure))

;; Setup rainbow delimiters
(use-package
 rainbow-delimiters
 :hook
 ((emacs-lisp-mode go-mode java-mode c++-mode)
  .
  rainbow-delimiters-mode))

;; Setup tree sitter
(use-package
 tree-sitter
 :init (require 'tree-sitter)
 :hook ((go-mode java-mode c++-mode) . tree-sitter-mode))

(use-package
 tree-sitter-langs
 :init (require 'tree-sitter-langs)
 :hook ((go-mode java-mode c++-mode) . tree-sitter-hl-mode))

;; Setup CP
(use-package web-server)
(require 'cp)

;; Setup general
(use-package
 general
 :init
 (setq general-override-states '(normal))
 (require 'keybindings))

;; Set custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(message "Startup finished.")

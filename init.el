(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq default-directory "~/")
(setq auto-save-default nil)
(setq make-backup-files nil)
(set-default 'truncate-lines t)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(setq initial-major-mode 'org-mode)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(set-frame-font "Jetbrains Mono-14" nil t)

;; Setup package
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
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
(use-package evil
  :config (evil-mode 1)
  :init (setq evil-undo-system 'undo-fu))

;; Setup electric-pairs
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})))
(electric-pair-mode 1)

;; Setup go
(use-package go-mode)

;; Setup vertico
(use-package vertico :config (vertico-mode 1))

;; Setup yasnippet
(use-package yasnippet :init (add-hook 'go-mode-hook #'yas-minor-mode))

;; Setup corfu
(use-package corfu
  :custom (corfu-auto t)
  :bind
  (:map corfu-map
        ([tab] . corfu-next)
        ([backtab] . corfu-previous))
  :hook ((go-mode . corfu-mode)))

;; Setup eglot
(use-package eglot :init (add-hook 'go-mode-hook 'eglot-ensure))

;; If you want java working
;(cl-defmethod eglot-execute-command
;  (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
;  "Eclipse JDT breaks spec and replies with edits as arguments."
;  (mapc #'eglot--apply-workspace-edit arguments))

;; Setup rainbow delimiters
(use-package rainbow-delimiters :init (add-hook 'go-mode-hook 'rainbow-delimiters-mode))

;; Setup tree sitter
(use-package tree-sitter :init (require 'tree-sitter))
(use-package tree-sitter-langs :init (require 'tree-sitter-langs))
(add-hook 'go-mode-hook #'tree-sitter-mode)
(add-hook 'go-mode-hook #'tree-sitter-hl-mode)

;; Setup general
(use-package general
  :init
  (setq general-override-states '(normal))
  (require 'keybindings))

;; Setup powerline
(require 'powerline)
(powerline-default-theme)

;; Set custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(message "Startup finished.")

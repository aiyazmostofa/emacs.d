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
(set-frame-font "Jetbrains Mono-16" nil t)
(add-to-list 'exec-path (concat user-emacs-directory "jdtls/bin"))

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

;; Setup go-mode
(use-package go-mode)

;; Setup dart-mode
(use-package dart-mode)

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

;; Setup vertico
(use-package vertico :config (vertico-mode 1))

;; Setup yasnippet
(use-package yasnippet :init
  (add-hook 'java-mode-hook #'yas-minor-mode)
  (add-hook 'go-mode-hook #'yas-minor-mode)
  (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode)
  (add-hook 'html-mode-hook #'yas-minor-mode)
  (add-hook 'dart-mode-hook #'yas-minor-mode)
  (add-hook 'latex-mode-hook #'yas-minor-mode)
  (add-hook 'js-mode-hook #'yas-minor-mode)
  (add-hook 'css-mode-hook #'yas-minor-mode)
  (add-hook 'python-mode-hook #'yas-minor-mode))

;; Setup corfu
(use-package corfu
  :custom (corfu-auto t)
  :bind
  (:map corfu-map
        ([tab] . corfu-next)
        ([backtab] . corfu-previous))
  :hook
  (java-mode . corfu-mode)
  (go-mode . corfu-mode)
  (html-mode . corfu-mode)
  (dart-mode . corfu-mode)
  (latex-mode . corfu-mode)
  (css-mode . corfu-mode)
  (js-mode . corfu-mode)
  (emacs-lisp-mode . corfu-mode)
  (python-mode . corfu-mode))

;; Setup eglot
(use-package eglot
  :hook
  (java-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  (html-mode . eglot-ensure)
  (latex-mode . eglot-ensure)
  (dart-mode . eglot-ensure)
  (css-mode . eglot-ensure)
  (js-mode . eglot-ensure)
  (python-mode . eglot-ensure))

;; Setup rainbow delimiters
(use-package rainbow-delimiters
  :hook
  (java-mode . rainbow-delimiters-mode)
  (go-mode . rainbow-delimiters-mode)
  (html-mode . rainbow-delimiters-mode)
  (dart-mode . rainbow-delimiters-mode)
  (latex-mode . rainbow-delimiters-mode)
  (css-mode . rainbow-delimiters-mode)
  (js-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (python-mode . rainbow-delimiters-mode))

;; Setup tree sitter
(use-package tree-sitter
  :init
  (require 'tree-sitter)
  (add-hook 'java-mode-hook #'tree-sitter-mode)
  (add-hook 'go-mode-hook #'tree-sitter-mode)
  (add-hook 'js-mode-hook #'tree-sitter-mode)
  (add-hook 'python-mode-hook #'tree-sitter-mode))

(use-package tree-sitter-langs
  :init
  (require 'tree-sitter-langs)
  (add-hook 'java-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'go-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'js-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'python-mode-hook #'tree-sitter-hl-mode))

(org-babel-do-load-languages
 'org-babel-load-languages '((gnuplot . t)))
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("gnuplot"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; Setup CP Enjoyer
(use-package web-server)
(require 'cp)
(setenv "CP_ENJOYER" "1")

;; Setup general
(use-package general
  :init
  (setq general-override-states '(normal))
  (require 'keybindings))

;; Set custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(message "Startup finished.")

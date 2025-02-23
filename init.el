;; Dispose of custom settings in custom-file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Disable nativecomp messages
(setq native-comp-async-report-warnings-errors nil)

;; Stop the emacs save/backup shit
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Cleanup the look
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(when (find-font (font-spec :name "Jetbrains Mono"))
  (set-frame-font "Jetbrains Mono-12" nil t))

;; Set scratch to org-mode, along with home page
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq initial-major-mode 'org-mode)

;; Set line settings
(set-default 'truncate-lines t)
(global-display-line-numbers-mode)

;; Setup package management
(require 'package)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'use-package)

;; Setup themes ef-themes
(use-package
 ef-themes
 :ensure t
 :config (load-theme 'ef-dream :no-confirm))

;; Setup evil-mode
(use-package undo-fu :ensure t)
(use-package
 evil
 :ensure t
 :init (setq evil-undo-system 'undo-fu)
 :config (evil-mode 1))

;; Setup spacemaster
(use-package
 spacemaster
 :config (evil-global-set-key 'normal (kbd "SPC") 'spacemaster))

;; Set escape to quit to make life easier
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Run a eshell script called cmd.el from the current directory
(global-set-key
 (kbd "C-c c")
 (lambda ()
   (interactive)
   (when (file-exists-p "cmd.el")
     (load-file "cmd.el"))))

;; Install copeforces
(use-package web-server :ensure t)
(use-package copeforces :bind (("C-c C" . copeforces)))

;; Electric pairs
(setq electric-pair-pairs '((?\" . ?\") (?\{ . ?\})))
(electric-pair-mode 1)

;; Setup vertico
(use-package
 orderless
 :ensure t
 :custom
 (completion-styles '(orderless basic))
 (completion-category-defaults nil)
 (completion-category-overrides
  '((file (styles partial-completion)))))
(use-package vertico :ensure t :config (vertico-mode 1))

;; Setup company
(use-package company :ensure t :hook (emacs-lisp-mode . company-mode))

;; Setup rainbow-delimiters
(use-package
 rainbow-delimiters
 :ensure t
 :config
 :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; Setup eglot
(use-package
 eglot
 :ensure t
 :init (setq eglot-autoshutdown t)
 :config
 (add-hook
  'eglot-managed-mode-hook
  (lambda ()
    (interactive)
    (rainbow-delimiters-mode 1)
    (company-mode 1)))
 :hook ((c-mode c++-mode) . eglot-ensure)
 :bind
 (:map
  eglot-mode-map
  ("C-c r" . eglot-rename)
  ("C-c f" . eglot-format)
  ("C-c a" . eglot-code-actions)))

;; Setup elisp-autofmt
(use-package
 elisp-autofmt
 :ensure t
 :bind
 (:map emacs-lisp-mode-map ("C-c f" . elisp-autofmt-buffer)))

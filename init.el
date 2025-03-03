;; Dispose of custom settings in custom-file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; General settings
(setq
 native-comp-async-report-warnings-errors nil
 auto-save-default nil
 make-backup-files nil
 ring-bell-function 'ignore)

;; Home page settings
(setq
 inhibit-splash-screen t
 initial-scratch-message ""
 initial-major-mode 'org-mode)

;; Configure appearence
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(when (find-font (font-spec :name "Jetbrains Mono"))
  (set-frame-font "Jetbrains Mono-10" nil t))
(setq-default truncate-lines t)
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
 :config (load-theme 'ef-tritanopia-dark :no-confirm))

;; Install mood-line
(use-package mood-line :ensure t :config (mood-line-mode))

;; Electric pairs
(setq electric-pair-pairs '((?\" . ?\") (?\{ . ?\})))
(electric-pair-mode 1)

;; Setup vertico
(use-package
 orderless
 :ensure t
 :custom (completion-styles '(orderless basic))
 (completion-category-overrides
  '((file (styles partial-completion)))))
(use-package vertico :ensure t :config (vertico-mode 1))

;; Setup company
(use-package
 company
 :ensure t
 :config (setq company-tooltip-align-annotations t)
 :hook (emacs-lisp-mode . company-mode))

;; Setup rainbow-delimiters
(use-package
 rainbow-delimiters
 :ensure t
 :config
 :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; Install treesitter (for web dev only ): )
(use-package tree-sitter :ensure t)
(use-package
 tree-sitter-langs
 :ensure t
 :hook ((tsx-ts-mode) . setup-tide-mode))

;; Install magit
(use-package magit :ensure t :bind (("C-c m" . magit)))

;; Install programming modes
(use-package go-mode :ensure t)

;; Install tide (for web dev)
(use-package
 tide
 :ensure t
 :init
 (defun setup-tide-mode ()
   (interactive)
   (tide-setup)
   (flycheck-mode 1)
   (eldoc-mode 1)
   (tide-hl-identifier-mode 1)
   (rainbow-delimiters-mode 1)
   (company-mode 1))
 :hook ((typescript-mode typescript-ts-mode) . setup-tide-mode)
 :config (setq flycheck-check-syntax-automatically '(save mode-enabled))
 :bind
 (:map
  tide-mode-map
  ("C-c r" . tide-rename-symbol)
  ("C-c f" . tide-format)
  ("C-c a" . tide-fix)))

;; Setup eglot
(use-package
 eglot
 :ensure t
 :init
 (setq
  eglot-autoshutdown t
  eglot-events-buffer-config 0)
 :config
 (add-hook
  'eglot-managed-mode-hook
  (lambda ()
    (interactive)
    (rainbow-delimiters-mode 1)
    (company-mode 1)))
 :hook ((c-mode c++-mode go-mode python-mode) . eglot-ensure)
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

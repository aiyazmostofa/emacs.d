;; Dispose of custom settings in custom-file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

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

;; Make escape into quit to make life easier
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Setup package management
(require 'package)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package)

;; Setup theme
(use-package
 ef-themes
 :ensure t
 :config (load-theme 'ef-day :no-confirm))

;; Setup rainbow-delimiters
(use-package
 rainbow-delimiters
 :ensure t
 :hook (prog-mode . rainbow-delimiters-mode))

;; Setup evil-mode
(use-package undo-fu :ensure t)
(use-package
 evil
 :ensure t
 :init (setq evil-undo-system 'undo-fu)
 :config (evil-mode 1))

;; Special evil-mode space -> ctrl binding
(define-key
 key-translation-map " "
 '(menu-item
   "" event-apply-control-modifier
   :filter
   (lambda (cmd)
     (and (fboundp 'evil-normal-state-p)
          (evil-normal-state-p)
          (not isearch-mode)
          cmd))))
(evil-global-set-key 'normal (kbd "C-SPC") 'execute-extended-command)
(global-set-key
 (kbd "C-c c")
 (lambda ()
   (interactive)
   (when (file-exists-p "cmd.el")
     (load-file "cmd.el"))))

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
(use-package
 company
 :ensure t
 :hook ((c-mode c++-mode emacs-lisp-mode) . company-mode))

;; Setup eglot
(use-package
 eglot
 :ensure t
 :init (setq eglot-autoshutdown t)
 :hook ((c-mode c++-mode) . eglot-ensure)
 :bind
 (("C-c r" . eglot-rename)
  ("C-c f" . eglot-format)
  ("C-c a" . eglot-actions)))

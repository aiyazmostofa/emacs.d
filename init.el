;;; init.el --- The emacs config. -*- lexical-binding: t; -*-
;; Set gc cap high so gc is limited
(setq gc-cons-threshold (* 50 1000 1000))

;; Dispose of custom settings in custom-file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Setup package management
(require 'package)
(package-initialize)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/lisp")
(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

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
(when (find-font (font-spec :name "JetBrains Mono"))
  (set-frame-font "JetBrains Mono-16" nil t))
(setq-default truncate-lines t)
(global-display-line-numbers-mode)

;; Nice theme
(use-package
 ef-themes
 :ensure t
 :config (ef-themes-select 'ef-cyprus))

;; Set escape to quit to make life easier
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(with-eval-after-load 'transient
  (define-key transient-map (kbd "<escape>") 'transient-quit-one))

(defun kill-window-possibly-buffer ()
  "Kill the current window.
If the buffer associated with the window is not in any other window, kill it too."
  (interactive)
  (if (eq (length (window-list)) 1)
      (if (eq (length (get-buffer-window-list)) 1)
          (kill-current-buffer))
    (if (eq (length (get-buffer-window-list)) 1)
        (kill-buffer-and-window)
      (delete-window))))

;; Setup evil-mode
(use-package
 evil
 :ensure t
 :init (setq evil-undo-system 'undo-redo)
 :config (evil-mode 1)
 (evil-define-key
  '(normal visual motion)
  'global
  (kbd "q")
  'kill-window-possibly-buffer)
 (evil-define-key 'insert 'global (kbd "C-n") nil (kbd "C-p") nil)
 (evil-define-key
  'insert
  eshell-mode-map
  (kbd "C-p")
  'eshell-previous-matching-input-from-input
  (kbd "C-n")
  'eshell-next-matching-input-from-input))

;; Setup spacemaster
(use-package
 spacemaster
 :config
 (evil-define-key
  '(normal visual motion)
  'global
  (kbd "C-SPC")
  'execute-extended-command
  (kbd "SPC")
  'spacemaster))
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "SPC") 'spacemaster))
(setq dired-kill-when-opening-new-dired-buffer t)

;; Run a eshell script called cmd.el from the current directory
(global-set-key
 (kbd "C-c c")
 (lambda ()
   (interactive)
   (when (file-exists-p "cmd.el")
     (load-file "cmd.el"))))

;; Get eat
(use-package
 eat
 :ensure t
 :config (add-hook 'eshell-load-hook #'eat-eshell-mode))

;; Install copeforces
(use-package copeforces :bind (("C-c C" . copeforces)))

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

;; Setup corfu
(use-package
 corfu
 :ensure t
 :custom
 (corfu-auto t)
 (corfu-auto-delay 0.01)
 (corfu-auto-prefix 1)
 :hook (emacs-lisp-mode . corfu-mode))

;; Setup rainbow-delimiters
(use-package
 rainbow-delimiters
 :ensure t
 :config
 :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; Install magit
(use-package magit :ensure t :defer t)

;; Install treesitter
(use-package
 treesit-auto
 :ensure t
 :custom (treesit-auto-install 'prompt)
 :config
 (treesit-auto-add-to-auto-mode-alist 'all)
 (global-treesit-auto-mode))
(setq
 typescript-ts-mode-indent-offset 4
 tsx-ts-mode-indent-offset 4
 js-ts-mode-indent-offset 4)

;; Setup yasnippet
(use-package
 yasnippet
 :ensure t
 :hook ((emacs-lisp-mode . yas-minor-mode)))

;; Setup eglot
(use-package
 eglot
 :ensure t
 :init (setq eglot-autoshutdown t)
 :config (setf (plist-get eglot-events-buffer-config :size) 0)
 (add-hook
  'eglot-managed-mode-hook
  (lambda ()
    (interactive)
    (rainbow-delimiters-mode 1)
    (yas-minor-mode 1)
    (corfu-mode 1)))
 :hook
 ((c-ts-mode
   c++-ts-mode
   go-ts-mode
   python-ts-mode
   typescript-ts-mode
   tsx-ts-mode
   js-ts-mode
   html-ts-mode
   css-ts-mode)
  . eglot-ensure)
 :custom
 (eglot-ignored-server-capabilities
  '(:documentOnTypeFormattingProvider))
 :bind
 (:map
  eglot-mode-map
  ("C-c r" . eglot-rename)
  ("C-c f" . eglot-format)
  ("C-c a" . eglot-code-actions)
  ("C-c D" . xref-find-definitions)
  ("C-c R" . xref-find-references)))
(use-package
 eglot-booster
 :vc
 (:url "https://github.com/jdtsmith/eglot-booster.git" :rev :newest)
 :after eglot
 :config (eglot-booster-mode))

;; Setup elisp-autofmt
(use-package
 elisp-autofmt
 :ensure t
 :commands (elisp-autofmt-buffer)
 :bind
 (:map
  emacs-lisp-mode-map
  ("C-c f" .
   (lambda ()
     (interactive)
     (message "Formatting...")
     (elisp-autofmt-buffer)))
  ("C-c D" . xref-find-definitions) ("C-c R" . xref-find-references)))
;; Setup eshell keybinding
(defun eshell-buffer-p (buffer)
  (with-current-buffer buffer
    (eq major-mode 'eshell-mode)))
(defun switch-to-eshell-buffer ()
  (interactive)
  (let ((buffers (seq-filter #'eshell-buffer-p (buffer-list))))
    (cond
     ((eq (length buffers) 0)
      (message "No eshell buffers open."))
     ((eq (length buffers) 1)
      (switch-to-buffer (car buffers)))
     (t
      (switch-to-buffer
       (completing-read
        "Switch to eshell buffer: " (mapcar #'buffer-name buffers)
        nil t))))))
(global-set-key (kbd "C-c e") #'switch-to-eshell-buffer)
(global-set-key
 (kbd "C-c E")
 (lambda ()
   (interactive)
   (eshell t)))

;; Setup gc back to a decently normal level
(setq gc-cons-threshold (* 2 1000 1000))

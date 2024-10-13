(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))
(setq auto-save-default nil)
(setq make-backup-files nil)
(set-default 'truncate-lines t)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(set-frame-font "Jetbrains Mono-12" nil t)

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

;; Setup web-mode
(use-package
 web-mode
 :init
 (setq web-mode-markup-indent-offset 2)
 (setq web-mode-css-indent-offset 2)
 (setq web-mode-code-indent-offset 2)
 (setq web-mode-attr-indent-offset 2)
 (setq web-mode-attr-value-indent-offset 2)
 (setq web-mode-indentless-elements 2)
 (setq web-mode-markup-indent-offset 2)
 (setq web-mode-sql-indent-offset 2)
 (setq web-mode-style-padding 2)
 (setq web-mode-script-padding 2)
 (setq web-mode-block-padding 2))
;; Astro mode
(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode)) auto-mode-alist))

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
 ((c++-mode
   c-mode go-mode java-mode emacs-lisp-mode latex-mode astro-mode)
  . corfu-mode))


;; Java fix
(cl-defmethod eglot-execute-command
    (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
  "Eclipse JDT breaks spec and replies with edits as arguments."
  (mapc #'eglot--apply-workspace-edit arguments))

;; Setup eglot
(use-package
 eglot
 :init (setq eglot-autoshutdown t)
 :config
 (add-to-list
  'eglot-server-programs
  '(astro-mode
    .
    ("astro-ls"
     "--stdio"
     :initializationOptions
     (:typescript (:tsdk "./node_modules/typescript/lib")))))
 :hook
 ((go-mode java-mode c++-mode c-mode latex-mode astro-mode)
  . eglot-ensure))

;; Setup rainbow delimiters
(use-package
 rainbow-delimiters
 :hook
 ((emacs-lisp-mode go-mode java-mode c++-mode c-mode latex-mode)
  . rainbow-delimiters-mode))

;; Setup tree sitter
(use-package
 tree-sitter
 :init (require 'tree-sitter)
 :hook ((go-mode java-mode c++-mode c-mode) . tree-sitter-mode))

(use-package
 tree-sitter-langs
 :init (require 'tree-sitter-langs)
 :hook
 ((go-mode java-mode c++-mode c-mode) . tree-sitter-hl-mode))

;; Setup Markdown
(use-package markdown-mode)

;; Setup general
(use-package
 general
 :init (setq general-override-states 'normal)
 :config (general-create-definer leader :prefix "SPC")
 (leader
  :states 'normal
  :keymaps
  'override
  "SPC"
  'execute-extended-command
  "eq"
  'kill-emacs
  "fs"
  'save-buffer
  "ff"
  'find-file
  "bs"
  'switch-to-buffer
  "bk"
  'kill-current-buffer
  "be"
  'eval-buffer
  "wh"
  (lambda ()
    (interactive)
    (split-window-horizontally)
    (other-window 1))
  "wv"
  (lambda ()
    (interactive)
    (split-window-vertically)
    (other-window 1))
  "wo"
  'other-window
  "wd"
  (lambda ()
    (interactive)
    (if (eq (length (get-buffer-window-list)) 1)
        (kill-buffer-and-window)
      (delete-window)))
  "ca"
  'eglot-code-actions
  "cf"
  'eglot-format
  "cr"
  'eglot-rename
  "jk"
  (lambda ()
    (interactive)
    (if (file-exists-p "cmd.el")
        (load-file "cmd.el")))))

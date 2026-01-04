(require 'package)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq
 custom-file (locate-user-emacs-file "custom.el")
 make-backup-files nil
 auto-save-default nil
 sentence-end-double-space nil
 ring-bell-function 'ignore
 org-edit-src-content-indentation 0
 tramp-histfile-override t
 dired-listing-switches "-alh --group-directories-first"
 dired-dwim-target t)
(load custom-file 'noerror)
(setq-default truncate-lines t
              indent-tabs-mode nil
              cursor-type 'bar)
(global-display-line-numbers-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(defconst font-name "IBM Plex Mono")
(defvar font-size 140)
(defun font-change-size (increment)
  (setq font-size (+ font-size increment))
  (set-face-attribute
   'default nil
   :family font-name
   :height font-size)
  (message "Set font size to %d" font-size))
(font-change-size 0)
(use-package ef-themes
  :ensure t
  :custom (modus-themes-common-palette-overrides
           '((border-mode-line-active unspecified)
             (border-mode-line-inactive unspecified)))
  :config (modus-themes-select 'ef-symbiosis))

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

(defmacro zucchini (prefix)
  "Returns a lambda that will execute an input sequence in the form \"Pe\".
\"P\" is the argument PREFIX. \"e\" is the first input event recorded
after the invocation of the lambda."
  `(lambda ()
     (interactive)
     (let (message-log-max) ; Disable adding messages to the log.
       (message ,prefix)
       (let* ((next-key (single-key-description (read-event)))
              (sequence (concat ,prefix next-key))
              (command (key-binding (kbd sequence))))
         (if (and command (not (keymapp command)))
             (call-interactively command)
           (message (format "%s is undefined" sequence)))))))
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
  (select-window (if (< (window-pixel-height) (window-pixel-width))
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
  :config (evil-collection-init))

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
(use-package elec-pair
  :custom (electric-pair-skip-self nil)
  :hook (prog-mode . electric-pair-local-mode))
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package sly
  :ensure t
  :custom (sly-mrepl-history-file-name (locate-user-emacs-file "sly-mrepl-history")))
(use-package markdown-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package go-mode :ensure t)
(use-package zig-mode :ensure t)
(use-package rust-mode :ensure t)
(use-package just-mode :ensure t)
(use-package auctex :ensure t)

(setq-default
 mode-line-format
 '("%e"
   (:eval (propertize
           (cond
            ((not (bound-and-true-p evil-mode)) "")
            ((evil-normal-state-p) " <N> ")
            ((evil-replace-state-p) " <R> ")
            ((evil-motion-state-p) " <M> ")
            ((evil-operator-state-p) " <O> ")
            ((evil-visual-state-p) " <V> ")
            ((evil-visual-state-p) " <V> ")
            ((evil-insert-state-p) " <I> ")
            (t " <E> "))
           'face `(:background
                   ,(face-attribute (if (mode-line-window-selected-p)
                                        'font-lock-keyword-face
                                      'mode-line-inactive)
                                    :foreground)
                   :foreground
                   ,(face-attribute (if (mode-line-window-selected-p)
                                        'default
                                      'mode-line-inactive)
                                    :background))))
   (:eval (propertize (format " %s" (buffer-name)) 'face 'bold))
   (:eval (propertize
           (if (and buffer-file-name (buffer-modified-p)) " *" "")
           'face 'error))
   (:eval (propertize
           (if (and
                (display-graphic-p)
                (not (zerop text-scale-mode-amount)))
               (concat
                (if (> text-scale-mode-amount 0) " +" " ")
                (number-to-string text-scale-mode-amount))
             "")
           'face 'success))
   mode-line-format-right-align
   (:eval (format "(%s)" (capitalize
                          (string-replace
                           "-" " "
                           (substring
                            (symbol-name major-mode) 0 -5)))))
   (:eval (cond
           ((bound-and-true-p eglot--managed-mode) "[LSP] ")
           ((and (bound-and-true-p sly-mode) (sly-connected-p))
            "[SLY] ")
           (t " ")))))

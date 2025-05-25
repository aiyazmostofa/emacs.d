;;; init.el --- The emacs config. -*- lexical-binding: t; -*-

;; This setting is set so that the GC isn't triggered as often when
;; Emacs is starting up. This is to improve startup times. This
;; setting is set to a more reasonable value at the end of this file
;; to improve memory usage.
(setq gc-cons-threshold (* 50 1000 1000))

;; When you set a variable using Emacs's GUI, those are automatically
;; placed in the 'init.el'. We prevent this by putting the settings in
;; a "throwaway" file called 'custom.el'.
(setq custom-file (file-name-concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; To install external packages from both ELPA and MELPA.
(require 'package)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; This directory contains all of my personal packages that can safely
;; be isolated from the rest of my configuration.
(add-to-list 'load-path (file-name-concat user-emacs-directory "lisp"))

;; These contain general configuration for Emacs's behavior.
(setq
 native-comp-async-report-warnings-errors nil
 auto-save-default nil
 make-backup-files nil
 ring-bell-function 'ignore
 inhibit-splash-screen t
 initial-scratch-message ""
 initial-major-mode 'org-mode)

;; These contain general configuration for Emacs's appearance.
(menu-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))
(setq-default
 truncate-lines t
 cursor-type 'bar)
(global-display-line-numbers-mode)

;; This section manages fonts. By default, we use JetBrains Mono, if
;; available. We also set the font size to 14. We skip this section if
;; we are in terminal Emacs.
(when (display-graphic-p)
  (if (find-font (font-spec :name "JetBrains Mono"))
      (set-face-attribute 'default nil
                          :height 140
                          :family "JetBrains Mono")
    (set-face-attribute 'default nil :height 140)))

;; I like all of the dark themes provided by Ef themes.
(use-package
 ef-themes
 :ensure t
 ;; I am liking this theme the most (currently).
 :config (ef-themes-select 'ef-symbiosis))

;; This is a personal package that contains my custom mode line.
(use-package
 mostline
 :config (setq-default mode-line-format mostline-format))

;; I like being able to run a quick, project specific script. For
;; example, I could be working on a LaTeX project. In the project's
;; root, there is a 'cmd.el', where I could have some code that opens
;; up a new buffer/window, runs a LaTeX build command, then shows the
;; build output. I can do all of this without having to change my
;; actual config. My competitive programming configuration relies on
;; this functionality.
(global-set-key
 (kbd "C-c c")
 (lambda ()
   (interactive)
   (when-let ((directory (locate-dominating-file "." "cmd.el")))
     (load-file (file-name-concat directory "cmd.el")))))

;; This package makes Eshell more usable for running complex terminal
;; applications.
(use-package
 eat
 :ensure t
 :config (add-hook 'eshell-load-hook #'eat-eshell-mode))

;; I want to be able to switch between Eshell buffers without having
;; to dig through the normal buffer menu. So I have a dedicated
;; keybinding for this.
(defun eshell-buffer-p (buffer)
  "Determines whether BUFFER is an Eshell buffer."
  (with-current-buffer buffer
    (eq major-mode 'eshell-mode)))
(defun switch-to-eshell-buffer ()
  "A command similar to `switch-to-buffer', filtering only for Eshell
buffers. If only one buffer exists, automatically switch to that buffer."
  (interactive)
  (let ((buffers (seq-filter #'eshell-buffer-p (buffer-list))))
    (cond
     ((eq (length buffers) 0)
      (message "No Eshell buffers open."))
     ((eq (length buffers) 1)
      (switch-to-buffer (car buffers)))
     (t
      (switch-to-buffer
       (completing-read
        "Switch to Eshell buffer: " (mapcar #'buffer-name buffers)
        nil t))))))
(global-set-key (kbd "C-c e") #'switch-to-eshell-buffer)

;; I have a keybinding for creating new Eshell buffers.
(global-set-key
 (kbd "C-c E")
 (lambda ()
   (interactive)
   (eshell t)))

;; Whenever we are editing text, it is sometimes useful to unfill
;; paragraphs.
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(global-set-key (kbd "M-Q") 'unfill-paragraph)

;; Some Dired customizations.
(use-package
 dired
 :custom (dired-kill-when-opening-new-dired-buffer t)
 :config (add-hook 'dired-after-readin-hook 'hl-line-mode))

;; Setup Vertico and Orderless to make minibuffer completion not ass.
(use-package
 orderless
 :ensure t
 :custom (completion-styles '(orderless basic))
 (completion-category-overrides
  '((file (styles partial-completion)))))
(use-package vertico :ensure t :config (vertico-mode 1))

;; I like a faster control backspace than what is provided by
;; default. Thankfully, Vertico comes with an extension that already
;; provide this.
(use-package
 vertico-directory
 :after vertico
 :ensure nil
 :bind
 (:map vertico-map ("C-<backspace>" . vertico-directory-delete-word)))

;; Rainbow delimiters make code look better and easier to read.
(use-package rainbow-delimiters :ensure t)

;; Magit, the best Git client. Notice the lack of evil-collection in
;; this configuration. This means that Magit uses vanilla Emacs
;; navigation. I assume this package depends on external programs such
;; as 'diff' and similar core utils (along with Git of course).
(use-package magit :ensure t :defer t)

;; Help setup IDE-style autocompletion with Corfu. The defaults are
;; extremely aggressive, which might make this configuration unusable
;; on slower computers. This package also doesn't work in terminal
;; Emacs.
(use-package
 corfu
 :ensure t
 :custom
 (corfu-auto t)
 (corfu-auto-delay 0.01)
 (corfu-auto-prefix 1))

;; YASnippet is here so that the autocompletion can work more
;; seamlessly.
(use-package yasnippet :ensure t)

;; To use Emacs 29's Treesit capabilities without having to setup
;; everything ourselves, we use this package to do the heavy lifting
;; for us.
(use-package
 treesit-auto
 :ensure t
 :custom (treesit-auto-install 'prompt)
 :config
 (treesit-auto-add-to-auto-mode-alist 'all)
 (global-treesit-auto-mode))

;; We will use Eglot to have LSP support in Emacs.
(use-package
 eglot
 :custom
 (eglot-ignored-server-capabilities
  '(:documentOnTypeFormattingProvider))
 :init (setq eglot-autoshutdown t)
 ;; This is a simple optimization to speed up Eglot.
 :config (setf (plist-get eglot-events-buffer-config :size) 0)
 ;; We turn on all of our relevant plugins alongside Eglot so that at
 ;; any point, if we want to use Eglot, we don't have to then turn on
 ;; things like Corfu.
 (add-hook
  'eglot-managed-mode-hook
  (lambda ()
    (interactive)
    (electric-pair-local-mode 1)
    (rainbow-delimiters-mode 1)
    (yas-minor-mode 1)
    (corfu-mode 1)))
 :hook ((c-ts-mode c++-ts-mode go-ts-mode java-ts-mode) . eglot-ensure)
 :bind
 (:map
  eglot-mode-map
  ("C-c r" . eglot-rename)
  ("C-c f" . eglot-format)
  ("C-c a" . eglot-code-actions)
  ("C-c D" . xref-find-definitions)
  ("C-c R" . xref-find-references)))

;; To enable the fastest experience with Eglot, we will use this
;; package, that depends on an external program called
;; 'emacs-lsp-booster'. Eglot can work without it, but it will display
;; an error message on startup.
(use-package
 eglot-booster
 :vc
 (:url "https://github.com/jdtsmith/eglot-booster.git" :rev :newest)
 :after eglot
 :config (eglot-booster-mode))

;; Since Emacs Lisp doesn't need an LSP (all functionality can be
;; imitated through the editor itself), we configure its mode
;; seperately. This includes using an external package for formatting,
;; which depends on Python 3.8+.
(use-package elisp-autofmt :ensure t)
(use-package
 elisp-mode
 :init
 (add-hook
  'emacs-lisp-mode-hook
  (lambda ()
    (electric-pair-local-mode 1)
    (yas-minor-mode 1)
    (rainbow-delimiters-mode 1)
    (corfu-mode 1)))
 :bind
 (:map
  emacs-lisp-mode-map
  ("C-c D" . xref-find-definitions)
  ("C-c R" . xref-find-references)
  ("C-c f" .
   (lambda ()
     (interactive)
     (message "Formatting...")
     (elisp-autofmt-buffer)))))

;; This is a personal package for downloading problems for competitive
;; programming. This package is due for a rewrite.
(use-package copeforces :bind (("C-c C" . copeforces)))

;; Everything from here to the end (except setting back the GC) is
;; relevant to Evil Mode. You can safely delete this part of the
;; config.

;; Here is a personal function for window management that I use a lot,
;; so much so that it overrides the Evil Mode keybinding for 'q'.
(defun kill-window-possibly-buffer ()
  "Kill the current window. If the buffer associated with the window is not
in any other window, kill it too."
  (interactive)
  (if (eq (length (window-list)) 1)
      (if (eq (length (get-buffer-window-list)) 1)
          (kill-current-buffer))
    (if (eq (length (get-buffer-window-list)) 1)
        (kill-buffer-and-window)
      (delete-window))))

;; My Evil Mode configuration.
(use-package
 evil
 :ensure t
 :init (setq evil-undo-system 'undo-redo)
 :config (evil-mode 1) (setq evil-emacs-state-cursor 'bar)
 (evil-define-key
  '(normal visual motion) 'global
  ;; The aforementioned personal window killer.
  (kbd "q") 'kill-window-possibly-buffer
  ;; So things like `org-cycle' can work in the normal state.
  (kbd "TAB") nil)
 ;; This is so that `corfu-next' and `corfu-previous' can work.
 (evil-define-key 'insert 'global (kbd "C-n") nil (kbd "C-p") nil)
 ;; Since we don't have Corfu in Eshell, and my muscle memory likes
 ;; 'C-n' and 'C-p' for cycling command history, we set that here.
 (evil-define-key
  'insert
  eshell-mode-map
  (kbd "C-p")
  'eshell-previous-matching-input-from-input
  (kbd "C-n")
  'eshell-next-matching-input-from-input))

;; This is the weirdest part of my configuration. This package enables
;; me to use Emacs keybindings without having to use the modifier
;; keys. This is described more in the documentation of
;; 'spacemaster.el'.
(use-package
 spacemaster
 :config
 (evil-define-key
  '(normal visual motion)
  'global
  (kbd "C-SPC")
  'execute-extended-command
  (kbd "SPC")
  'spacemaster
  ;; I used to type 'SPC x SPC s' to save and 'SPC x SPC f' to
  ;; navigate files, which was pretty clumsy. Before Spacemaster, I
  ;; used to manually set these to 'SPC f s' and 'SPC f f', which were
  ;; nice and satisfying. Then, I realized that I can implement this
  ;; elegantly into Spacemaster with a few tweaks.
  (kbd "C-f")
  (lambda ()
    (interactive)
    (spacemaster "C-x C-"))))

;; Dired doesn't respect the 'SPC' override for Spacemaster. So we
;; manually override it. It also doesn't respect Evil's search
;; navigation (which is really annoying because Dired was made to be
;; searched), so we set that here as well.
(use-package
 dired
 :config
 (evil-define-key
  '(normal visual motion)
  dired-mode-map
  (kbd "SPC")
  'spacemaster
  (kbd "n")
  'evil-search-next
  (kbd "p")
  'evil-search-previous))

;; 'ESC' is much easier than 'C-g', so we override here it where it's
;; used.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package
 transient
 :config
 (define-key transient-map (kbd "<escape>") 'transient-quit-one))

;; Set the GC to a more reasonable level.
(setq gc-cons-threshold (* 2 1000 1000))

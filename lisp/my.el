;;; my.el --- Personal helper functions -*- lexical-binding: t; -*-

(defmacro my-key-prefix (prefix)
  `(lambda ()
     (interactive)
     (let (message-log-max) (message "%s" ,prefix))
     (let* ((next-key (single-key-description (read-event)))
            (sequence (concat ,prefix next-key))
            (command (key-binding (kbd sequence))))
       (if (and command (not (keymapp command)))
           (call-interactively command)
         (message "%s is undefined" sequence)))))

;; TODO: Make these configurable
(defconst my--font-name-mono "IBM Plex Mono")
(defconst my--font-name-sans "IBM Plex Sans")
(defvar my--font-size 140)
(defun my-font-change-size (increment)
  (setq my--font-size (+ my--font-size increment))
  (set-face-attribute
   'default nil
   :family my--font-name-mono
   :height my--font-size)
  (set-face-attribute
   'variable-pitch nil
   :family my--font-name-sans
   :height my--font-size)
  (message "Set font size to %d" my--font-size))

(defun my-arrange-windows ()
  (interactive)
  (select-window
   (if (< (window-pixel-height) (window-pixel-width))
       (split-window-horizontally)
     (split-window-vertically))))

(defun my--language-package-name (language)
  (intern
   (if (string-suffix-p "!" language)
       (substring language 0 -1)
     (format "%s-mode" language))))

(defun my--install-language (language)
  `(use-package ,(my--language-package-name (symbol-name language))
     :ensure t
     :defer t))

(defmacro my-install-languages (&rest languages)
  `(progn ,@(mapcar #'my--install-language languages)))

(provide 'my)

;;; my.el ends here

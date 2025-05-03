;; evil state
(defface evil-normal-state-face '((t))
  "bjksldf")
(defface evil-normal-state-face-a '((t))
  "bjksldf")
(ef-themes-with-colors
 (set-face-attribute 'evil-normal-state-face nil
                     :background fg-mode-line
                     :foreground bg-mode-line))
(ef-themes-with-colors
 (set-face-attribute
  'evil-normal-state-face-a
  nil
  :background fg-dim
  :foreground bg-dim))
;; buffer name
(defun my-modeline--buffer-name ()
  (format " %s " (buffer-name)))
(defvar-local my-modeline-buffer-name

    '(:eval (propertize (my-modeline--buffer-name) 'face 'bold)))
(put 'my-modeline-buffer-name 'risky-local-variable t)
(defun my-modeline--evil-state ()
  (format " <%s> "
          (cond
           ((evil-normal-state-p)
            "N")
           ((evil-replace-state-p)
            "R")
           ((evil-motion-state-p)
            "M")
           ((evil-operator-state-p)
            "O")
           ((evil-visual-state-p)
            "V")
           ((evil-insert-state-p)
            "I")
           (t
            "E"))))
(defvar-local my-modeline-evil-state
    '(:eval
      (if (mode-line-window-selected-p)
          (propertize (my-modeline--evil-state)
                      'face
                      'evil-normal-state-face)
        (propertize (my-modeline--evil-state)
                    'face
                    'evil-normal-state-face-a))))
(put 'my-modeline-evil-state 'risky-local-variable t)

;; minor modes
(defun my-modeline--minor-mode-name ()
  "Return capitalized `minor-mode' as a string."
  (if (bound-and-true-p eglot--managed-mode)
      (format "[LSP] " (capitalize (symbol-name major-mode)))
    " "))
(defvar-local my-modeline-minor-mode
    '(:eval (my-modeline--minor-mode-name))
  "Mode line construct to display the major mode.")
(put 'my-modeline-minor-mode 'risky-local-variable t)

;; major mode
(defun my-modeline--major-mode-name ()
  "Return capitalized `major-mode' as a string."
  (format "(%s)" (capitalize (symbol-name major-mode))))
(defvar-local my-modeline-major-mode
    '(:eval (my-modeline--major-mode-name))
  "Mode line construct to display the major mode.")
(put 'my-modeline-major-mode 'risky-local-variable t)
(setq-default mode-line-format
              '("%e"
                my-modeline-evil-state
                my-modeline-buffer-name
                mode-line-format-right-align
                my-modeline-major-mode
                my-modeline-minor-mode))

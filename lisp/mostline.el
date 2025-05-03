;;; mostline.el --- The worst mode line. -*- lexical-binding: t -*-
(defface mostline--state-active-face '((t))
  "Face for state indicator when the buffer is selected.")
(defface mostline--state-disabled-face '((t))
  "Face for state indicator when the buffer is NOT selected.")
(ef-themes-with-colors
 (set-face-attribute
  'mostline--state-active-face
  nil
  :background fg-mode-line
  :foreground bg-mode-line)
 (set-face-attribute
  'mostline--state-disabled-face
  nil
  :background fg-dim
  :foreground bg-dim))
(defun mostline--state ()
  (format " <%s> "
          (cond
           ((not (bound-and-true-p evil-mode))
            "E")
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
(defvar-local mostline-state
    '(:eval
      (propertize (mostline--state)
                  'face
                  (if (mode-line-window-selected-p)
                      'mostline--state-active-face
                    'mostline--state-disabled-face))))
(put 'mostline-state 'risky-local-variable t)

(defun mostline--buffer ()
  (format " %s " (buffer-name)))
(defvar-local mostline-buffer
    '(:eval (propertize (mostline--buffer) 'face 'bold)))
(put 'mostline-buffer 'risky-local-variable t)

(defun mostline--saved ()
  (if (and buffer-file-name (buffer-modified-p))
      "*"
    ""))

(defvar-local mostline-saved
    '(:eval (propertize (mostline--saved) 'face 'error))
  "What is going on")
(put 'mostline-saved 'risky-local-variable t)

(defun mostline--major-mode ()
  "Return capitalized `major-mode' as a string."
  (format "(%s)" (capitalize (symbol-name major-mode))))
(defvar-local mostline-major-mode '(:eval (mostline--major-mode))
  "Mode line construct to display the major mode.")
(put 'mostline-major-mode 'risky-local-variable t)

(defun mostline--lsp ()
  "Return capitalized `minor-mode' as a string."
  (if (bound-and-true-p eglot--managed-mode)
      (format "[LSP] " (capitalize (symbol-name major-mode)))
    " "))
(defvar-local mostline-lsp '(:eval (mostline--lsp))
  "Mode line construct to display the major mode.")
(put 'mostline-lsp 'risky-local-variable t)

(defvar mostline-format
  '("%e"
    mostline-state
    mostline-buffer
    mostline-saved
    mode-line-format-right-align
    mostline-major-mode
    mostline-lsp))

(provide 'mostline)
;;; mostline.el ends here

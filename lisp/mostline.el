;;; mostline.el --- The worst mode line. -*- lexical-binding: t -*-
;; State indicator (for evil)
(defun mostline--state ()
  "Computes the text for the state indicator."
  (cond
   ((not (bound-and-true-p evil-mode))
    "")
   ((evil-normal-state-p)
    " <N> ")
   ((evil-replace-state-p)
    " <R> ")
   ((evil-motion-state-p)
    " <M> ")
   ((evil-operator-state-p)
    " <O> ")
   ((evil-visual-state-p)
    " <V> ")
   ((evil-insert-state-p)
    " <I> ")
   (t
    " <E> ")))
(defvar-local mostline-state
    '(:eval
      (propertize (mostline--state)
                  'face
                  (if (mode-line-window-selected-p)
                      `(:background
                        ,(face-attribute 'mode-line :foreground)
                        :foreground
                        ,(face-attribute 'mode-line :background))
                    `(:background
                      ,(face-attribute
                        'mode-line-inactive
                        :foreground)
                      :foreground
                      ,(face-attribute
                        'mode-line-inactive
                        :background)))))
  "Computes the text+style for the state indicator.")
(put 'mostline-state 'risky-local-variable t)

(defun mostline--buffer ()
  "Computes the text for the buffer name."
  (format " %s " (buffer-name)))
(defvar-local mostline-buffer
    '(:eval (propertize (mostline--buffer) 'face 'bold))
  "Computes the text+style for the buffer name.")
(put 'mostline-buffer 'risky-local-variable t)

(defun mostline--saved ()
  "Computes the text for whether the current buffer is saved."
  (if (and buffer-file-name (buffer-modified-p))
      "*"
    ""))

(defvar-local mostline-saved
    '(:eval (propertize (mostline--saved) 'face 'error))
  "Computes the text+style for whether the current buffer is saved.")
(put 'mostline-saved 'risky-local-variable t)

(defun mostline--major-mode ()
  "Computes the text for whether the current buffer's major mode."
  (format "(%s)" (capitalize (symbol-name major-mode))))
(defvar-local mostline-major-mode '(:eval (mostline--major-mode))
  "Computes the text+style for whether the current buffer's major mode.")
(put 'mostline-major-mode 'risky-local-variable t)

(defun mostline--lsp ()
  "Computes the text for whether the current buffer is using eglot."
  (if (bound-and-true-p eglot--managed-mode)
      (format "[LSP] " (capitalize (symbol-name major-mode)))
    " "))
(defvar-local mostline-lsp '(:eval (mostline--lsp))
  "Computes the text+style for whether the current buffer is using eglot.")
(put 'mostline-lsp 'risky-local-variable t)

(defvar mostline-format
  '("%e"
    mostline-state
    mostline-buffer
    mostline-saved
    mode-line-format-right-align
    mostline-major-mode
    mostline-lsp)
  "The main entrypoint for mostline.
 Set the `mode-line-format' variable using `setq-default'.")

(provide 'mostline)
;;; mostline.el ends here

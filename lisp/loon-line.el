;;; loon-line.el --- A personal mode-line -*- lexical-binding: t; -*-

;; To use loon-line:
;; (require 'loon-line)
;; (setq-default mode-line-format loon-line)

(defun loon-line--state-bg ()
  (let ((face (if (mode-line-window-selected-p)
                  'font-lock-keyword-face
                'mode-line-inactive)))
    (face-attribute face :foreground)))

(defun loon-line--state-fg ()
  (let ((face (if (mode-line-window-selected-p)
                  'default
                'mode-line-inactive)))
    (face-attribute face :background)))

(defun loon-line--state-symbol ()
  (cond
   ((not (bound-and-true-p evil-mode)) "")
   ((evil-normal-state-p) " <N> ")
   ((evil-replace-state-p) " <R> ")
   ((evil-motion-state-p) " <M> ")
   ((evil-operator-state-p) " <O> ")
   ((evil-visual-state-p) " <V> ")
   ((evil-visual-state-p) " <V> ")
   ((evil-insert-state-p) " <I> ")
   (t " <E> ")))

(defun loon-line--state-indicator ()
  (propertize
   (loon-line--state-symbol)
   'face
   (list ':background (loon-line--state-bg)
         ':foreground (loon-line--state-fg)
         ':box `(:line-width 1 :color ,(loon-line--state-bg)))))

(defun loon-line--dirty-buffer-indicator ()
  (propertize
   (if (and buffer-file-name (buffer-modified-p)) " *" "")
   'face
   'error))

(defun loon-line--zoom-value ()
  (if (and
       (display-graphic-p)
       (not (zerop text-scale-mode-amount)))
      (concat
       (if (> text-scale-mode-amount 0) " +" " ")
       (number-to-string text-scale-mode-amount))
    ""))

(defun loon-line--zoom-indicator ()
  (propertize (loon-line--zoom-value) 'face 'success))

(defun loon-line--mode-string-unformatted ()
  (substring (symbol-name major-mode) 0 -5))

(defun loon-line--mode-string-formatted ()
  (capitalize
   (string-replace "-" " " (loon-line--mode-string-unformatted))))

(defun loon-line--mode-indicator ()
  (format "(%s)" (loon-line--mode-string-formatted)))

(defconst loon-line
  '("%e"
    (:eval (loon-line--state-indicator))
    (:eval (propertize (format " %s" (buffer-name)) 'face 'bold))
    (:eval (loon-line--dirty-buffer-indicator))
    (:eval (loon-line--zoom-indicator))
    mode-line-format-right-align
    (:eval (loon-line--mode-indicator))
    (:eval (if (bound-and-true-p eglot--managed-mode) "[LSP] " " "))))

(provide 'loon-line)

;;; loon-line.el ends here

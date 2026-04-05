;;; prot-ptyxis.el --- Protesilaos's themes for Ptyxis terminal emulator -*- lexical-binding: t; -*-

;; Usage:
;; M-x prot-ptyxis-install-themes

(require 'modus-themes)
(require 'standard-themes)
(require 'ef-themes)

(defun prot-ptyxis--format-name (name)
  (capitalize (string-replace "-" " " name)))

(defun prot-ptyxis--convert-named-color (color)
  (substring (color-rgb-to-hex (color-name-to-rgb color)) 0 -6))

(defun prot-ptyxis--get-color (theme key)
  (upcase
   (let ((color (modus-themes-get-color-value key nil theme)))
     (if (not (string-prefix-p "\#" color))
         (prot-ptyxis--convert-named-color color)
       color))))

(defun prot-ptyxis--get-colors (theme)
  `(("Background" ,(prot-ptyxis--get-color theme 'bg-main))
    ("Foreground" ,(prot-ptyxis--get-color theme 'fg-main))
    ("Cursor" ,(prot-ptyxis--get-color theme 'cursor))
    ("Color0" ,(prot-ptyxis--get-color theme 'bg-term-black))
    ("Color1" ,(prot-ptyxis--get-color theme 'bg-term-red))
    ("Color2" ,(prot-ptyxis--get-color theme 'bg-term-green))
    ("Color3" ,(prot-ptyxis--get-color theme 'bg-term-yellow))
    ("Color4" ,(prot-ptyxis--get-color theme 'bg-term-blue))
    ("Color5" ,(prot-ptyxis--get-color theme 'bg-term-magenta))
    ("Color6" ,(prot-ptyxis--get-color theme 'bg-term-cyan))
    ("Color7" ,(prot-ptyxis--get-color theme 'bg-term-white))
    ("Color8" ,(prot-ptyxis--get-color theme 'bg-term-black-bright))
    ("Color9" ,(prot-ptyxis--get-color theme 'bg-term-red-bright))
    ("Color10" ,(prot-ptyxis--get-color theme 'bg-term-green-bright))
    ("Color11" ,(prot-ptyxis--get-color theme 'bg-term-yellow-bright))
    ("Color12" ,(prot-ptyxis--get-color theme 'bg-term-blue-bright))
    ("Color13" ,(prot-ptyxis--get-color theme 'bg-term-magenta-bright))
    ("Color14" ,(prot-ptyxis--get-color theme 'bg-term-cyan-bright))
    ("Color15" ,(prot-ptyxis--get-color theme 'bg-term-white-bright))))

(defun prot-ptyxis--make-group (theme)
  (let* ((theme-string (string-replace "-light" "" (symbol-name theme)))
         (theme-dark (intern (string-replace "light" "dark" (symbol-name theme)))))
    (list theme-string theme theme-dark)))

(defun prot-ptyxis--make-group-modus (theme)
  (let* ((theme-string (string-replace "-operandi" "" (symbol-name theme)))
         (theme-dark (intern (string-replace "operandi" "vivendi" (symbol-name theme)))))
    (list theme-string theme theme-dark)))

(defun prot-ptyxis--make-groups ()
  (let ((themes (append modus-themes-collection
                        standard-themes-collection
                        ef-themes-collection))
        (groups nil))
    (dolist (theme themes groups)
      (cond
       ((string-search "light" (symbol-name theme))
        (setq groups (cons (prot-ptyxis--make-group theme) groups)))
       ((string-search "operandi" (symbol-name theme))
        (setq groups (cons (prot-ptyxis--make-group-modus theme) groups)))
       ((not (or (string-search "dark" (symbol-name theme))
                 (string-search "vivendi" (symbol-name theme))))
        (setq groups (cons theme groups)))))))

(defun prot-ptyxis--install-theme-dynamic (directory group)
  (with-temp-file (file-name-concat directory (concat (cl-first group) ".palette"))
    (insert "[Palette]\n")
    (insert "Name=" (prot-ptyxis--format-name (cl-first group)) "\n")
    (insert "\n[Light]\n")
    (dolist (field (prot-ptyxis--get-colors (cl-second group)))
      (insert (cl-first field) "=" (cl-second field) "\n"))
    (insert "\n[Dark]\n")
    (dolist (field (prot-ptyxis--get-colors (cl-third group)))
      (insert (cl-first field) "=" (cl-second field) "\n"))))

(defun prot-ptyxis--install-theme (directory theme)
  (with-temp-file (file-name-concat directory (concat (symbol-name theme) ".palette"))
    (insert "[Palette]\n")
    (insert "Name=" (prot-ptyxis--format-name (symbol-name theme)) "\n")
    (dolist (field (prot-ptyxis--get-colors theme))
      (insert (cl-first field) "=" (cl-second field) "\n"))))

(defun prot-ptyxis-install-themes ()
  (interactive)
  (let ((directory (read-directory-name "Ptyxis theme directory: ")))
    (mapc (lambda (group)
            (if (listp group)
                (prot-ptyxis--install-theme-dynamic directory group)
              (prot-ptyxis--install-theme directory group)))
          (prot-ptyxis--make-groups))))

(provide 'prot-ptyxis)

;;; prot-ptyxis.el ends here

;; Generate themes for Ptyxis terminal from Ef themes

(require 'modus-themes)
(require 'ef-themes)
(defun insert-color (prefix symbol theme)
  (insert (format "%s=%s\n"
		  prefix
		  (upcase (let ((intermediate (modus-themes-get-color-value symbol nil theme)))
			    (if (string-prefix-p "\#" intermediate)
				intermediate
			      (substring (apply #'color-rgb-to-hex (color-name-to-rgb intermediate)) 0 -6)))))))
(dolist (theme (append ef-themes-collection modus-themes-collection))
  (with-temp-file (format "%s.palette" (symbol-name theme))
    (insert (format "[Palette]\n"))
    (insert (format "Name=%s\n" (format "%s" (capitalize
					      (string-replace
					       "-" " "
					       (substring
						(symbol-name theme)))))))
    (insert-color "Background" 'bg-main theme)
    (insert-color "Foreground" 'fg-main theme)
    (insert-color "Cursor" 'cursor theme)
    (insert-color "Color0" 'bg-term-black theme)
    (insert-color "Color1" 'bg-term-red theme)
    (insert-color "Color2" 'bg-term-green theme)
    (insert-color "Color3" 'bg-term-yellow theme)
    (insert-color "Color4" 'bg-term-blue theme)
    (insert-color "Color5" 'bg-term-magenta theme)
    (insert-color "Color6" 'bg-term-cyan theme)
    (insert-color "Color7" 'bg-term-white theme)
    (insert-color "Color8" 'bg-term-black-bright theme)
    (insert-color "Color9" 'bg-term-red-bright theme)
    (insert-color "Color10" 'bg-term-green-bright theme)
    (insert-color "Color11" 'bg-term-yellow-bright theme)
    (insert-color "Color12" 'bg-term-blue-bright theme)
    (insert-color "Color13" 'bg-term-magenta-bright theme)
    (insert-color "Color14" 'bg-term-cyan-bright theme)
    (insert-color "Color15" 'bg-term-white-bright theme)))

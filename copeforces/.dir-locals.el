((c++-ts-mode
  ;; Disable new imports
  (eglot-server-programs
   . ((c++-ts-mode . ("clangd" "--header-insertion=never"))))

  ;; Set tab width for competitive programming only
  (indent-tabs-mode . nil)
  (c-ts-mode-indent-style . "k&r")
  (c-ts-mode-indent-offset . 4)))

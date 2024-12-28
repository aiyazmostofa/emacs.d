((c++-mode
  ;; Disable new imports
  (eglot-server-programs
   . ((c++-mode . ("clangd" "--header-insertion=never"))))

  ;; Set tab width for competitive programming only
  (indent-tabs-mode . nil)
  (tab-width . 4)
  (c-set-style . "k&r")
  (c-basic-offset . 4)))

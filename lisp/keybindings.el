;;; keybindings.el --- Keybinding setup
;;; Commentary:
;;; Code:

(general-create-definer leader :prefix "SPC")

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun kill-window-possibly-buffer ()
  (interactive)
  (if (eq (length (get-buffer-window-list)) 1)
      (kill-buffer-and-window)
    (delete-window)))

(leader
 :states 'normal
 :keymaps 'override

 "SPC" 'execute-extended-command

 "ek" 'delete-frame
 "eq" 'kill-emacs

 "fs" 'save-buffer
 "ff" 'find-file
 "fw" 'write-file
 "fd" 'delete-file

 "bs" 'switch-to-buffer
 "bk" 'kill-current-buffer
 "be" 'eval-buffer

 "wh" 'split-and-follow-horizontally
 "wv" 'split-and-follow-vertically
 "wo" 'other-window
 "wd" 'kill-window-possibly-buffer

 "ca" 'eglot-code-actions
 "cc" 'eglot
 "cd" 'eglot-find-implementation
 "cf" 'eglot-format
 "cr" 'eglot-rename

 "jd" 'cp-download-problem
 "jr" 'cp-kill-results
 "jk" 'cp-test-problem)

(provide 'keybindings)
;;; keybindings.el ends here

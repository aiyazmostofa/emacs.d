;;; spacemaster.el --- Use space instead of control because you're lazy. -*- lexical-binding: t; -*-

(defvar spacemaster--buffer ""
  "A string representing the command being built by spacemaster.")

(defvar spacemaster--char nil
  "The current character being considered by spacemaster.")

(defvar spacemaster--candidate-function nil
  "The current function being considered by spacemaster.")

(defun spacemaster--echo (string &rest args)
  "A utility function, copied directly from evil-mode, to output to the echo area without logging to *Messages*."
  (let (message-log-max)
    (apply #'message string args)))

(defun spacemaster--handle-space ()
  "Handle space key being pressed in spacemaster."
  (cond
   ((string-match-p "C-\\'\\|M-\\'" spacemaster--buffer)
    (setq spacemaster--buffer (concat spacemaster--buffer "SPC")))
   (t
    (setq spacemaster--buffer (concat spacemaster--buffer " C-")))))

(defun spacemaster--handle-tab ()
  "Handle tab key being pressed in spacemaster."
  (cond
   ((string-match-p "C-\\'" spacemaster--buffer)
    (setq spacemaster--buffer
          (concat (substring spacemaster--buffer nil -2) "M-")))
   ((string-match-p "C-M-\\'" spacemaster--buffer)
    (setq spacemaster--buffer
          (concat (substring spacemaster--buffer nil -4) "C-")))
   ((string-match-p "M-\\'" spacemaster--buffer)
    (setq spacemaster--buffer
          (concat (substring spacemaster--buffer nil -2) "C-M-")))
   (t
    (setq spacemaster--buffer ""))))

(defun spacemaster--handle-misc ()
  "Handle any character not specially handled by spacemaster."
  (unless (string-match-p "C-\\'\\|M-\\'" spacemaster--buffer)
    (setq spacemaster--buffer (concat spacemaster--buffer " ")))
  (setq spacemaster--buffer
        (concat spacemaster--buffer (list spacemaster--char))))

(defun spacemaster--process-command ()
  "See if a command can be executed from our current keybinding buffer.
If it cannot, handle accordingly."
  (unless (string-match-p "C-\\'\\|M-\\'" spacemaster--buffer)
    (setq spacemaster--candidate-function
          (key-binding (kbd spacemaster--buffer)))
    (cond
     ((not spacemaster--candidate-function)
      (spacemaster--echo "Invalid key sequence")
      (setq spacemaster--buffer ""))
     ((not (keymapp spacemaster--candidate-function))
      (call-interactively spacemaster--candidate-function)
      (setq spacemaster--buffer "")))))

(defun spacemaster ()
  "The master command that temporarily takes control of the keyboard.

Here are some example keybinding conversions:
'SPC a' -> 'C-a'
'SPC a SPC b' -> 'C-a C-b'
'SPC a b' -> 'C-a b'
'SPC a SPC TAB b' -> 'C-a M-b'
'SPC TAB a' -> 'M-a'
'SPC TAB a b' -> 'M-a b'
'SPC TAB a TAB b' -> 'M-a C-b'
"
  (interactive)
  (setq spacemaster--buffer "C-")
  (spacemaster--echo "C-")
  (while (not (string-empty-p spacemaster--buffer))
    (setq spacemaster--char (read-char))
    (cond
     ((eq spacemaster--char 32)
      (spacemaster--handle-space))
     ((eq spacemaster--char 27)
      (setq spacemaster--buffer ""))
     ((eq spacemaster--char 9)
      (spacemaster--handle-tab))
     (t
      (spacemaster--handle-misc)))
    (spacemaster--echo "%s" spacemaster--buffer)
    (unless (string-empty-p spacemaster--buffer)
      (spacemaster--process-command))))

(provide 'spacemaster)
;;; spacemaster.el ends here

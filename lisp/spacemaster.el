;;; spacemaster.el --- Use space instead of control because you're lazy. -*- lexical-binding: t; -*-

;;; Commentary: This package is used to avoid modifier keys in Emacs
;;; as much as possible. By mapping 'SPC' to the `spacemaster' command
;;; (usually in Evil states like 'normal' and 'motion'), you can build
;;; up keybindings without the use of modifiers. This is described in
;;; further detail in the documentation of `spacemaster'. This package
;;; does not directly integrate into any native Emacs key loop, so
;;; some features are broken. Things like 'which-key',
;;; 'describe-key'. Universal arguments work, as long as you use
;;; 'C-u'. If you don't (like 'C-12'), the argument is corrupted.

(defvar spacemaster--combo ""
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
   ((string-match-p "C-\\'\\|M-\\'" spacemaster--combo)
    (setq spacemaster--combo (concat spacemaster--combo "SPC")))
   (t
    (setq spacemaster--combo (concat spacemaster--combo " C-")))))

(defun spacemaster--handle-tab ()
  "Handle tab key being pressed in spacemaster."
  (cond
   ((string-match-p "C-\\'" spacemaster--combo)
    (setq spacemaster--combo
          (concat (substring spacemaster--combo nil -2) "M-")))
   ((string-match-p "C-M-\\'" spacemaster--combo)
    (setq spacemaster--combo
          (concat (substring spacemaster--combo nil -4) "C-")))
   ((string-match-p "M-\\'" spacemaster--combo)
    (setq spacemaster--combo
          (concat (substring spacemaster--combo nil -2) "C-M-")))
   (t
    (setq spacemaster--combo (concat spacemaster--combo " TAB")))))

(defun spacemaster--handle-misc ()
  "Handle any character not specially handled by spacemaster."
  (unless (string-match-p "C-\\'\\|M-\\'" spacemaster--combo)
    (setq spacemaster--combo (concat spacemaster--combo " ")))
  (setq spacemaster--combo
        (concat spacemaster--combo (list spacemaster--char))))

(defun spacemaster--process-command ()
  "See if a command can be executed from our current keybinding buffer.
If it cannot, handle accordingly."
  (unless (string-match-p "C-\\'\\|M-\\'" spacemaster--combo)
    (setq spacemaster--candidate-function
          (key-binding (kbd spacemaster--combo)))
    (cond
     ((not spacemaster--candidate-function)
      (spacemaster--echo (concat spacemaster--combo " is undefined"))
      (setq spacemaster--combo ""))
     ((not (keymapp spacemaster--candidate-function))
      (call-interactively spacemaster--candidate-function)
      (setq spacemaster--combo "")))))

(defun spacemaster (&optional prefix)
  "The master command that temporarily takes control of the keyboard.

Here are some example keybinding conversions:
'SPC a' -> 'C-a'
'SPC a SPC b' -> 'C-a C-b'
'SPC a b' -> 'C-a b'
'SPC a SPC TAB b' -> 'C-a M-b'
'SPC TAB a' -> 'M-a'
'SPC TAB a b' -> 'M-a b'
'SPC TAB a TAB b' -> 'M-a C-b'
'SPC TAB TAB a' -> 'C-M-a'
'SPC x ESC' -> Nothing happens (escape quits).

Providing an argument for PREFIX allows you to start your keybinding from a place other than 'C-'.
"
  (interactive)
  (or prefix (setq prefix "C-"))
  (setq spacemaster--combo prefix)
  (spacemaster--echo prefix)
  (while (not (string-empty-p spacemaster--combo))
    (setq spacemaster--char (read-char))
    (cond
     ((eq spacemaster--char 32)
      (spacemaster--handle-space))
     ((eq spacemaster--char 27)
      (setq spacemaster--combo ""))
     ((eq spacemaster--char 9)
      (spacemaster--handle-tab))
     (t
      (spacemaster--handle-misc)))
    (spacemaster--echo "%s" spacemaster--combo)
    (unless (string-empty-p spacemaster--combo)
      (spacemaster--process-command))))

(provide 'spacemaster)
;;; spacemaster.el ends here

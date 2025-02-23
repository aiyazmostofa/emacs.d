;;; spacemaster.el --- Use space instead of control because you're lazy. -*- lexical-binding: t; -*-
(defun spacemaster--echo (string &rest args)
  (let (message-log-max)
    (apply #'message string args)))

(defun spacemaster--handle-space ()
  (cond
   ((string-equal spacemaster--buffer "C-")
    (call-interactively 'execute-extended-command)
    (setq spacemaster--buffer ""))
   ((string-match-p "C-\\'\\|M-\\'" spacemaster--buffer)
    (setq spacemaster--buffer (concat spacemaster--buffer "SPC")))
   (t
    (setq spacemaster--buffer (concat spacemaster--buffer " C-")))))

(defun spacemaster--handle-escape ()
  (if (string-match-p "C-\\'" spacemaster--buffer)
      (setq spacemaster--buffer
            (concat (substring spacemaster--buffer nil -2) "M-"))
    (setq spacemaster--buffer "")))

(defun spacemaster--process-command ()
  (setq spacemaster--candidate-function
        (key-binding (kbd spacemaster--buffer)))
  (cond
   ((string-match-p "C-\\'\\|M-\\'" spacemaster--buffer))
   ((not spacemaster--candidate-function)
    (spacemaster--echo "Invalid key sequence")
    (setq spacemaster--buffer ""))
   ((not (keymapp spacemaster--candidate-function))
    (call-interactively spacemaster--candidate-function)
    (setq spacemaster--buffer ""))))

(defun spacemaster ()
  (interactive)
  (setq spacemaster--buffer "C-")
  (spacemaster--echo "C-")
  (while (not (string-empty-p spacemaster--buffer))
    (setq spacemaster--char (read-char))
    (cond
     ((eq spacemaster--char 32)
      (spacemaster--handle-space))
     ((eq spacemaster--char 27)
      (spacemaster--handle-escape))
     (t
      (unless (string-match-p "C-\\'\\|M-\\'" spacemaster--buffer)
        (setq spacemaster--buffer (concat spacemaster--buffer " ")))
      (setq spacemaster--buffer
            (concat spacemaster--buffer (list spacemaster--char)))))
    (spacemaster--echo "%s" spacemaster--buffer)
    (unless (string-empty-p spacemaster--buffer)
      (spacemaster--process-command))))

(provide 'spacemaster)
;;; spacemaster.el ends here

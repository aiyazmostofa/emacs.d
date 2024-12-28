;; Delete any stray buffers
(when (get-buffer "cf-output")
  (kill-buffer "cf-output"))

;; Create a new buffer
(let ((buffer (generate-new-buffer "*copeforces*")))
  ;; Display the new buffer in a new window
  (display-buffer buffer)
  (other-window 1)

  (with-current-buffer buffer
    ;; If this buffer is killed, delete the main executable
    (make-local-variable 'kill-buffer-hook)
    (add-hook
     'kill-buffer-hook
     (lambda ()
       (when (file-exists-p "main")
         (delete-file "main"))))

    ;; Insert big header
    (insert "* ")
    (insert
     (format "[[%s][%s]]"
             (file-name-concat default-directory "main.cpp")
             (file-name-nondirectory
              (directory-file-name default-directory))))
    (insert "\n")

    ;; Compile the program
    (setq-local compiler-output
                (shell-command-to-string
                 "g++ -g -Wall main.cpp -o main"))

    ;; Display compiler output if needed
    (unless (string-empty-p compiler-output)
      (insert "** Compiler Output\n")
      (insert "#+BEGIN_SRC\n")
      (insert compiler-output)
      (insert "#+END_SRC\n"))

    ;; Only run if executable exists
    (if (file-exists-p "main")
        ;; Go through each .txt file and run program with file as input
        (dolist (file (directory-files "." t "^[0-9]+$"))
          ;; Create the header
          (insert "** " (file-name-nondirectory file) "\n")
          (insert "#+BEGIN_SRC\n")

          ;; Create a new process
          (let
              ((proc
                (make-process
                 :name "cf-process"
                 :buffer (current-buffer) ; All the output is fed into this buffer
                 :command (list (expand-file-name "main")) ; Expand full file name so it is executable
                 :sentinel #'ignore ; Don't add any status messages to end of runtime
                 )))
            ;; Send the input
            (process-send-string
             proc
             (concat
              ;; Hacky code to get file content as string
              (with-temp-buffer
                (insert-file-contents file)
                (insert "\n")
                (buffer-string))
              ;; Insert a new line so that the input is processed by the program
              "\n"))

            ;; Set a timeout of 1 second
            (with-timeout (1 (kill-process proc)
                             ;; Add a extra new line if needed
                             (unless (string-equal
                                      (buffer-substring
                                       (- (point-max) 1) (point-max))
                                      "\n")
                               (insert "\n"))
                             (insert "time limit exceeded\n"))
              ;; "Game" loop
              (while (process-live-p proc)
                (sit-for .05))))

          ;; Create the footer
          (insert "#+END_SRC\n")))

    ;; Make output org mode and read only
    (org-mode)
    (view-mode)

    ;; Set the keybinding to delete buffer/window
    (evil-local-set-key
     'normal (kbd "q")
     (lambda ()
       (interactive)
       (kill-buffer (current-buffer))
       (delete-window)))

    ;; Set the keybinding that starts GDB
    (evil-local-set-key
     'normal (kbd "d")
     (lambda ()
       (interactive)
       (gdb "gdb -i=mi main")
       (gdb-many-windows)))

    ;; Jump to the top of the final output buffer
    (beginning-of-buffer)))

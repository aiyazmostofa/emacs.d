;;; copeforces.el --- Spend more time configuring your competitive programming setup than actually practicing. -*- lexical-binding: t -*-
(defun copeforces--strip-http-header (req)
  (replace-regexp-in-string (rx (*? anychar) "\r\n\r\n") "" req))
(defun copeforces--server-filter (process request)
  (let* ((body
          (json-parse-string (copeforces--strip-http-header request)))
         ;; Name removes all non-alphanumeric characters
         (name
          (replace-regexp-in-string
           "[^A-Za-z0-9]+" "" (gethash "name" body))))
    ;; Copy the template directory over
    (copy-directory (file-name-concat user-emacs-directory
                                      "copeforces")
                    (file-name-concat copeforces--directory name)
                    ;; Create any new directories along the way
                    nil t)
    ;; Loop through each test case
    (setq-local count 0)
    (while (< count (length (gethash "tests" body)))
      (let ((test (elt (gethash "tests" body) count)))
        ;; Create input file and insert data
        (with-temp-file (file-name-concat copeforces--directory
                                          name
                                          (number-to-string count))
          (insert (gethash "input" test))))
      (setq-local count (+ count 1)))
    ;; Insert name notifying of success (with links to cpp file)
    (insert "** ")
    (insert
     (format "[[%s][%s]]"
             (file-name-concat copeforces--directory name "main.cpp")
             name))
    (insert "\n")))
(defun copeforces ()
  (interactive)
  ;; Have to set global variable to reach inside lambda
  (setq copeforces--directory
        (read-directory-name "Problem directory: "))
  ;; Create a new buffer
  (let ((buffer (generate-new-buffer "*copeforces*")))
    ;; Display the new buffer in a new window
    (display-buffer buffer)
    (other-window 1)
    (with-current-buffer buffer
      ;; Make output org-mode
      (org-mode)
      ;; Create the header
      (insert (format "* [[%s]]\n" copeforces--directory))
      ;; Start (and store) the server
      (let ((server
             (make-network-process
              :name "copeforces"
              :server t
              :local [127 0 0 1 10043]
              :service 10043
              :filter #'copeforces--server-filter)))
        ;; Kill server when buffer killed
        (make-local-variable 'kill-buffer-hook)
        (add-hook
         'kill-buffer-hook
         (lambda ()
           (when (process-live-p server)
             (delete-process server))))
        ;; Set the keybinding to delete buffer/window
        (evil-local-set-key
         'normal (kbd "q")
         (lambda ()
           (interactive)
           (kill-buffer (current-buffer))
           (delete-window)))))))
(provide 'copeforces)
;;; copeforces.el ends here

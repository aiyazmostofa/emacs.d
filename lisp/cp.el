;;; cp.el --- CP Setup
;;; Commentary:
;;; Code:

(defun cp-parse-json (request)
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string)
        (json-string
         (replace-regexp-in-string
          "\\\\\\(.\\|\n\\)" "\\1"
          (substring
           (format "%S" request)
           (string-match (regexp-quote "{\\") (format "%S" request))
           -2))))
    (setq json-map (json-read-from-string json-string))))

(defun cp-post-handler (request)
  (cp-parse-json request)
  (let ((i 0))
    (while (< i (length (gethash "tests" json-map)))
      (with-temp-file (format "%d.in" i)
        (insert (gethash "input" (nth i (gethash "tests" json-map)))))
      (setq i (1+ i))))
  (copy-file (concat user-emacs-directory "main.cpp") "main.cpp")
  (copy-file (concat user-emacs-directory "run.sh") "run.sh")
  (shell-command "chmod +x run.sh")
  (find-file "main.cpp")
  (message (gethash "testType" json-map))
  (with-slots
   (process headers) request
   (ws-response-header
    process 200 '("Content-type" . "text/plain"))
   (ws-stop-all)))

(defun cp-download-problem (problem-name)
  (ws-stop-all)
  (interactive "sEnter Problem Name: ")
  (message "Waiting for Companion...")
  (while (file-exists-p "main.cpp")
    (cd ".."))
  (make-directory problem-name)
  (cd problem-name)
  (ws-start '(((:POST . ".*") . cp-post-handler)) 10043))

(defun cp-test-problem ()
  (interactive)
  (let ((buffer (generate-new-buffer "results")))
    (with-current-buffer buffer
      (insert (shell-command-to-string "./run.sh"))
      (org-mode))
    (display-buffer buffer)
    (other-window 1)
    (beginning-of-buffer)))

(provide 'cp)
;;; cp.el ends here

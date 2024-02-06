;;; cp.el --- CP Enjoyer Setup
;;; Commentary:
;;; Code:

(defun cp-download-problem (problem-name)
  (interactive "sEnter Problem Name: ")
  (message "Waiting for Companion")
  (while (file-exists-p "Solution.java")
        (cd ".."))
  (setq template-file (with-temp-buffer
                        (insert-file-contents (concat user-emacs-directory "Solution.java"))
                        (buffer-string)))
  (make-directory problem-name)
  (cd problem-name)
  (ws-start
   '(((:POST . ".*") .
      (lambda (request)
        (let ((json-object-type 'hash-table)
              (json-array-type 'list)
              (json-key-type 'string)
              (json-string
               (replace-regexp-in-string "\\\\\\(.\\|\n\\)" "\\1"
                                         (substring
                                          (format "%S" request)
                                          (string-match
                                           (regexp-quote "{\\")
                                           (format "%S" request))
                                          -2))))
          (setq json-map (json-read-from-string json-string)))
        (let ((i 0))
          (while (< i (length (gethash "tests" json-map)))
            (with-temp-file (format "%d.in" i)
              (insert (gethash "input" (nth i (gethash "tests" json-map)))))
            (setq i (1+ i))))

        (with-temp-file "Solution.java"
          (when (string= (gethash "type" (gethash "input" json-map)) "file")
            (setq input-type (gethash "fileName" (gethash "input" json-map))))
          (when (string= (gethash "type" (gethash "output" json-map)) "file")
            (setq output-type (gethash "fileName" (gethash "output" json-map))))
          (when (string= (gethash "type" (gethash "input" json-map)) "stdin")
            (setq input-type ""))
          (when (string= (gethash "type" (gethash "output" json-map)) "stdout")
            (setq output-type ""))
          (insert (format template-file (gethash "url" json-map) input-type output-type)))
        (find-file "Solution.java")
        (message "Success")
        (with-slots (process headers) request
          (ws-response-header process 200 '("Content-type" . "text/plain"))
          (ws-stop-all)
          )))) 10043))

(defun cp-project-override (dir)
  (let ((override (locate-dominating-file dir "Solution.java")))
    (if override
        (cons 'vc override)
      nil)))

(defun cp-test-problem ()
  (interactive)
  (setq test-cases (directory-files default-directory nil "\\.in$"))
  (let ((buffer (generate-new-buffer "results")))
    (with-current-buffer buffer
      (org-mode)
      (while test-cases
        (setq output (shell-command-to-string (concat "timeout 5 java Solution.java < " (car test-cases))))
        (insert (concat "* " (car test-cases) "\n"))
        (insert "#+BEGIN_SRC\n")
        (insert output)
        (insert "#+END_SRC\n")
        (setq test-cases (cdr test-cases))))
    (display-buffer buffer)
    (other-window 1)
    (beginning-of-buffer)))

(defun cp-kill-results ()
  (interactive)
  (kill-matching-buffers "results*" nil t)
  (message "Results have been cleared."))

(provide 'cp)
;;; cp.el ends here

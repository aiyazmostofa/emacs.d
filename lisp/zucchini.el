;;; zucchini.el --- Per project keybindings and task runner -*- lexical-binding: t; -*-

;; TODO: Add documentation

(defvar zucchini--tasks nil)
(defvar zucchini--project-directory nil)
(defvar zucchini--local-directory nil)
(defcustom zucchini-trusted-scripts nil
  "Trusted Zucchini scripts"
  :group 'zucchini
  :type '(repeat (file)))

(defun zucchini--parse-task (args)
  (let ((key (car args))
        (arg nil)
        (from (cdr args))
        (to nil))
    (when (not (vectorp key))
      (error "Zucchini register has incorrect form"))
    (while (and from (not (vectorp (car from))))
      (setq arg (car from))
      (cond ((eq arg :local)
             (setq arg `(cd ,zucchini--local-directory)))
            ((eq arg :project)
             (setq arg `(cd ,zucchini--project-directory))))
      (setq to (cons arg to))
      (setq from (cdr from)))
    (cons (cons key (reverse to)) from)))

(defun zucchini--parse-tasks (args)
  (let ((tasks nil))
    (while args
      (setq args (zucchini--parse-task args))
      (setq tasks (cons (car args) tasks))
      (setq args (cdr args)))
    (reverse tasks)))

(defun zucchini--task-wrapper (body)
  `(lambda ()
     (interactive)
     (unwind-protect
         (progn ,@body)
       (cd ,zucchini--local-directory))))

(defun zucchini--add-task (task)
  `(add-to-list
    'zucchini--tasks
    (cons ,(car task) ,(zucchini--task-wrapper (cdr task)))))

(defmacro zucchini-register (&rest args)
  `(progn
     ,@(mapcar
        #'zucchini--add-task (zucchini--parse-tasks args))))

(defun zucchini--press ()
  (message "Zucchini waiting for key press...")
  (let* ((key (single-key-description (read-event)))
         (task
          (assoc (vector (intern key)) zucchini--tasks 'equal)))
    (if task
        (call-interactively (cdr task))
      (message "Can't find task bound to [%s]" key))))

(defun zucchini--load (file)
  (when (and
         (not (seq-contains-p zucchini-trusted-scripts file))
         (y-or-n-p (format "Do you want to trust '%s'?" file)))
    (customize-save-variable
     'zucchini-trusted-scripts (cons file zucchini-trusted-scripts)))
  (if (seq-contains-p zucchini-trusted-scripts file)
      (load file)
    (message "Trust was not given for '%s'" file)
    nil))

(defun zucchini-play ()
  (interactive)
  (setq zucchini--tasks nil)
  (setq zucchini--project-directory
        (locate-dominating-file "." "zucchini.el"))
  (setq zucchini--local-directory default-directory)
  (if (not zucchini--project-directory)
      (message "Can't find Zucchini script")
    (when (zucchini--load
           (file-name-concat
            zucchini--project-directory "zucchini.el"))
      (zucchini--press))))

(provide 'zucchini)

;;; zucchini.el ends here 

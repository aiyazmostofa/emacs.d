;;; zucchini.el --- Per project keybindings -*- lexical-binding: t; -*-

;; TODO: Add documentation

(defvar zucchini--actions nil)
(defvar zucchini--dominating-directory nil)
(defcustom zucchini-trusted-scripts nil
  "Trusted Zucchini scripts"
  :group 'zucchini
  :type '(repeat (file)))

(defun zucchini--parse-action (args)
  (let ((key (car args))
        (arg nil)
        (from (cdr args))
        (to nil))
    (when (not (vectorp key))
      (error "Zucchini register has incorrect form"))
    (while (and from (not (vectorp (car from))))
      (setq arg (car from))
      (cond ((eq arg :local)
             (setq arg `(cd ,default-directory)))
            ((eq arg :project)
             (setq arg `(cd ,zucchini--dominating-directory))))
      (setq to (cons arg to))
      (setq from (cdr from)))
    (cons (cons key (reverse to)) from)))

(defun zucchini--parse-actions (args)
  (let ((actions nil))
    (while args
      (setq args (zucchini--parse-action args))
      (setq actions (cons (car args) actions))
      (setq args (cdr args)))
    (reverse actions)))

(defun zucchini--action-wrapper (body)
  `(lambda ()
     (interactive)
     (let ((default-directory ,zucchini--dominating-directory))
       ,@body)))

(defun zucchini--add-action (action)
  `(add-to-list
    'zucchini--actions
    (cons ,(car action) ,(zucchini--action-wrapper (cdr action)))))

(defmacro zucchini-register (&rest args)
  `(progn
     ,@(mapcar
        #'zucchini--add-action (zucchini--parse-actions args))))

(defun zucchini--press ()
  (message "Zucchini waiting for key press...")
  (let* ((key (single-key-description (read-event)))
         (action
          (assoc (vector (intern key)) zucchini--actions 'equal)))
    (if action
        (call-interactively (cdr action))
      (message "Can't find action bound to [%s]" key))))

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
  (setq zucchini--actions nil)
  (setq zucchini--dominating-directory
        (locate-dominating-file "." "zucchini.el"))
  (if (not zucchini--dominating-directory)
      (message "Can't find Zucchini script")
    (when (zucchini--load
           (file-name-concat
            zucchini--dominating-directory "zucchini.el"))
      (zucchini--press))))

(provide 'zucchini)

;;; zucchini.el ends here 

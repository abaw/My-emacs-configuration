(require 'p4-lowlevel)

(defvar q4-user nil "The p4 user name")

(defun q4-args (&rest args)
  "return the proper argument list for used in p4 commands."
  (if q4-user
    (append (list "-u" q4-user) args)
    args))

(defmacro q4-with-current-file (&rest body)
  "If current buffer is associated with a file, bind
  current-filename and current-filename* to absolute path and
  non-absolute path respectively. Then execute BODY. Otherwise,
  BODY is ignored and a error message is printed"
  `(let* ((current-filename* (buffer-file-name (current-buffer)))
	  (current-filename (when current-filename* (file-truename current-filename*))))
     (if current-filename
	 ,@body
       (error "This buffer is not associated with a file"))))

(defun q4-edit ()
  "do \"p4 edit\" for current file"
  (interactive)
  (q4-with-current-file
   (when (p4-lowlevel-command-or-error (q4-args "edit" current-filename))
     (toggle-read-only 0))))

(defun q4-revert ()
  "do \"p4 edit\" for current file"
  (interactive)
  (q4-with-current-file
   (when (p4-lowlevel-command-or-error (q4-args "revert" current-filename))
     (revert-buffer t t))))

(provide 'q4)

;; (require 'vc-p4)
;; (setq vc-p4-require-p4config t)

;; use p4.el
(when (abaw-try-to-require 'p4)
 (setq p4-follow-symlinks t)
 (setq p4-use-p4config-exclusively t)
 (defun abaw-p4-describe (cl)
   "p4 describe CL and return an alist translated from p4 tagged results"
   (loop for line in (with-temp-buffer
		       (if (= 0 (p4-exec-p4 (current-buffer) (list "-s" "-ztag" "describe" (number-to-string cl)) nil))
			   (split-string (buffer-string) "\n")
			 (error (buffer-string))))
	 ;; got an error
	 if (string-match "^error:" line)
	 do (error line)

	 ;; tagged results
	 if (string-match "info[[:digit:]]: \\(\\w+\\) \\(.+\\)" line)
	 collect (list (match-string 1 line) (match-string 2 line))))

 (defun abaw-p4-describe-get-files-string (alist)
   "return a list of strings describing affected files in ALIST, ALIST is returned by abaw-p4-describe"
   (loop for tag in alist
	 if (string-match "^depotFile" (car tag))
	 collect (cadr tag)))

 (defun abaw-org-template-changelist ()
   "return the template used for a changelist entry for org-capture"
   (let ((cl (read-minibuffer "Enter a changelist#:")))
     (unless (and cl (number-or-marker-p cl))
       (error "changelist should be a number"))
     (let ((tags (abaw-p4-describe cl)))
       (format "* TODO %d %s %%^g
:PROPERTIES:
:NUMBER:   %d
:END:
%%?
Affected files:%s
Added: %%U\n"
	       cl
	       (cadr (assoc-string "desc" tags))
	       cl
	       (abaw-p4-describe-get-files-string tags)))))
)

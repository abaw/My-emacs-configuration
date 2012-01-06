;; (require 'vc-p4)
;; (setq vc-p4-require-p4config t)

;; use p4.el
(when (abaw-try-to-require 'p4)
 (setq p4-follow-symlinks t)
 (setq p4-use-p4config-exclusively t)

 (defun abaw-p4-exec-tagged-result (args)
   "run p4 command with ARGS and return a tagged result. It will
   raise en error if the command failed."

   (loop for line in (with-temp-buffer
		       (if (= 0 (p4-exec-p4 (current-buffer) (append '("-s" "-ztag") args) nil))
			   (split-string (buffer-string) "\n")
			 (error (buffer-string))))
	 ;; got an error
	 if (string-match "^error:" line)
	 do (error line)

	 ;; tagged results
	 if (string-match "info[[:digit:]]: \\(\\w+\\) \\(.+\\)" line)
	 collect (list (match-string 1 line) (match-string 2 line))))

 (defun abaw-p4-describe (cl)
   "p4 describe CL and return an alist translated from p4 tagged results"
   (abaw-p4-exec-tagged-result (list "describe" (number-to-string cl))))

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

 (defun abaw-org-sync-all-changelist (&optional min-changelist)
   "create entries in refile.org for all changelists that are
submitted by me on the p4 server and not already contained in any
of my agenda files. MIN-CHANGELIST if given is the minimum
changelist to start to sync with. Then I can manully refile it
into proper place. This create a buffer first for you to review
the changelists before pulling them in refile.org"
   (interactive "nWith Minimum Changelist#:")
   (when (yes-or-no-p (format "p4 user is %s, correct?" (getenv "P4USER")))
     (let* ((min-changelist (or min-changelist 1))
	    (matcher (cdr (org-make-tags-matcher "+CHANGELIST")))
	    (all-search-files (remove-if-not 'file-exists-p (org-agenda-files t t)))
	    (org-changelists (apply 'append
				    (mapcar (lambda (file)
					      (with-current-buffer (org-get-agenda-file-buffer file)
						(mapcar (lambda (prop) (string-to-number (cdr prop)))
							(remove nil (org-scan-tags
								     (lambda () (assoc-string "NUMBER"
											      (org-entry-properties)))
								     matcher)))))
					    all-search-files)))
	    (not-in-org-changelists (loop for tag in (abaw-p4-exec-tagged-result (list "changes" "-u" (getenv "P4USER") "-s" "submitted" "-l"))
					  with change
					  if (and (not change) (equal (car tag) "change"))
					  do (let ((n (string-to-number (cadr tag))))
					       (when (and (>= n min-changelist)
							  (not (memq n org-changelists)))
						 (setq change n)))
					  else if (and change (equal (car tag) "desc"))
					  collect (cons change (cadr tag)) and do (setq change nil))))

       (pop-to-buffer "*SYNC-CHANGELISTS-TO-ORG*")
       (erase-buffer)
       (conf-mode)
       (setq header-line-format "Finish with `C-c C-c'")
       (loop for cl in not-in-org-changelists
	     do (insert (format "%d # %s\n" (car cl) (cdr cl))))
       
       (local-set-key (kbd "C-c C-c")
		      (lambda ()
			(interactive)
			(let ((refile (or (find "refile.org" org-agenda-files
						:key 'file-name-nondirectory :test 'equal)
					  (error "no refile.org in org-agenda-files, dunno where to put the changelist entries"))))
			  (goto-char (point-min))

			  (flet ((get-cl-number-at-line ()
							"get changelist number at
current line if there is one."
							(let ((line (buffer-substring-no-properties (line-beginning-position)
												    (line-end-position))))
							  (when (string-match "^[[:space:]]*\\([[:digit:]]+\\)" line)
							    (string-to-number (match-string 1 line)))))
				 (insert-entry-for-changelist (cl-number)
							      "insert a changelist entry to refile.org"
							      (message "Processing for CL#%d" cl-number)
							      (awhen (abaw-p4-describe cl-number)
								     (let ((template (format "* TODO %d %s
:PROPERTIES:
:NUMBER: %d
:END:

Affected files:%s
Added: %%U
" cl-number (cadr (assoc-string "desc" it)) cl-number (abaw-p4-describe-get-files-string it))))
								       (org-capture-set-plist `("" "" entry (file ,refile) ,template))
								       (org-capture-set-target-location)
								       (org-capture-put :template (org-capture-fill-template))
								       (with-current-buffer (org-capture-get :buffer)
									 (goto-char (org-capture-get :pos))
									 (org-capture-insert-template-here))))))

			    (org-capture-put :default-time (org-current-time))
			    (loop do (awhen (get-cl-number-at-line)
					    (insert-entry-for-changelist it))
				  while (= 0 (forward-line)))
			    (kill-buffer (current-buffer)))))))))
)

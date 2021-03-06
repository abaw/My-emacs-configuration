(add-to-list 'load-path (concat my-emacs-lisp-dir (file-name-as-directory "org-mode") "lisp"))
(add-to-list 'load-path (concat my-emacs-lisp-dir (file-name-as-directory "org-mode") (file-name-as-directory "contrib") "lisp"))

(when (abaw-try-to-require 'org)
  (global-set-key (kbd "<f12>") 'org-agenda)
  (global-set-key (kbd "C-S-r") 'org-capture)

  (add-hook 'org-mode-hook
	    #'(lambda ()
		(flyspell-mode t)))

  (add-hook 'org-agenda-mode-hook (lambda () (highlight-current-line-minor-mode t)))

  ;; todo keys settings
  (setq org-todo-keywords
	'((sequence "MAYBE(m)" "TODO(t)" "STARTED(s)" "WAITING(w)" "POSTPONED(p)" "|" "DONE(d)" "CANCELLED(c)" "DELEGATED(D)" )))

  (setq org-todo-state-tags-triggers
	'((done ("DONE" . t) ("STARTED" . t))
	  ("TODO" ("STARTED"))
	  ("MAYBE" ("STARTED"))
	  ("STARTED" ("STARTED" .t))
	  ("WAITING" ("STARTED" .t))))

  ;; change todo to STARTED while clocking in a tody entry which is in state "TODO" or "MAYBE"
  (defun abaw-org-update-state-for-current-clock-entry ()
    "Updates the todo state to \"STARTED\" if current clock entry
    is a todo entry and in state \"TODO\" or \"MAYBE\". "

    (unless (org-clocking-p)
      (error "not current clocking entry"))

    (save-excursion
      (save-window-excursion
	(org-clock-goto)
	(awhen (org-get-todo-state)
	       (when (member it '("TODO" "MAYBE"))
		 (org-todo "STARTED"))))))

  (add-hook 'org-clock-in-hook 'abaw-org-update-state-for-current-clock-entry)

  ;; highlists persists after modifying buffer
  (setq org-remove-highlights-with-change nil)

  (setq org-agenda-custom-commands
	'((" " "Agenga"
	   ((agenda "" nil)
	    (tags "+CATEGORY=\"REFILE\""
		  ((org-agenda-overriding-header "Refile These Tasks")
		   (org-agenda-prefix-format "  ")))
	    (tags-todo "+STARTED"
		       ((org-agenda-overriding-header "Started and Not Finished Tasks")
			(org-agenda-skip-function
			 '(org-agenda-skip-entry-if 'scheduled))
			(org-agenda-sorting-strategy
			 '(category-keep todo-state-down))))
	    (tags-todo "-PROJECT-INTEGRATION/!-STARTED-WAITING"
		       ((org-agenda-overriding-header "Tasks")
			(org-agenda-skip-function
			 '(org-agenda-skip-entry-if 'scheduled 'deadline))
			(org-agenda-sorting-strategy
			 '(category-keep todo-state-down))))
	    (tags-todo "+PROJECT/!-STARTED-WAITING"
		       ((org-agenda-overriding-header "Project Tasks")
			(org-agenda-skip-function
			 '(org-agenda-skip-entry-if 'scheduled 'deadline))
			(org-agenda-sorting-strategy
			 '(category-keep todo-state-down))))
	    (tags-todo "+INTEGRATION/!-STARTED-WAITING"
		       ((org-agenda-overriding-header "Unintegrated Changelists")
			(org-agenda-skip-function
			 '(org-agenda-skip-entry-if 'scheduled 'deadline))
			(org-agenda-sorting-strategy
			 '(todo-state-down user-defined-up))
			(org-agenda-prefix-format "  "))) ;; user-defined-up make entries with low NUMBER first.
	    (tags-todo "+BACKGROUND"
		       ((org-agenda-overriding-header "All Background Tasks")))))

	  ("w" "Last Week Clocking Review"
	   ((abaw-org-review-clocking "-1w" nil)))

	  ("c" "All Changelists"
	   ((tags "+CHANGELIST+NUMBER>0"
		  ((org-agenda-sorting-strategy
		    '(user-defined-up))
		   (org-agenda-prefix-format "")))))))

  ;; the function used to compare two entries
  (defun abaw-org-cmp-entry (a b)
    "return +1 if A > B, return -1 if a < B, return 0 if a = B"
    (labels ((get-number (entry)
			 "return the associated number of an entry or nil if no such number"
			 (awhen (cdr (assoc "NUMBER"
					    (org-entry-properties
					     (get-text-property 0 'org-hd-marker entry) 'all "NUMBER")))
				(string-to-number it))))
      (let (na nb)
	(if (and (setq na (get-number a))
		 (setq nb (get-number b)))
	    (cond
	     ((< na nb) -1)
	     ((> na nb) +1)
	     (t nil))
	  ;; no NUMBER property, just treat each entry equally
	  0))))
  (setq org-agenda-cmp-user-defined 'abaw-org-cmp-entry)

  ;; added link type "latex", use this [latex:xxx][yyy] => \xxx{yyy}
  (org-add-link-type
   "latex" nil
   (lambda (path desc format)
     (cond
      ((eq format 'html)
       (format "<span class=\"%s\">%s</span>" path desc))
      ((eq format 'latex)
       (format "\\%s{%s}" path desc)))))

  (abaw-try-to-require 'org-s5)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
				 (ditaa . t)))

  ;; configure remember if we have remember installed(this is packed with Emacs 23)
  ;; no more using remember because we have better org-capture
  ;; (when (fboundp 'remember)
  ;;   (org-remember-insinuate))

  ;; some help functions
  (defun abaw-org-export-pdf-by-html ()
    "export current org file to pdf by wkhtmltopdf program"
    (let* ((file-without-ext (file-name-sans-extension (buffer-file-name)))
	   (html-file (concat file-without-ext ".html"))
	   (pdf-file  (concat file-without-ext ".pdf")))
      (when (or (not (file-exists-p pdf-file)) (yes-or-no-p (format "%s already existed, overwrite it?" pdf-file)))
	(call-interactively 'org-export-as-html)
	(shell-command (format "wkhtmltopdf %s %s" html-file pdf-file)))))

  (defun abaw-org-review-clocking (&optional match)
    "Reviewing clocking data until now. MATCH is a time
    specification like \"-1w\" or \"2011-12-25\". The MATCH argument is for easily
    used in org-agenda. If MATCH is not passed, this function
    will prompt you to input the date. "
    (interactive)

    (let* ((fmt "%Y-%m-%d %H:%M")
	   (start-time (format-time-string
			fmt
			(org-read-date t t match nil (org-read-date t t "-1w"))))
	   (end-time (format-time-string
		      fmt
		      (org-read-date t t "now")))
	   (review-buffer (get-buffer-create "*REVIEW-CLOCKING*")))
      (with-current-buffer review-buffer
	(erase-buffer)
	(unless (eq major-mode 'org-mode)
	  (org-mode))

	(insert (format "#+BEGIN: clocktable :scope agenda :tstart \"%s\" :tend \"%s\""
			start-time
			end-time))
	(newline)
	(set-mark (point))
	(insert "#+END:")
	(newline)
	(pop-to-mark-command)
	(org-dblock-update)
	(switch-to-buffer review-buffer))))

  ;; org-beamer
  (setq org-beamer-environments-extra
	'(("overlayarea" "r" "\\begin{overlayarea}{\\textwidth}{\\textheight}" "\\end{overlayarea}")))

  ;; FIXME: how could this work??
  (defadvice org-attach-tag (around tag-while-read-only first (&rest args))
    "because org-attach-tag may be called in column view which is read-only. This advice function make this possible"
    )


  (defadvice org-agenda-set-tags (around with-agenda-tag-list first (&optional tag onoff) activate)
    "in agenda buffer, we want to access to available tags while
setting tags"
    (let ((org-tag-alist (org-uniquify (append org-tag-alist org-tag-alist-for-agenda))))
      ad-do-it))

  (defun abaw-org-insert-screenshot ()
    "insert a screenshot in org-mode. it will use `scrot' program
to capture the screenshot and insert a link to the taken image in
org-mode."
    (interactive)
    (unless (eq major-mode 'org-mode)
      (error "This is only useful in org-mode"))

    (let ((filename (concat (format-time-string "%Y%m%d_%H%M_screenshot" (current-time))
			    (format "_%s.png" (make-temp-name "")))))
      (when (file-exists-p filename)
	(error "a screenshot file already exists, try it again."))
      (message "Selecting a window or rectangle to take screenshot!!")
      (if (= 0 (call-process "scrot" nil nil nil "-s" filename))
	  (progn (insert (format "[[file:%s]]\n" filename))
		 (org-display-inline-images))
	(error "scrot failed"))))

  (defun abaw-org-capture-add-timestamp ()
    "Add timestamp for current \"todo\" item. this is intended to
    hook to org-capture-mode-hook."
    (when (org-entry-is-todo-p)
      (org-set-property "ADDED" (with-temp-buffer (org-insert-time-stamp (current-time) t t)
						  (buffer-string)))))

)

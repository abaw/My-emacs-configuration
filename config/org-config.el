(add-to-list 'load-path (concat my-emacs-lisp-dir (file-name-as-directory "org-mode") "lisp"))
(add-to-list 'load-path (concat my-emacs-lisp-dir (file-name-as-directory "org-mode") (file-name-as-directory "contrib") "lisp"))

(when (abaw-try-to-require 'org)
  (global-set-key (kbd "<f12>") 'org-agenda)
  (global-set-key (kbd "C-S-r") 'org-capture)
  (setq org-todo-keywords
	'((sequence "MAYBE(m)" "TODO(t)" "STARTED(s)" "WAITING(w)" "POSTPONED(p)" "|" "DONE(d)" "CANCELLED(c)" "DELEGATED(d)" )))
  (setq org-todo-state-tags-triggers
	'((done ("DONE" . t))))
  (add-hook 'org-mode-hook
	    #'(lambda ()
		(flyspell-mode t)))

  ;; highlists persists after modifying buffer
  (setq org-remove-highlights-with-change nil)
  (setq org-agenda-custom-commands
	'((" " "Agenga"
	   ((agenda "" nil)
	    (tags-todo "-PROJECT-INTEGRATION/!-STARTED-WAITING"
		       ((org-agenda-overriding-header "Tasks")
			(org-agenda-skip-function
			 '(org-agenda-skip-entry-if 'scheduled 'deadline))
			(org-agenda-sorting-strategy
			 '(category-keep todo-state-down))))
	    (tags-todo "+PROJECT"
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
		       ((org-agenda-overriding-header "Background Tasks")))
	    (todo "WAITING"
		  ((org-agenda-overriding-header "Waiting Tasks")))))))

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

  ;; org-beamer
  (setq org-beamer-environments-extra
	'(("overlayarea" "r" "\\begin{overlayarea}{\\textwidth}{\\textheight}" "\\end{overlayarea}")))
)
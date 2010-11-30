(add-to-list 'load-path (concat my-emacs-lisp-dir (file-name-as-directory "org-mode") "lisp"))
(add-to-list 'load-path (concat my-emacs-lisp-dir (file-name-as-directory "org-mode") (file-name-as-directory "contrib") "lisp"))

(when (abaw-try-to-require 'org-install)
  (setq org-todo-keywords
	'((sequence "TODO" "WAIT" "|" "DONE" "DELEGATED" "CANCELED")))
  (add-hook 'org-mode-hook
	    #'(lambda ()
		(flyspell-mode t)))

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
				 (ditaa . t))))

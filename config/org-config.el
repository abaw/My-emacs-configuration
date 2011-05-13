(add-to-list 'load-path (concat my-emacs-lisp-dir (file-name-as-directory "org-mode") "lisp"))
(add-to-list 'load-path (concat my-emacs-lisp-dir (file-name-as-directory "org-mode") (file-name-as-directory "contrib") "lisp"))

(when (abaw-try-to-require 'org-install)
  (setq org-todo-keywords
	'((sequence "TODO" "WAIT" "|" "DONE" "DELEGATED" "CANCELED")))
  (add-hook 'org-mode-hook
	    #'(lambda ()
		(flyspell-mode t)))

  (abaw-try-to-require 'org-s5)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
				 (ditaa . t)))

  ;; configure remember if we have remember installed(this is packed with Emacs 23)
  (when (fboundp 'remember)
    (org-remember-insinuate)))
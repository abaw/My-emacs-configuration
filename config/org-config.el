(when (fboundp 'org-mode)
  (setq org-todo-keywords
	'((sequence "TODO" "WAIT" "|" "DONE" "DELEGATED" "CANCELED")))
  (add-hook 'org-mode-hook
	    #'(lambda ()
		(flyspell-mode t))))
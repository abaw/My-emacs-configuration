(add-to-list 'load-path (concat my-emacs-lisp-dir (file-name-as-directory "org-mode") "lisp"))
(add-to-list 'load-path (concat my-emacs-lisp-dir (file-name-as-directory "org-mode") (file-name-as-directory "contrib") "lisp"))

(when (abaw-try-to-require 'org)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (setq org-todo-keywords
	'((sequence "TODO" "WAIT" "|" "DONE" "DELEGATED" "CANCELED")))
  (add-hook 'org-mode-hook
	    #'(lambda ()
		(flyspell-mode t)))

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
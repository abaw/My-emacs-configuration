(add-to-list 'load-path (abaw-path (list my-emacs-lisp-dir "doxymacs" "lisp") t))
(when (abaw-try-to-require 'doxymacs)
  (custom-set-variables '(doxymacs-doxygen-style "Qt"))
  (defvar *doxymacs-paragraph-keywords* '("file" "author" "date" "brief" "param" "return" "see") "a list of doxymacs keywords that should be treated as paragraph start")
  (defadvice doxymacs-mode (before setup-paragraph activate)
    (setq paragraph-start (concat paragraph-start
				  "\\|[ \t]*\\([\\@]"
				  (loop for keyword in *doxymacs-paragraph-keywords*
					with result = ""
					do (setq result (concat result "\\|" keyword))
					finally (return result)) "\\)")))

  (add-hook 'c-mode-common-hook 'doxymacs-mode)
  (add-hook 'font-lock-mode-hook (lambda ()
				   (when (or (eq major-mode 'c-mode)
					     (eq major-mode 'c++-mode))
				     (doxymacs-font-lock)))))

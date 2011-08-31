(add-to-list 'load-path (concat my-emacs-lisp-dir "bash-completion"))
(when (abaw-try-to-require 'bash-completion)
  (bash-completion-setup))

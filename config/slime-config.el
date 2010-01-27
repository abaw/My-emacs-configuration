(add-to-list 'load-path (concat my-emacs-lisp-dir "slime"))
(setq inferior-lisp-program "sbcl")
(require 'slime)
(slime-setup '(slime-fancy))

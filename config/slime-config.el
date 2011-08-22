;; (add-to-list 'load-path (concat my-emacs-lisp-dir "slime"))

;; (require 'slime)
;; (slime-setup '(slime-fancy))

;; (when (abaw-try-to-require 'paredit)
;;   (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1))))

(let ((helper-file (expand-file-name "~/quicklisp/slime-helper.e")))
  (if (file-exists-p helper-file)
      (progn (load helper-file)
	     (setq inferior-lisp-program "sbcl")
	     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol))
    (message "%s: not found %s" (buffer-file-name) helper-file)))


(when (abaw-try-to-require 'ido)
  (ido-mode t)
  (fset 'read-file-name 'ido-read-file-name)
  (setq ido-enable-flex-matching t)

  (when (abaw-try-to-require 'smex)
    (add-hook 'after-init-hook 'smex-initialize)

    (defun my-smex (&optional args)
      "with C-u, update the smex database first."
      (interactive "P")
      (call-interactively
       (if args
	   'smex-update-and-run
	 'smex)))

    (global-set-key (kbd "M-x") 'my-smex)
    (global-set-key (kbd "C-c M-x") 'execute-extended-command)))

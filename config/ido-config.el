(when (abaw-try-to-require 'ido)
  (defadvice ido-ignore-item-p (around ido-wa first (item &rest other-args) activate)
    "because ido-ignore-item-p may get a nil when there are
killed buffers whose (buffer-name) return nil. We must filter nil
values."
    (when item
      ad-do-it))

  (ido-mode t)
  ;; this is kind of danger
  (fset 'read-file-name 'ido-read-file-name)
  (setq ido-enable-flex-matching t)
  (setq ido-default-buffer-method 'selected-window)
  (setq ido-default-file-method 'selected-window)

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

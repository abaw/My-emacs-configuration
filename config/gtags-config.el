(when (abaw-try-to-require 'gtags)
  (defun my-enable-gtags-mode ()
    (gtags-mode t))
  (add-hook 'c-mode-hook 'my-enable-gtags-mode)
  (add-hook 'c++-mode-hook 'my-enable-gtags-mode)

  (when (abaw-try-to-require 'anything-gtags)
    (defun my-gtags-find (&optional use-anything)
      "the convenient command to choose gatgs-find-tag or
      anything-gtags-select. with the prefix argument,
      anything-gtags-select will be used."
      (interactive "P")
      (if use-anything
	  (call-interactively 'anything-gtags-select)
	(call-interactively 'gtags-find-tag)))

    (setq anything-gtags-enable-initial-pattern t)
    (define-key gtags-mode-map (kbd "M-.") 'my-gtags-find)))


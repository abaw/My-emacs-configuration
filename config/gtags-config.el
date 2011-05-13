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

    (defadvice gtags-goto-tag (around gtags-lib-path first (&rest ignored-args) activate)
      "using gtags-lib-path as our GTASLIBPATH environment
      variable if it is set."
      (let ()
	(if (and (boundp 'gtags-lib-path) gtags-lib-path)
	    (let ((old-path (getenv "GTAGSLIBPATH")))
	      (setenv "GTAGSLIBPATH" gtags-lib-path)
	      ad-do-it
	      (setenv "GTAGSLIBPATH" old-path))
	  ad-do-it)))

    (add-to-list 'anything-for-files-prefered-list 'anything-c-source-gtags-files)

    (setq anything-gtags-enable-initial-pattern t)
    (define-key gtags-mode-map (kbd "M-.") 'my-gtags-find)))


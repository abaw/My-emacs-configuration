(when (abaw-try-to-require 'anything-config)
  (global-set-key (kbd "C-c C-f") 'anything-for-files)

  (when (abaw-try-to-require 'anything-c-moccur)
    (setq moccur-split-word t
	  anything-c-moccur-enable-auto-look-flag t
	  anything-c-moccur-enable-initial-pattern t
	  anything-c-moccur-higligt-info-line-flag)
    (add-hook 'dired-mode-hook
	      '(lambda ()
		 (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur))))

  (abaw-try-to-require 'anything-match-plugin))

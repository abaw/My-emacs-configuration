(when (abaw-try-to-require 'anything-config)
  (global-set-key (kbd "<f5>") 'anything-for-files)

  (defvar anything-c-source-open-files
    '((name . "Opened Files")
      (candidates . (lambda () (remove nil (mapcar 'buffer-file-name (buffer-list)))))
      (action . '("Switch to buffer" . find-file))
      (action-transformer . (anything-c-transform-file-load-el anything-c-transform-file-browse-url))
      (candidate-transformer . (anything-c-w32-pathname-transformer anything-c-skip-boring-files anything-c-shorten-home-path))))

  (setq anything-for-files-prefered-list '(anything-c-source-files-in-current-dir+
					   anything-c-source-buffers+
					   anything-c-source-open-files
					   anything-c-source-ffap-guesser
					   anything-c-source-ffap-line
					   anything-c-source-recentf))

  (when (abaw-try-to-require 'anything-c-moccur)
    (setq moccur-split-word t
	  anything-c-moccur-enable-auto-look-flag t
	  anything-c-moccur-enable-initial-pattern t
	  anything-c-moccur-higligt-info-line-flag)
    (add-hook 'dired-mode-hook
	      '(lambda ()
		 (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur))))

  (abaw-try-to-require 'anything-match-plugin))

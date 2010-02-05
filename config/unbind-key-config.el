;; unbind keys we don't want to have

(add-hook 'comint-mode-hook
	  (lambda ()
	    (define-key comint-mode-map (kbd "C-<up>") nil)
	    (define-key comint-mode-map (kbd "C-<down>") nil)))

(when (abaw-try-to-require 'gtags)
  (defun my-enable-gtags-mode ()
    (gtags-mode t))
  (add-hook 'c-mode-hook 'my-enable-gtags-mode)
  (add-hook 'c++-mode-hook 'my-enable-gtags-mode)

  (when (abaw-try-to-require 'anything-gtags)
    (setq anything-gtags-enable-initial-pattern t)
    (define-key gtags-mode-map (kbd "M-.") 'anything-gtags-select)))


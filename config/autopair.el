(when (abaw-try-to-require 'autopair)
  (autopair-global-mode)
  (add-hook 'sldb-mode-hook #'(lambda () (setq autopair-dont-activate t))))

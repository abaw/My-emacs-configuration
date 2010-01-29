(when (abaw-try-to-require 'anything-config)
  (global-set-key (kbd "C-c C-f") 'anything-for-files))
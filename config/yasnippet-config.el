(when (abaw-try-to-require 'yasnippet)
  (require 'time-stamp)
  
  (yas/global-mode t)
  (yas/load-directory (concat my-emacs-dir "snippets"))
  
  ;; if we use setq, the default "TAB" key will not be unbind.
  (custom-set-variables '(yas/trigger-key "<C-tab>")))

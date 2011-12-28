(add-to-list 'load-path (concat my-emacs-lisp-dir "ibus"))
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)

;; lisp hook
(defun my/lisp-hook ()
  (eldoc-mode t))

(add-hook 'lisp-mode-hook 'my/lisp-hook)
(add-hook 'emacs-lisp-mode-hook 'my/lisp-hook)

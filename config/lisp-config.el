;; lisp hook
(defun my/lisp-hook ()
  (eldoc-mode t)
  (when (eq major-mode 'emacs-lisp-mode)
    (local-set-key (kbd "M-.") 'find-function)
    (local-set-key (kbd "M-*") 'pop-global-mark)))

(add-hook 'lisp-mode-hook 'my/lisp-hook)
(add-hook 'emacs-lisp-mode-hook 'my/lisp-hook)


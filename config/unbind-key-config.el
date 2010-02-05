;; unbind keys we don't want to have

(defvar my-keys-may-conflict (list (kbd "C-<up>") (kbd "C-<down>"))
  "The keys that may conflicts my hot key settings in some major
  modes")

(defvar my-major-modes-with-key-conflict (list 'comint-mode 'eshell-mode))

(defun my-unbind-conflict-keys ()
  (dolist (key my-keys-may-conflict)
    (local-set-key key nil)))

(dolist (mode my-major-modes-with-key-conflict)
  (let ((hook-symbol (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook-symbol 'my-unbind-conflict-keys)))

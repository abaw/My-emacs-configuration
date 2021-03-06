(when (abaw-try-to-require 'full-ack)
  (setq ack-executable (executable-find "ack-grep"))
  (defun ack-find-all-files (pattern &optional regexp directory more-args)
    (interactive (append (ack-interactive)
			 (list (split-string (read-from-minibuffer "More arguments(e.g. --cc --cpp):")))))
    (dolist (file (apply 'ack-list-files default-directory
			 (append more-args (unless (zerop (length pattern)) (list pattern)))))
      (find-file-noselect file)
      )))

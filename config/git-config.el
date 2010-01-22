;;(require 'git)
;; (autoload 'git-blame-mode "git-blame"
;;   "Minor mode for incremental blame for Git." t)

(when (abaw-try-to-require 'magit)
  (defun magit-grep (grep-command)
    "grep using magit-grep"
    (interactive (let ((initial-command (magit-format-git-command "grep -n " nil)))
		   (list (read-shell-command "Run git grep(like this):" initial-command))))
    (compilation-start grep-command 'grep-mode)))

;;(require 'git)
;; (autoload 'git-blame-mode "git-blame"
;;   "Minor mode for incremental blame for Git." t)

(when (abaw-try-to-require 'magit)
  (defun magit-grep (grep-command)
    "grep using magit-grep"
    (interactive (let ((initial-command (magit-format-git-command "grep -n " nil)))
                   (list (read-shell-command "Run git grep(like this):" initial-command))))
    (compilation-start grep-command 'grep-mode))

  (when (abaw-try-to-require 'anything-config)
    (defvar anything-c-source-git-project-files
      '((name . "Files from Current GIT Project")
	(init . (lambda () (setq anything-git-top-dir (magit-get-top-dir (if (buffer-file-name)
									     (file-name-directory (buffer-file-name))
									   default-directory)))))
        (candidates . (lambda ()
			(if anything-git-top-dir
			    (let ((default-directory anything-git-top-dir))
			      (mapcar (lambda (file) (concat default-directory file)) (magit-shell-lines (magit-format-git-command "ls-files" nil)))))))
        (type . file)))
    (add-to-list 'anything-for-files-prefered-list 'anything-c-source-git-project-files)))


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
    (defvar anything-c-source-git-project-files-cache nil "(path signature cached-buffer)")
    (defvar anything-c-source-git-project-files
      '((name . "Files from Current GIT Project")
	(init . (lambda ()
		  (let* ((top-dir (magit-get-top-dir (if (buffer-file-name)
							 (file-name-directory (buffer-file-name))
						       default-directory)))
			 (default-directory top-dir)
			 (signature (magit-shell (magit-format-git-command "rev-parse --verify HEAD" nil))))

		    (unless (and anything-c-source-git-project-files-cache
				 (third anything-c-source-git-project-files-cache)
				 (equal (first anything-c-source-git-project-files-cache) top-dir)
				 (equal (second anything-c-source-git-project-files-cache) signature))
		      (if (third anything-c-source-git-project-files-cache)
			  (kill-buffer (third anything-c-source-git-project-files-cache)))
		      (setq anything-c-source-git-project-files-cache
			    (list top-dir
				  signature
				  (anything-candidate-buffer 'local)))
		      (with-current-buffer (third anything-c-source-git-project-files-cache)
			(dolist (filename (mapcar (lambda (file) (concat default-directory file))
						  (magit-shell-lines (magit-format-git-command "ls-files" nil))))
			  (insert filename)
			  (newline))))
		    (anything-candidate-buffer (third anything-c-source-git-project-files-cache)))))

        (type . file)
	(candidates-in-buffer)))
    (add-to-list 'anything-for-files-prefered-list 'anything-c-source-git-project-files)))

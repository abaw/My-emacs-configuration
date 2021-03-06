;;(require 'git)
;; (autoload 'git-blame-mode "git-blame"
;;   "Minor mode for incremental blame for Git." t)

(add-to-list 'load-path (concat my-emacs-lisp-dir (file-name-as-directory "magit")))

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
		  (aif (magit-get-top-dir (if (buffer-file-name)
					      (file-name-directory (buffer-file-name))
					    default-directory))
		   (let* ((top-dir (file-truename it))
			  (default-directory top-dir)
			  (signature (magit-rev-parse "HEAD")))

		     (unless (and anything-c-source-git-project-files-cache
				  (third anything-c-source-git-project-files-cache)
				  (equal (first anything-c-source-git-project-files-cache) top-dir)
				  (equal (second anything-c-source-git-project-files-cache) signature))
		       (if (third anything-c-source-git-project-files-cache)
			   (kill-buffer (third anything-c-source-git-project-files-cache)))
		       (setq anything-c-source-git-project-files-cache
			     (list top-dir
				   signature
				   (anything-candidate-buffer 'global)))
		       (with-current-buffer (third anything-c-source-git-project-files-cache)
			 (dolist (filename (mapcar (lambda (file) (concat default-directory file))
						   (magit-git-lines "ls-files")))
			   (insert filename)
			   (newline))))
		     (anything-candidate-buffer (third anything-c-source-git-project-files-cache)))
		   (anything-candidate-buffer 'global) ;; need to make a empty candicate buffer if here is not a git reposiroty
		   )))

        (type . file)
	(candidates-in-buffer)))
    (add-to-list 'anything-for-files-prefered-list 'anything-c-source-git-project-files)))

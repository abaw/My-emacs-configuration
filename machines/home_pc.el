;;; path settings
(defvar my-emacs-dir
  (file-name-as-directory
   (file-name-directory
    (directory-file-name
     (file-name-directory
      (file-truename load-file-name))))))

(defvar my-emacs-config-dir
  (file-name-as-directory (concat my-emacs-dir "config")))
(defvar my-emacs-lisp-dir
  (file-name-as-directory (concat my-emacs-dir "lisp")))

(add-to-list 'load-path my-emacs-config-dir)
(add-to-list 'load-path my-emacs-lisp-dir)

(set-fontset-font "fontset-startup" 'ascii "Monaco-14")
(set-fontset-font "fontset-startup" 'han "LiHei Pro-14")

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(load "basic-config")
(load "cc-config")
(load "lisp-config")
(load "yasnippet-config")
(load "doremi-config")
(load "git-config")
(load "org-config")
(load "slime-config")
(load "ido-config")

(setq magit-git-executable "/usr/local/git/bin/git")

(put 'set-goal-column 'disabled nil)

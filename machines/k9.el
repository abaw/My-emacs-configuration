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
(load "my-anything-config")
(load "unbind-key-config")
(load "ack-config")
(load "doxymacs-config")
(load "header-config")
(load "gtags-config")

;; add my projects to ibuffer-saved-filter-groups
(setq ibuffer-saved-filter-groups
      (cons (loop for item in (find "default" ibuffer-saved-filter-groups :key #'car :test #'equal)
		  ;; my projects
		  when (and (listp item) (equal (car item) "Programming"))
		  collect '("Project k10.git" (filename . "/home/abaw/k10.git/"))
		  and collect '("Project k9.git" (filename . "/home/abaw/k9.git/"))
		  and collect '("Project 8820.git" (filename . "/home/abaw/projs/8820/8820.git/"))
		  ;; end of my projects
		  when (or (atom item) (not (string-match "^Project " (car item))))
		  collect item)
	    (remove* "default" ibuffer-saved-filter-groups :key #'car :test #'equal)))

(put 'set-goal-column 'disabled nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cscope-do-not-update-database t)
 '(doxymacs-doxygen-style "Qt")
 '(flyspell-use-meta-tab nil)
 '(yas/trigger-key "<C-tab>"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "Green4" :height 1.3)))))

(put 'narrow-to-region 'disabled nil)

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
(add-to-list 'Info-default-directory-list (file-name-as-directory (concat my-emacs-dir "info")))

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
;;(load "ack-config")
;;(load "doxymacs-config")
(load "header-config")
(load "gtags-config")
(load "auto-complete-config")

(setenv "P4CONFIG" ".p4config")
(load "p4-config")
(load "term-config")

;; set the browser
(setq browse-url-generic "opera")
(setq browse-url-browser-function 'browse-url-generic)

;; org-remember templates
(setq org-directory "~/orgs/")
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-capture-templates
      '(("c" "Code" entry (file+headline "ruckus.org" "tracing code") "** %?\n----\n%a")
	("p" "Place" entry (file "places.org") "* %(format \"[[file:%s]]\" (or (with-current-buffer (org-capture-get :original-buffer) (bookmark-buffer-file-name)) (error \"no file associated with this buffer\")))\n")))

;; add my projects to ibuffer-saved-filter-groups
(setq ibuffer-saved-filter-groups
      (cons (loop for item in (find "default" ibuffer-saved-filter-groups :key #'car :test #'equal)
		  ;; my projects
		  when (and (listp item) (equal (car item) "Programming"))
		  collect '("Project WSG" (filename . "/home/abaw/p4dir/mainline/wsg"))
		  and collect '("Project Mainline" (filename . "/home/abaw/p4dir/mainline"))
		  and collect '("Project Release Toronto" (filename . "/home/abaw/p4dir/release/toronto"))
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
 '(ibuffer-saved-filter-groups (quote (("default" ("Org" (mode . org-mode)) ("Magit" (mode . magit-mode)) ("Project WSG" (filename . "/home/abaw/p4dir/wsg")) ("Project Mainline" (filename . "/home/abaw/p4dir/mainline")) ("Project Release Toronto" (filename . "/home/abaw/p4dir/release/toronto")) ("Programming" (or (mode . c-mode) (mode . c++-mode) (mode . perl-mode) (mode . python-mode) (mode . lisp-mode) (mode . emacs-lisp-mode)))))))
 '(ibuffer-saved-filters (quote (("server buffer" ((predicate . server-buffer-clients))) ("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(yas/trigger-key "<C-tab>"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "Green4" :height 1.3)))))

(put 'narrow-to-region 'disabled nil)

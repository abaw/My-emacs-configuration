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

(put 'set-goal-column 'disabled nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flyspell-use-meta-tab nil)
 '(yas/trigger-key "<C-tab>"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

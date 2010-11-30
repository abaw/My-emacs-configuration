;; use emacsclient as the default editors for most programs
(setenv "EDITOR" "emacsclient")

;; a bigger kill-ring buffer
(setq kill-ring-max 200)

;; get rid of toolbar
(tool-bar-mode 0)

;; load my utilities
(load "abaw")

;; load utilities collected from the internet
(load "misc-utilities")

;; hippie-expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-visible
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))

;; cua mode for rectangle selection
(when (fboundp 'cua-mode)
  (cua-mode t)
  (setq cua-enable-cua-keys 'shift))

;; load various useful packages
(mapc  #'(lambda (package) (abaw-try-to-require package))
       `(highlight-parentheses dired+ dired-sort-menu hide-region
			       hide-lines eol-conversion xcscope
			       all ascii boxquote
			       browse-kill-ring dedicated htmlize
			       keydef perldoc shell-command))

;; common hot keys
(global-set-key (kbd "C-c r") 'font-lock-fontify-buffer)
(global-set-key (kbd "C-2") 'set-mark-command)
(global-set-key (kbd "<f12>") 'compile)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "M-%") 'abaw-query-replace)
(global-set-key (kbd "C-j") 'abaw-newline-and-indent)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-.") 'ska-point-to-register)
(global-set-key (kbd "C-,") 'ska-jump-to-register)
(global-set-key (kbd "C-c f") 'wy-go-to-char)
(global-set-key (kbd "C-c b") 'ywb-create/switch-scratch)
(when (abaw-try-to-require 'ibuffer)
    (global-set-key (kbd "C-x C-b") 'ibuffer)
    (setq ibuffer-saved-filter-groups
	  (quote (("default"
		   ("Org" ;; all org-related buffers
		    (mode . org-mode))
		   ("Magit"
		    (mode . magit-mode))
		   ("Programming" ;; prog stuff not already in MyProjectX
		    (or
		     (mode . c-mode)
		     (mode . c++-mode)
		     (mode . perl-mode)
		     (mode . python-mode)
		     (mode . lisp-mode)
		     (mode . emacs-lisp-mode)
		     ;; etc
		     ))))))
    (add-hook 'ibuffer-mode-hook
	      (lambda ()
		(ibuffer-switch-to-saved-filter-groups "default"))))


;; set title for xterm/putty
(when (and (not window-system)
	   (string-match "^xterm" (getenv "TERM")))
  (if (abaw-try-to-require 'xterm-title)
      (xterm-title-mode 1)))

;; utf-8 is good
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; wrap long lines
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)

;; get best highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq-default show-trailing-whitespace t)

;; active region highlighting
(transient-mark-mode t)

;; frame title is current buffer's name
(setq frame-title-format
      (concat (chomp (shell-command-to-string "hostname"))
	      ":%b:%f"))

;; yes/no => y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; show parens
(show-paren-mode t)

;; show column number
(setq column-number-mode t)

;; better scrolling
(setq scroll-margin 3
      scroll-conservatively 10000)

;; let dired could recursively copy or delete directories
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

;; we want more space
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -99) ;; tool bar be accessed through C-mouse-3
(set-fringe-style 0)

;; the very nice install-elisp package
(when (abaw-try-to-require 'install-elisp)
  (setq install-elisp-repository-directory my-emacs-lisp-dir))

;; install the color theme for emacs when we got a window system
(when (and (abaw-try-to-require 'color-theme) window-system)
 (color-theme-jedit-grey))

;; using cdargs
(abaw-try-to-require 'cdargs)

;; start the server
(server-start)

;; open read-only file using sudo, taken from Tassilo's Blog
(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

(defun th-refind-file-sudo ()
  (interactive)
  (th-find-file-sudo (buffer-file-name)))

(global-set-key (kbd "M-r") 'th-refind-file-sudo)

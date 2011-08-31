;; use emacsclient as the default editors for most programs
(setenv "EDITOR" "emacsclient")

;; a bigger kill-ring buffer
(setq kill-ring-max 200)

;; fill-column
(set-default 'fill-column 80)

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
(global-set-key (kbd "M-r") 'th-refind-file-sudo)
(global-set-key (kbd "C-x <") 'abaw-scroll-to-current-column)
(global-set-key (kbd "M-F") 'forward-part-in-word)
(global-set-key (kbd "M-B") 'backward-part-in-word)
(global-set-key (kbd "C-x F") 'find-file-at-point)
(define-key universal-argument-map [?c] 'my-column-argument)

;; categories of buffers
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


;; set title for xterm/putty, the emacs hangs when call xterm-title-mode
;; (when (and (not window-system)
;; 	   (string-match "^xterm" (getenv "TERM")))
;;   (if (abaw-try-to-require 'xterm-title)
;;       (xterm-title-mode 1)))

;; utf-8 is good
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; wrap long lines
(set-default 'truncate-lines nil)
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

;; install the color theme for emacs when we got a window system
(when (and (abaw-try-to-require 'color-theme) window-system)
 (color-theme-jedit-grey))

;; using cdargs - this is too bad, cause org-export not to work
;; (abaw-try-to-require 'cdargs)

(when window-system
  (setq server-raise-frame t))

;; start the server
(server-start)

;; put emacs backup files all together

(lexical-let ((backup-dir "~/.emacs_backups/"))
  (mkdir backup-dir t)
  (setq make-backup-file-name-function
	(lambda (file) (concat backup-dir (file-name-nondirectory file) "~"))))

(when (abaw-try-to-require 'paredit)
  (add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
  (add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
  (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
  (add-hook 'minibuffer-setup-hook (lambda ()
				     (if (eq this-command 'eval-expression)
					 (paredit-mode 1)))))

(abaw-try-to-require 'moccur-edit)
(when (abaw-try-to-require 'iedit)
  (global-set-key (kbd "C-c ;") 'iedit-mode))


;; customize mode-line, not yet decided to use this
(defun set-my-mode-line ()
  ;; modifified from emacs-fu's post
  (setq mode-line-format
	(list
	 ;; encoding
	 mode-line-mule-info
	 ;; End-of-line info
	 mode-line-client
	 mode-line-modified
	 ;; buffer name and file name
	 '(:eval (propertize " %b "
			     'face 'font-lock-keyword-face
			     'help-echo (buffer-file-name)))

	 ;; line/column number and relative position
	 mode-line-position

	 ;; modes
	 "("
	 '(:eval (propertize "%m" 'face 'font-lock-comment-face))
	 minor-mode-alist
	 ")"
	 "%-")))

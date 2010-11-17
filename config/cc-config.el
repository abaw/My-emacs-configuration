(add-to-list 'load-path (concat my-emacs-lisp-dir (file-name-as-directory "cc-mode")))
(when (abaw-try-to-require 'ctypes)
  (ctypes-auto-parse-mode 1))

(when (abaw-try-to-require 'flyspell)
  ;; disable binding ctrl-. and ctrl-,
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (mapc #'(lambda (hook) (add-hook hook #'flyspell-prog-mode))
	`(c-mode-hook c++-mode-hook)))

;; c/c++ hook
(defun my-c/c++-hook ()
  (gtags-mode t)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq case-fold-search nil)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'cpp-macro 0)
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'cpp-macro-cont 0)
  (setq c-syntactic-indentation-in-macros nil)
  (when (featurep 'highlight-parentheses) (highlight-parentheses-mode 1)))

;; for QT
;; (setq qt-decl-keywords (list "Q_OBJECT" "Q_CLASSINFO" "Q_PROPERTY" "Q_ENUMS" ))
;; TODO: font-lock for QT
;; (make-face 'qt-keywords-face)
;; (set-face-foreground 'qt-keywords-face "BlueViolet")
;; (dolist (word qt-decl-keywords)
;;   (font-lock-add-keywords 'c++-mode (list (concat "\\<" word "\\>")) 'qt-keywords-face))

(defun qt-c++-hook ()
  (add-to-list 'c-macro-names-with-semicolon "Q_CLASSINFO")
  (c-make-macro-with-semi-re))

(add-hook 'c-mode-hook 'my-c/c++-hook)
(add-hook 'c++-mode-hook 'my-c/c++-hook)
(add-hook 'c++-mode-hook 'qt-c++-hook)

;;; styles
(c-add-style "OCS"
	     '("gnu"
	       (c-basic-offset . 4)
	       (c-offsets-alist
		(brace-list-open . 0)
		(substatement-open . 0)
		(case-label . +)
		(inextern-lang . 0)
		(label . +)
		(cpp-macro . 0))))
(c-add-style "UBOOT"
	     '("OCS"))

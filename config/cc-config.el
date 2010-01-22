;; c/c++ hook
(defun my-c/c++-hook ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq case-fold-search nil)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'cpp-macro 0)
  (c-set-offset 'brace-list-open 0)
  (when (featurep 'highlight-parentheses) (highlight-parentheses-mode 1)))

;; for QT
(setq qt-decl-keywords (list "Q_OBJECT" "Q_CLASSINFO" "Q_PROPERTY" "Q_ENUMS" ))
;; TODO: font-lock for QT
;; (make-face 'qt-keywords-face)
;; (set-face-foreground 'qt-keywords-face "BlueViolet")
;; (dolist (word qt-decl-keywords)
;;   (font-lock-add-keywords 'c++-mode (list (concat "\\<" word "\\>")) 'qt-keywords-face))

(defun qt-c++-hook ()
  (when (functionp 'abaw-at-vsemi-for-decl-macro-p)
    (setq c-at-vsemi-p-fn 'abaw-at-vsemi-for-decl-macro-p)
    (setq abaw-class-decl-macro-list qt-decl-keywords)))

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


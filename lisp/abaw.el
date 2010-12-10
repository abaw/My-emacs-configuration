;; This file contains some enhancements I writing or collected from the internet.
(defun abaw-query-replace ()
  "do query-replace without losing marked region"
  (interactive)
  (let ((deactivate-mark nil))
    (save-excursion (call-interactively 'query-replace))))

(defun abaw-indent ()
  "indent this line or the region"
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'indent-region)
      (call-interactively 'indent-according-to-mode)))
(defun abaw-newline-and-indent ()
  "insert newline at the end of line and indent"
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun chomp (string)
  "Perform a perl-like chomp"
  (let ((s string)) (if (string-match "\\(.*\\)\n" s) (match-string 1 s) s)))

(defun abaw-try-to-require (feature &optional filename)
  "Try to require a feature, if filename is omitted, the filenameis feature.el."
  (let ((require-ok (require feature filename t)))
    (if require-ok
	require-ok (progn (message "'%s is not loaded" (symbol-name feature)) nil))))

(setq abaw-class-decl-macro-list nil)
(make-variable-buffer-local 'abaw-class-decl-macro-list)

(defun abaw-at-vsemi-for-decl-macro-p (&optional pos)
  (when abaw-class-decl-macro-list
    (save-excursion
      (when pos
	(goto-char pos))
      (when (eq (char-after) ?\))
	(forward-char)
	(c-backward-sexp))
      (back-to-indentation)
      (looking-at (c-make-keywords-re nil abaw-class-decl-macro-list)))))

(defun abaw-c-get-syntax ()
  "get syntax information."
  (if (boundp 'c-syntactic-context)
      ;; Use `c-syntactic-context' in the same way as
      ;; `c-indent-line', to be consistent.
      c-syntactic-context
    (c-save-buffer-state nil
      (c-guess-basic-syntax))))

(defun abaw-c-in-class ()
  "Return non-nil if we are in a class definition now."
  (remove-if-not (lambda (item)
		   (eq 'inclass (first item)))
		 (abaw-c-get-syntax)))

(defun abaw-locals-refresh ()
  "refresh local variables for current buffer"
  (interactive)
  (let ((dir-class (dir-locals-find-file (buffer-file-name))))
    (when (consp dir-class)
      (setq dir-locals-directory-alist
	    (remassoc  (car dir-class)
		       dir-locals-directory-alist))
      (hack-local-variables))))

(defun comint-run-command (command)
  "Run COMMAND in a Comint buffer and switch to it.
The buffer name is made by surrounding the program file name of
COMMAND with `*'s.  See `make-comint' and `comint-exec'."
  (interactive "sRun command: ")
  (let* ((arg-list (split-string command))
	 (name (file-name-nondirectory (car arg-list))))
    (switch-to-buffer (apply #'make-comint (append (list name (car arg-list) nil) (cdr arg-list))))))

(defun abaw-scroll-to-current-column ()
  "This command scroll horizontally to let current column become
the leftmost column on screen."
  (interactive)
  (scroll-left (- (current-column) (window-hscroll)) t))

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

(defun my-universal-argument ()
  "Allow to use more other values for prefix argument:"
  (interactive)
  (message "%s" unread-command-events)
  (let ((event (read-event "press c(column),- or numbers" nil 5)))
    (if (and (not (event-modifiers event))
	     (eq (event-basic-type event) ?c))
	(setf prefix-arg (- (current-column) (window-hscroll)))
      (progn
	(setq unread-command-events (cons event unread-command-events))
	(universal-argument)))))

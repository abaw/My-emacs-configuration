;; This file contains some enhancements I writing or collected from the internet.
(require 'cl)
(defmacro aif (condition then &optional else)
  "bind the result of CONDITION to it in THEN and ELSE."
  `(let ((it ,condition))
     (if it
	 ,then
       ,else)))

(defmacro awhen (condition &rest body)
  `(aif ,condition
	(progn ,@body)))

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
             (buffer-name))
     t)))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

(defun th-refind-file-sudo ()
  (interactive)
  (th-find-file-sudo (buffer-file-name)))

(defun my-column-argument (arg)
  "Used to set prefix argument to current column number"
  (interactive "P")
  (when (and (not (event-modifiers last-command-event))
	     (eq (event-basic-type last-command-event) ?c))
    (setf prefix-arg (- (current-column) (window-hscroll))))
  (setq universal-argument-num-events (length (this-command-keys)))
  (ensure-overriding-map-is-bound))

(defun abaw-path (components &optional is-directory)
  "concatenating all the given components to a file name. If
  IS-DIRECTORY is non-nil, the resulting filename is a directory
  instead of a filename."
  (when components
    (if (cdr components)
	(concat (file-name-as-directory (car components))
	       (abaw-path (cdr components)
			   is-directory))
      (if is-directory
	  (file-name-as-directory (car components))
	(car components)))))

(defun abaw-list-all-files-in (directory &optional recursive regexp)
  "list all files in DIRECTORY. Recursively listing files if
RECURSIVE is non-nil. Only files whose names match REGEXP will be
returned if REGEXP is non-nil. Note symlinks are not followed."
  (loop for file in (ignore-errors (directory-files-and-attributes directory t))
	when (and (null (second file)) (or (null regexp) (string-match regexp (car file))))
	collect (car file)
	when (and recursive (eq (second file) t) (not (string-match "\\.\\.?$" (car file))))
	append (abaw-list-all-files-in (car file) t regexp)))

(defun abaw-find-all-files-in (directory &optional recursive regexp)
  "find all files inside DIRECTORY. if RECURSIVE is non-nil,
  finding is recursive. if REGEXP is non-nil, only files whose
  name match REGEXP are opened."
  (interactive (list (read-directory-name "Directory")
		     (read-minibuffer "Recursive?" nil)
		     (read-regexp "Regex matcher:" nil)))
  (let ((files (abaw-list-all-files-in directory recursive regexp)))
    (mapc 'find-file files)
    (message "opened %d files" (length files))))

(defun c++-inclass-name (&optional pos)
  "return the class name at POS if POS is inside a class
  definition. if POS is nil, current point is position used."
  (flet ((get-syntax ()
		     (if (boundp 'c-syntactic-context) ;; this is copied from cc-cmds.el
			 ;; Use `c-syntactic-context' in the same way as
			 ;; `c-indent-line', to be consistent.
			 c-syntactic-context
		       (c-save-buffer-state nil
			 (c-guess-basic-syntax)))))
    (let ((syntax (get-syntax)))
      (when (eq (caar syntax) 'inclass)
	(save-excursion
	  (let* ((in-pos (cadar syntax)) ;; { pos
		 (syntax (progn (goto-char in-pos) (get-syntax)))
		 (open-class-pos ;; class XXX or template <YYY> classs XXX pos
		  (when (eq (caar syntax) 'class-open) (cadar syntax)))
		 (class-name-string (c-syntactic-content open-class-pos in-pos t)))
	    (when (string-match "class \\(\\w+\\)" class-name-string)
	      (match-string 1 class-name-string))))))))

(defun c++-todef (begin end)
  "convert function declrations between BEGIN and END to empty
definitions and store them in the kill ring for pasting."
  (interactive "r")
  (let ((decl (buffer-substring-no-properties begin end))
	(class-name (save-excursion (goto-char begin) (c++-inclass-name))))
    (with-temp-buffer
      (c++-mode)
      (insert decl)
      (goto-char (point-min))
      (kill-comment t)
      (loop for point = (point)
	    do (kill-comment t)
	    until (= point (point)))

      (goto-char (point-min))
      (flet ((next-semicolmn ()
			     (c-syntactic-re-search-forward ";" nil t))
	     (clear-=0 ()
		       (let ((orig-point (point)))
			 (re-search-backward ")")
			 (when (search-forward "=" orig-point t)
			   (backward-char)
			   (kill-word 1))
			 (search-forward ";")))
	     (clear-default-arguments ()
				      (save-restriction
				      	(destructuring-bind (start . end) (bounds-of-thing-at-point 'sexp)
				      	  (narrow-to-region start end))
				      	(goto-char (point-min))
				      	(while (search-forward "=" nil t)
				      	  (backward-char)
				      	  (kill-word 1))
					(goto-char (point-min))))
	     (semicolmn-to-{} ()
			      (c-electric-backspace nil)
			      (newline)
			      (insert "{\n}" ))
	     (back-to-arg-left-paren ()
				      (while (not (looking-at "("))
					(backward-sexp)))
	     (beginning-of-fn-name ()
					 (backward-sexp)
					 (when (looking-back "~")
					   (backward-char)))
	     (clear-unused-qualifer-before-fn-name ()
						   (when (looking-at "\\(virtual\\|explicit\\)")
						     (kill-word 1)
						     (delete-horizontal-space)))
	     (convert-one-declaration ()
				      (clear-=0)
				      (semicolmn-to-{})
				      (back-to-arg-left-paren)
				      (clear-default-arguments)
				      (beginning-of-fn-name)
				      (when class-name
					(insert (concat class-name "::")))
				      (c-beginning-of-defun)
				      (clear-unused-qualifer-before-fn-name)))

	(loop for start-point = (point) then (point)
	      while (next-semicolmn)
	      do (progn
		   (convert-one-declaration)
		   (kill-region start-point (point))
		   (c-end-of-defun)))
	(kill-ring-save (point-min)
			(point-max))))))


(defun forward-part-in-word ()
  "forward one part in word like \"ThisIsAWord\""
  (interactive)
  (when (looking-at "[[:blank:]]")
    (forward-whitespace 1))
  (if (re-search-forward ".[A-Z]" (cdr (bounds-of-thing-at-point 'word)) t)
      (backward-char)
    (forward-word)))

(defun backward-part-in-word ()
  "backward one part in word like \"ThisIsAWord\""
  (interactive)
  (unless (re-search-backward "[A-Z]" (car (bounds-of-thing-at-point 'word))
			   t)
      (backward-word)))

(defun edit-region (start end mode)
  "edit selected text in different major mode"
  (interactive "r\nCMajor mode:")
  (unless (equal (substring (format "%s" mode) -5) "-mode")
    (error "%s is not a major mode" mode))

  (let ((edit-buffer ))
    (pop-to-buffer (make-indirect-buffer (current-buffer)
					 (generate-new-buffer-name (concat (buffer-name) "-region"))))
    (call-interactively mode)
    (narrow-to-region start end)
    (local-set-key (kbd "C-c C-c")
		   (lambda ()
		     (interactive)
		     (let ((base-buffer (buffer-base-buffer (current-buffer))))
		       (kill-buffer)
		       (switch-to-buffer base-buffer))))))

(defun sticky-window-mode-update ()
  "do `set-window-dedicated-p' according to current sticky-window-mode."

  ;; set the window for the buffer for the first time
  (unless (local-variable-p 'sticky-window-mode-window)
    (make-local-variable 'sticky-window-mode-window)
    (setq sticky-window-mode-window (get-buffer-window)))

  ;; update the window if the origianl window was gone
  (unless (and (window-live-p sticky-window-mode-window)
	       (eq (window-buffer sticky-window-mode-window)
		   (current-buffer)))
    (setq sticky-window-mode-window (get-buffer-window)))

  (set-window-dedicated-p sticky-window-mode-window sticky-window-mode))

(define-minor-mode sticky-window-mode
  "when enabled, this window is dedicated for its current buffer."
  :lighter "*S*"
  (flet ()
    (if sticky-window-mode
	(add-hook 'window-configuration-change-hook 'sticky-window-mode-update t)
      (remove-hook 'window-configuration-change-hook 'sticky-window-mode-update t))
    (sticky-window-mode-update)))


(defun select-info-frame (name)
  "select an info frame named NAME. create a frame first if it
  does exist yet. an info frame is a frame with special title
  that make xmonad know how to manage it."
  (let* ((title (concat "abaw:INFO:" name))
	(frame (find title (frame-list) :test 'equal :key (lambda (f) (frame-parameter f 'title)))))
    (select-frame (or frame
		      (make-frame `((title . ,title)))))))

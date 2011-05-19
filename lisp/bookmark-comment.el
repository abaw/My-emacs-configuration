;; This package can be used to add comment overlay for bookmarks

(defun bookmark-for-current-buffer ()
  "return a list of bookmarks for current-buffer"
  (let* ((filename (buffer-file-name))
	 (true-filename (when filename (file-truename filename))))
    (when true-filename (remove-if-not (lambda (bookmark) (equal (file-truename (bookmark-get-filename bookmark)) true-filename)) bookmark-alist))))

(defun bookmark-set-comment (bookmark comment)
  "set COMMENT on the BOOKMARK. BOOKMARK can be the name or the
bookmark item."
  (bookmark-prop-set (if (stringp bookmark)
			 (bookmark-get-bookmark bookmark)
		       bookmark)
		     'comment comment))

(defun bookmark-get-comment (bookmark)
  "get comment of BOOKMARK. BOOKMARK can be the name or the
  bookmark item."
  (bookmark-prop-get (if (stringp bookmark)
			 (bookmark-get-bookmark bookmark)
		       bookmark)
		     'comment))

(provide 'bookmark-comment)

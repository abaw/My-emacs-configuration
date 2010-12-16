(defun maybe-set-header-format ()
  (when (buffer-file-name)
    (let* ((my-name (file-name-nondirectory (buffer-file-name)))
	   (buffers-with-duplicated-names
	    (loop for buf in (buffer-list)
		  when (and (buffer-file-name buf)
			    (equal my-name (file-name-nondirectory (buffer-file-name buf))))
		  collect buf)))
      (when (cdr buffers-with-duplicated-names)
	;; more than once, set header format for these buffers
	(loop for buf in buffers-with-duplicated-names
	      do (with-current-buffer buf
		   (setq header-line-format "%f")))))))

(add-hook 'find-file-hook 'maybe-set-header-format)
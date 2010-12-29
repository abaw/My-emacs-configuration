;; temporary mark
(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register.
Use ska-jump-to-register to jump back to the stored
position."
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8))

(defun ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
        (jump-to-register 8)
        (set-register 8 tmp)))

;; goto char
(defun wy-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `wy-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (let ((event (read-event)))
	   (and (characterp event)
		(char-equal event char)))
        (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))

;; switch to or create scratch buffer
(defvar ywb-scratch-buffer "*scratch*")
(defun ywb-create/switch-scratch (arg)
  (interactive "P")
  (when arg
    (setq ywb-scratch-buffer (read-buffer "Set scratch to: " (buffer-name))))
  (let ((buf (get-buffer ywb-scratch-buffer)))
    (if (null buf)
        (progn
          (switch-to-buffer ywb-scratch-buffer)
          (lisp-interaction-mode)
	  (insert initial-scratch-message))
      (switch-to-buffer ywb-scratch-buffer))))

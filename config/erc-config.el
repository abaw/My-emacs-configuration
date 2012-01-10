;; erc related config
(defun abaw-im ()
  "connect to bitlbee"
  (interactive)
  (erc :server "localhost" :nick "abaw"))

(defun abaw-skype-here ()
  "set current buffer dedicated for skype"
  (interactive)
  (let ((buffer (current-buffer)))
    (select-info-frame "skype")
    (switch-to-buffer buffer)
    (sticky-window-mode t)))

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "333" "353"))
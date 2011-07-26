(require 'emms-setup)
(emms-standard)
(emms-default-players)
(setq emms-repeat-playlist t)

;;; Add music file to playlist on '!', --lgfang
(add-to-list 'dired-guess-shell-alist-user
             (list "\\.\\(flac\\|mp3\\|ogg\\|wav\\)\\'"
                   '(if (y-or-n-p "Add to emms playlist?")
                        (progn (emms-add-file (dired-get-filename))
                               (keyboard-quit))
                      "mplayer")))

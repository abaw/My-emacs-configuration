(require 'doremi-cmd)
(defmacro doremi-key (key)
  `(kbd ,(concat "C-x t " key)))

(when (abaw-try-to-require 'windmove)
  (windmove-default-keybindings 'super))

(global-set-key (doremi-key "r") 'doremi-window-width)

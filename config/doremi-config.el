(require 'doremi-cmd)
(defmacro doremi-key (key)
  `(kbd ,(concat "C-x t " key)))

;; FIXME: there are a lot of duplicated codes
(defun windmove-interactively (key)
  "interactively move between windows"
  (interactive "kUsing arrow keys to move cursor between windows")
  (cond
   ((equal key [left]) (windmove-left) (call-interactively 'windmove-interactively))
   ((equal key [right]) (windmove-right) (call-interactively 'windmove-interactively))
   ((equal key [up]) (windmove-up) (call-interactively 'windmove-interactively))
   ((equal key [down]) (windmove-down) (call-interactively 'windmove-interactively))))

(global-set-key (doremi-key "r") 'doremi-window-width)
(global-set-key (doremi-key "<up>") 'windmove-up)
(global-set-key (doremi-key "<down>") 'windmove-down)
(global-set-key (doremi-key "<left>") 'windmove-left)
(global-set-key (doremi-key "<right>") 'windmove-right)

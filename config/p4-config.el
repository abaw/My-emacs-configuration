;; (require 'vc-p4)
;; (setq vc-p4-require-p4config t)

;; use p4.el
(when (abaw-require 'p4)
 (setq p4-follow-symlinks t))

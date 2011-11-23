;; (require 'vc-p4)
;; (setq vc-p4-require-p4config t)

;; use p4.el
(when (abaw-try-to-require 'p4)
 (setq p4-follow-symlinks t)
 (setq p4-use-p4config-exclusively t))

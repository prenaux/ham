(provide 'ni-js2)
(require 'ni-base)

;;;======================================================================
;;; JS2 mode
;;;======================================================================
(autoload 'js2-mode "js2-mode" nil t)
;; semicolons are OPTIONAL in JS --- we dont want/care if they are
;; there or not
(setq js2-strict-missing-semi-warning t)
;; Insert closing (, [, etc... archtung
(setq js2-mirror-mode nil)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(eval-after-load "js2-mode" '(progn (setq-default js2-basic-offset 2)))

(setq-default js2-basic-offset 2)
(setq js-indent-level 2)

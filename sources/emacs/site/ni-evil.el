(provide 'ni-evil)

(add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs/site/evil"))
(require 'evil)
(evil-mode 1)

(setq evil-search-wrap nil)
(setq evil-search-module 'evil-search)
(evil-select-search-module 'evil-search-module 'evil-search)

(GNUEmacsMin28
  (evil-set-undo-system 'undo-redo))

;;=================================================================
;; evil-owl (registers preview)
;;=================================================================
(require 'evil-owl)
(setq evil-owl-max-string-length 500)
(add-to-list 'display-buffer-alist
  '("*evil-owl*"
     (display-buffer-in-side-window)
     (side . bottom)
     (window-height . 0.3)))
(evil-owl-mode)
(setq evil-owl-idle-delay 0.3)
(diminish 'evil-owl-mode)

(provide 'ni-autocomplete-fancy-dabbrev)

(require 'fancy-dabbrev)
(require 'diminish)
(diminish 'fancy-dabbrev-mode)

;; Enable fancy-dabbrev previews everywhere.
(global-fancy-dabbrev-mode)

;; (setq dabbrev-case-distinction t)
;; (setq dabbrev-case-fold-search t)
;; (setq dabbrev-case-replace nil)

;; (setq fancy-dabbrev-preview-delay nil)
(setq fancy-dabbrev-expansion-on-preview-only t)

(provide 'ni-autocomplete-pabbrev)

(require 'pabbrev)
(require 'diminish)

(global-pabbrev-mode)
(setq pabbrev-idle-timer-verbose nil)
(global-set-key (kbd "C-/") 'make-agl-expand)
(diminish 'pabbrev-mode)

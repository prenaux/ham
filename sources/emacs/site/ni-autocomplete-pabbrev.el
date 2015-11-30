(provide 'ni-autocomplete-pabbrev)

(require 'pabbrev)
(require 'diminish)

(global-pabbrev-mode)
(setq pabbrev-idle-timer-verbose nil)
(diminish 'pabbrev-mode)

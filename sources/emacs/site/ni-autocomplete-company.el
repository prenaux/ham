(provide 'ni-autocomplete-company)
(require 'diminish)

(add-to-list 'load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/company-mode"))
(require 'company)
(require 'company-gtags)
(require 'company-elisp)
(require 'company-ni-idl)

(setq company-idle-delay-default nil) ;; setup company to show only when Ctrl+/ is pressed
;; (setq company-idle-delay-default 0.15)  ;; setup company mode to show up instantly
(setq company-idle-delay company-idle-delay-default)

;; This fucks up the text's upper/lower case by default ???
;; Without this set to nil the symbols are lower-cased !?!?
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)

(defun ni-company-complete ()
  ""
  (interactive)
  (company-mode-on)
  (company-complete))

;; Keyboard shortcuts here since this is still wip
(global-set-key (kbd "C-?") 'ni-company-complete)
(global-set-key (kbd "C-/") 'ni-company-complete)

(diminish 'company-mode)

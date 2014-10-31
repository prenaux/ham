(provide 'ni-autocomplete-company)
(require 'diminish)

(GNUEmacs24
 (add-to-list 'load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/company-mode"))
 (require 'company)
 (require 'company-ni-idl)

 (define-global-minor-mode my-global-company-mode company-mode
   (lambda ()
     (when (not (memq major-mode
                      (list
                       ;; modes for which company mode isn't enabled
                       'slime-repl-mode
                       'shell-mode
                       'ham-shell-mode
                      )))
       (setq company-backends '(company-elisp
                                company-ni-idl
                               ))
       (company-mode-on))
   ))
 (add-hook 'after-init-hook 'my-global-company-mode) ;; enable globaly

 (setq company-idle-delay-default nil) ;; setup company to show only when Ctrl+/ is pressed
 ;; (setq company-idle-delay-default 0.15)  ;; setup company mode to show up instantly

 (setq company-idle-delay company-idle-delay-default)
 (setq company-minimum-prefix-length 2)
 ;; LOL, seriously what's the point of fucking up the text's upper/lower case by default ???
 ;; Without this set to nil the symbols are lower-cased !?!?
 (setq company-dabbrev-downcase nil)
 (setq company-dabbrev-ignore-case nil)

 (setq company-require-match nil)
 (setq company-transformers '(company-sort-by-occurrence))

 (setq company-show-numbers nil)
 (setq company-tooltip-limit 30)
 (setq company-tooltip-align-annotations t)

 (defun my-auto-complete-off()
   (setq company-idle-delay nil))
 (defun my-auto-complete-on()
   (setq company-idle-delay company-idle-delay-default))

 (add-hook 'mark-multiple-enabled-hook 'my-auto-complete-off)
 (add-hook 'mark-multiple-disabled-hook 'my-auto-complete-on)

 ;; (global-set-key (kbd "C-/") 'company-complete-common)
 (global-set-key (kbd "C-/") 'company-ni-idl-complete)

 (diminish 'company-mode)
)

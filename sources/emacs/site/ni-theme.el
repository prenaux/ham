;;
;; Emacs theme management & OS dark mode handling.
;;
;; Usage:
;;   (ni-dark-mode-update-theme)
;;   (ni-dark-mode-enable-update-theme-polling)
;;
;; Choose the theme to use, the default:
;;   (setq ni-theme-light-mode 'flatui)
;;   (setq ni-theme-light-terminal-mode 'solarized-light)
;;   (setq ni-theme-dark-mode 'vscode-dark-plus)
;;   (setq ni-theme-dark-terminal-mode 'solarized-dark)
;;
(provide 'ni-theme)
(require 'ni-theme-solarized)

(set '--dark-mode-state "initial")
(add-to-list 'custom-theme-load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/themes"))

;;
;; Default theme for light & dark mode in gui & terminal.
;;
;; Choose one of the theme listed in ni-theme-disable-all so that they are
;; disabled cleanly when switching between them, you want that to avoid any
;; color issue.
;;
(defvar ni-theme-light-mode 'flatui)
(defvar ni-theme-light-terminal-mode 'solarized-light)
(defvar ni-theme-dark-mode 'vscode-dark-plus)
(defvar ni-theme-dark-terminal-mode 'solarized-dark)

(defun ni-theme-disable-all ()
  ;; light themes
  (disable-theme 'solarized-light)
  (disable-theme 'flatui)
  (disable-theme 'leuven)
  ;; dark themes
  (disable-theme 'solarized-dark)
  (disable-theme 'wombat)
  (disable-theme 'vscode-dark-plus))

;;
;; (ni-theme-load 'wombat nil)
;;
;; Pass `nil` to revert to the default emacs theme.
;;
(defun ni-theme-load (aGuiTheme aTerminalTheme)
  (ni-theme-disable-all)
  (IsNotTerminal
   (if aGuiTheme (load-theme aGuiTheme t)))
  (IsTerminal
   (if aTerminalTheme (load-theme aTerminalTheme t)))
  (set-cursor-color "#cc0000"))

(defun ni-theme-set-light-mode ()
  (interactive)
  (set '--dark-mode-state "light")
  (ni-theme-load
   ni-theme-light-mode
   ni-theme-light-terminal-mode)
  (message "Changed to light mode"))

(defun ni-theme-set-dark-mode ()
  (interactive)
  (set '--dark-mode-state "dark")
  (ni-theme-load
   ni-theme-dark-mode
   ni-theme-dark-terminal-mode)
  (message "Changed to dark mode"))

(defun ni-dark-mode-update-theme ()
  (interactive)
  (let ((is-interactive (if (called-interactively-p 'any)
                            t nil)))
    (ni-check-dark-mode
     (lambda ()
       (if (not (string= --dark-mode-state "dark"))
           (progn (ni-theme-set-dark-mode))
         (when is-interactive
           (message "Is already dark mode"))))
     (lambda ()
       (if (not (string= --dark-mode-state "light"))
           (progn (ni-theme-set-light-mode))
         (when is-interactive
           (message "Is already light mode"))))
     )))

(defconst ni-dark-mode-auto-update-idle-interval 2
  "The number of seconds between which to poll for dark mode state. Emacs must be restarted for this value to take effect")

;; timer-list
;; timer-idle-list
(defun ni-dark-mode-disable-update-theme-polling ()
  ;;
  ;; Function: cancel-function-timers
  ;;
  ;; Cancel all timers which would run FUNCTION.
  ;; This affects ordinary timers such as are scheduled by `run-at-time',
  ;; and idle timers such as are scheduled by `run-with-idle-timer'.
  ;;
  (cancel-function-timers 'ni-dark-mode-update-theme))

(defun ni-dark-mode-enable-update-theme-polling ()
  (interactive)
  (ni-dark-mode-disable-update-theme-polling)
  (run-with-idle-timer ni-dark-mode-auto-update-idle-interval t 'ni-dark-mode-update-theme))

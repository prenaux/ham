;;
;; Usage, in your .emacs:
;;  (setenv "HAM_HOME" "/path/to/ham/root/")
;;  (add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs"))
;;  (require 'ham-setup)
;;
(provide 'ham-setup)

;;;======================================================================
;;; Environment setup
;;;======================================================================
(defun ham-getenv (str)
  (let ((envValue (getenv str)))
    (if envValue
        (replace-regexp-in-string "\\\\" "/" envValue)
      nil)))

(if (not (getenv "HAM_HOME"))
    (setenv "HAM_HOME" (concat (ham-getenv "WORK") "/ham")))

(defconst HAM_HOME (ham-getenv "HAM_HOME"))

;;;======================================================================
;;; Ham shell
;;;======================================================================
(require 'ham-shell)

(Windows
 (setq explicit-ham-shell-file-name (concat HAM_HOME "/bin/nt-x86/bash.exe")))
(Linux
 (setq explicit-ham-shell-file-name "/bin/bash"))
(OSX
 (setq explicit-ham-shell-file-name "/bin/bash"))

;; (setq explicit-bash-ham-startfile "")

(setq explicit-bash-ham-args
      (list
       "--rcfile"
       (concat HAM_HOME "/bin/ham-bash-start.sh")
       "-i"))

;; (setq shell-file-name explicit-ham-shell-file-name)

(add-hook 'comint-output-filter-functions 'ham-shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t)

(defun ham-previous-input ()
  "Console previous input"
  (interactive)
  (goto-char (point-max))
  (comint-previous-input 1))

(defun ham-next-input ()
  "Console previous input"
  (interactive)
  (goto-char (point-max))
  (comint-next-input 1))

(add-hook 'ham-shell-mode-hook 'n-ham-shell-mode-hook)
(defun n-ham-shell-mode-hook ()
  "shell mode customizations."
  (local-set-key "\M-p" 'ham-previous-input)
  (local-set-key "\M-n" 'ham-next-input)
  (local-set-key "\C-cl" 'ham-ido-shell)
  (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
  (setq comint-input-sender 'n-ham-shell-simple-send)
  )

(defun n-ham-shell-simple-send (proc command)
  "Various commands pre-processing before sending to shell."
  (cond
   ;; Checking for clear command and execute it.
   ((string-match "^[ \t]*clear[ \t]*$" command)
    (comint-send-string proc "\n")
    (erase-buffer)
    )
   ;; Checking for clear command and execute it.
   ((string-match "^[ \t]*clear[        ;& \t]*.*$" command)
    (erase-buffer)
    (comint-simple-send proc command))
   ;; Checking for man command and execute it.
   ((string-match "^[ \t]*man[ \t]*" command)
    (comint-send-string proc "\n")
    (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
    (setq command (replace-regexp-in-string "[ \t]+$" "" command))
    ;;(message (format "command %s command" command))
    (funcall 'man command)
    )
   ;; Send other commands to the default handler.
   (t (comint-simple-send proc command))
   )
  )

(defun ham-shell-unique ()
  (interactive)
  (ham-shell)
  (rename-uniquely))

;;;======================================================================
;;; Hamfile mode
;;;======================================================================
(autoload 'ham-mode "hamfile" nil t)

(add-to-list 'auto-mode-alist '("\\.ham\\'" . ham-mode))
(add-to-list 'auto-mode-alist '("\\Jamfile\\'" . ham-mode))
(add-to-list 'auto-mode-alist '("\\Jamrules\\'" . ham-mode))
(add-to-list 'auto-mode-alist '("\\Jambase\\'" . ham-mode))
(add-to-list 'auto-mode-alist '("\\Hamfile\\'" . ham-mode))
(add-to-list 'auto-mode-alist '("\\Hamrules\\'" . ham-mode))
(add-to-list 'auto-mode-alist '("\\Hambase\\'" . ham-mode))

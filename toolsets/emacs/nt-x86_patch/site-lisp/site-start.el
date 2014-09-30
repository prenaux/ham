;; Setup the DEVENV environment variable
(if (not (getenv "EMACS_DEVENV"))
    (setenv "EMACS_DEVENV" (expand-file-name (concat exec-directory "../../../.."))))

(add-to-list 'load-path (concat (getenv "EMACS_DEVENV") "/sources/emacs"))
(add-to-list 'load-path (concat (getenv "EMACS_DEVENV") "/sources/emacs/site"))

;; Cause the HOME env-var isn't set on Windows and Emacs defaults to AppData/Roaming...
(setenv "HOME" (getenv "USERPROFILE"))

(if
    (and (not (file-exists-p (concat (getenv "HOME") "/.emacs")))
		 (not (file-exists-p (concat (getenv "HOME") "/.emacs.el")))
		 (not (file-exists-p (concat (getenv "HOME") "/.emacs.d/init.el"))))
	(let ((default-dot-emacs (concat (getenv "EMACS_DEVENV") "/sources/emacs/site/default-dot-emacs.el")))
      (message
       (concat "No ~/.emacs loading default one at '" default-dot-emacs "'"))
      (load-file default-dot-emacs)
      )
  )

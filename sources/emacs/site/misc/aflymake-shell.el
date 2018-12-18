;;; Flymake Shell mode
;; You may want to invoke aflymake-shell when sh-mode loads.

;; (require 'aflymake-shell)
;; (add-hook 'sh-mode-hook 'aflymake-shell-load)

(require 'aflymake)

(defcustom aflymake-shell-of-choice
  "bash"
  "Path of shell.")

(defcustom aflymake-shell-arguments
  (list "-n")
  "Shell arguments to invoke syntax checking.")

(defconst aflymake-allowed-shell-file-name-masks '(
     ("\\.sh$" aflymake-shell-init)
     ("\\.$" aflymake-shell-init))
  "Filename extensions that switch on aflymake-shell mode syntax checks.")

(defcustom aflymake-shell-err-line-pattern-re
  '(("^\\(.+\\): line \\([0-9]+\\): \\(.+\\)$" 1 2 nil 3))
  "Regexp matching JavaScript error messages.")

(defun aflymake-shell-init ()
  (let* ((temp-file (aflymake-init-create-temp-buffer-copy
                     'aflymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list aflymake-shell-of-choice (append aflymake-shell-arguments (list local-file)))))

(defun aflymake-shell-load ()
  (setq aflymake-allowed-file-name-masks
        (append aflymake-allowed-file-name-masks aflymake-allowed-shell-file-name-masks))
  (setq aflymake-err-line-patterns
        (append aflymake-err-line-patterns aflymake-shell-err-line-pattern-re))
  (aflymake-mode t)
  (local-set-key (kbd "C-c d") 'aflymake-display-err-menu-for-current-line))

(provide 'aflymake-shell)

;;; aflymake-shell.el ends here

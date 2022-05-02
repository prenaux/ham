(provide 'ni-flymake)

(require 'ni-base)
(require 'aflymake)
(require 'aflymake-cursor)

; Set flymake to start only when saving the buffer
(setq aflymake-no-changes-timeout 999999
      aflymake-start-syntax-check-on-newline nil)

; Error pattern matching :
;   regexp file-idx line-idx col-idx (optional) text-idx(optional), match-end to end of string is error text

;;**********************************************************************
;; Flymake - niScript
;;**********************************************************************
(defconst aflymake-allowed-ni-file-name-masks '(
      ("\\.ni$" aflymake-ni-init)
      ("\\.niw$" aflymake-ni-init)
      ("\\.nip$" aflymake-ni-init))
      "Filename extensions that switch on aflymake-aq mode syntax checks")

(defconst aflymake-ni-err-line-pattern-re
      '("^E/[[:space:]]*\\(.*\\)(\\([0-9]+\\)) : (col \\([0-9]+\\)) \\(.*\\)$" 1 2 3 4)
      "Regexp matching aglScript error messages")

(defun aflymake-ni-init ()
  (let* ((temp-file (aflymake-init-create-temp-buffer-copy
                     'aflymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list (concat (getenv "WORK") "/niLang/bin/ni-flymake") (list "-e" local-file))))

(defun aflymake-ni-load ()
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq aflymake-allowed-file-name-masks (append aflymake-allowed-file-name-masks aflymake-allowed-ni-file-name-masks))
        (setq aflymake-err-line-patterns (cons aflymake-ni-err-line-pattern-re aflymake-err-line-patterns))
        (aflymake-mode t)))
)

(defun aflymake-mode-or-syntax-check ()
  (interactive)
  (if (bound-and-true-p aflymake-mode)
    (aflymake-start-syntax-check)
  (aflymake-mode)))

;; (add-hook 'niscript-mode-hook 'aflymake-ni-load)

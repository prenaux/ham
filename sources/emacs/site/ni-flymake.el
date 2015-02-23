(provide 'ni-flymake)

(require 'ni-base)
(require 'flymake)
(require 'flymake-cursor)

; Set flymake to start only when saving the buffer
(setq flymake-no-changes-timeout 999999
      flymake-start-syntax-check-on-newline nil)

; Error pattern matching :
;   regexp file-idx line-idx col-idx (optional) text-idx(optional), match-end to end of string is error text

;; Flymake, show current error
(global-set-key [f3] 'flymake-goto-prev-error)
(global-set-key [f4] 'flymake-goto-next-error)
(global-set-key [(control f3)] 'flymake-start-syntax-check)
(global-set-key [(control f4)] 'flymake-mode)

;;**********************************************************************
;; Flymake - niScript
;;**********************************************************************
(defconst flymake-allowed-ni-file-name-masks '(
      ("\\.ni$" flymake-ni-init)
      ("\\.niw$" flymake-ni-init)
      ("\\.nip$" flymake-ni-init))
      "Filename extensions that switch on flymake-aq mode syntax checks")

(defconst flymake-ni-err-line-pattern-re
      '("^E/[[:space:]]*\\(.*\\)(\\([0-9]+\\)) : (col \\([0-9]+\\)) \\(.*\\)$" 1 2 3 4)
      "Regexp matching aglScript error messages")

(defun flymake-ni-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list (concat (getenv "WORK") "/niSDK/bin/ni-flymake") (list "-e" local-file))))

(defun flymake-ni-load ()
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-ni-file-name-masks))
        (setq flymake-err-line-patterns (cons flymake-ni-err-line-pattern-re flymake-err-line-patterns))
        (flymake-mode t)))
)

;; (add-hook 'niscript-mode-hook 'flymake-ni-load)

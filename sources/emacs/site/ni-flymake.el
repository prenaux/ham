(provide 'ni-flymake)

(require 'ni-base)
(require 'flymake)

; Set flymake to start only when saving the buffer
(setq flymake-no-changes-timeout 999999
      flymake-start-syntax-check-on-newline nil)

; Error pattern matching :
;   regexp file-idx line-idx col-idx (optional) text-idx(optional), match-end to end of string is error text

(defun ni-flymake-goto-prev-error-disp ()
 "Flymake, goto the previous error and display it in the minibuffer."
  (interactive)
  (flymake-goto-prev-error)
  (ni-flymake-display-err-minibuf)
)

(defun ni-flymake-goto-next-error-disp ()
 "Flymake, goto the next error and display it in the minibuffer."
  (interactive)
  (flymake-goto-next-error)
  (ni-flymake-display-err-minibuf)
)

(defun ni-flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "%s:%s: %s" full-file line text)
          )
        )
      (setq count (1- count)))))

(defun ni-flymake-follow-err ()
  "Open the file/line in which the error is described."
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
           ;;(message "%s:%s: %s" full-file line text)
		   (find-file full-file)
		   (goto-line line)
          )
        )
      (setq count (1- count)))))

(defun ni-flymake-goto-prev-error-disp ()
 "Flymake, goto the previous error and display it in the minibuffer."
  (interactive)
  (flymake-goto-prev-error)
  (ni-flymake-display-err-minibuf)
)

(defun ni-flymake-goto-next-error-disp ()
 "Flymake, goto the next error and display it in the minibuffer."
  (interactive)
  (flymake-goto-next-error)
  (ni-flymake-display-err-minibuf)
)

;; Flymake, show current error
; (global-set-key XXX 'flymake-display-err-menu-for-current-line) ; show in a menu
; (global-set-key XXX 'flymake-display-err-minibuf) ; show in the minibuffer
(global-set-key [f3] 'ni-flymake-goto-prev-error-disp)
(global-set-key [(control f3)] 'flymake-start-syntax-check)
(global-set-key [f4] 'ni-flymake-goto-next-error-disp)
(global-set-key [(control f4)] 'flymake-mode)
(global-set-key [(meta f3)] 'ni-flymake-follow-err)

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
    (list (concat (getenv "WORK") "/niLang/bin/ni-flymake") (list "-e" local-file))))

(defun flymake-ni-load ()
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-ni-file-name-masks))
        (setq flymake-err-line-patterns (cons flymake-ni-err-line-pattern-re flymake-err-line-patterns))
        (flymake-mode t)))
)

(add-hook 'niscript-mode-hook 'flymake-ni-load)

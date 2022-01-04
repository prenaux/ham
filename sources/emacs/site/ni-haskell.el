(provide 'ni-haskell)

(add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs/site/haskell-mode"))
(require 'haskell-mode-autoloads)

(add-to-list 'Info-default-directory-list (concat (getenv "HAM_HOME") "/sources/emacs/site/haskell-mode"))

(custom-set-variables
 '(haskell-process-path-stack (concat (getenv "HAM_HOME") "/toolsets/haskell/hs-stack"))
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-type 'stack-ghci)
 '(haskell-interactive-popup-errors nil))

;;;======================================================================
;;; Indentation
;;;======================================================================

;; Default haskell indent mode, commented because its confusing and hindent
;; takes care of proper indentation
;; (add-hook 'haskell-mode-hook 'haskell-indent-mode)

;; Custom simple indentation
(defun ni-haskell-setup-indentation ()
  "Setup variables for editing Haskell files."
  (setq whitespace-line-column 80)
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list (number-sequence 2 80 2))
  (haskell-indentation-mode 0)
  (setq indent-line-function 'indent-relative))
(add-hook 'haskell-mode-hook 'ni-haskell-setup-indentation)

;; hindent, override the regular indent commands, 'M-Q' to indent a block &
;; 'C-M-\' to indent a region
(require 'hindent)
(setq hindent-process-path (concat (getenv "HAM_HOME") "/toolsets/haskell/hs-indent-file"))
(add-hook 'haskell-mode-hook #'hindent-mode)

;;;======================================================================
;;; Interactive
;;;======================================================================
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(defun ni-haskell-mode-save-buffer-and-load-file ()
  (interactive)
  (save-buffer)
  (haskell-process-load-or-reload)
  (haskell-interactive-bring))

;;;======================================================================
;;; Keymap
;;;======================================================================
(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-,") 'ni-haskell-mode-save-buffer-and-load-file)
   ))

(provide 'ni-haskell)

(add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs/site/haskell-mode"))
(require 'haskell-mode-autoloads)

(Windows
 (add-to-list 'exec-path (concat (getenv "HAM_HOME") "/toolsets/haskell/nt-x86/bin"))
 (setenv "HASKELL_BIN_DIR" (concat (getenv "HAM_HOME") "/toolsets/haskell/nt-x86/bin/"))
)
(OSX
 (add-to-list 'exec-path "/opt/homebrew/bin/")
 (setenv "HASKELL_BIN_DIR" "/opt/homebrew/bin/")
)

(setq ghc-core-program (concat (getenv "HASKELL_BIN_DIR") "ghc"))
(setq haskell-process-path-ghci (concat (getenv "HASKELL_BIN_DIR") "ghci"))
(setq haskell-process-path-cabal (concat (getenv "HASKELL_BIN_DIR") "cabal"))
(setq haskell-process-path-stack (concat (getenv "HASKELL_BIN_DIR") "stack"))
(setq haskell-compile-command (concat "\"" ghc-core-program "\"" " -Wall -ferror-spans -fforce-recomp -c \"%s\""))

(add-to-list 'Info-default-directory-list (concat (getenv "HAM_HOME") "/sources/emacs/site/haskell-mode"))

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))

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

;; hindent
(require 'hindent)
(setq hindent-process-path (concat (getenv "HOME") "/.local/bin/hindent"))
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

(defun ni-haskell-mode-save-buffer-and-compile ()
  (interactive)
  (save-buffer)
  (haskell-compile))

;;;======================================================================
;;; Interactive
;;;======================================================================

(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-\\") 'haskell-indent-insert-guard)
     (define-key haskell-mode-map (kbd "C-c C-,") 'ni-haskell-mode-save-buffer-and-load-file)
     (define-key haskell-mode-map (kbd "C-c C-p") 'ni-haskell-mode-save-buffer-and-compile)
     (define-key haskell-mode-map (kbd "C-c C-y") 'ni-haskell-mode-save-buffer-and-compile)
   ))

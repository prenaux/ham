(provide 'ni-haskell)

(add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs/site/haskell-mode"))
(require 'haskell-mode-autoloads)

(OSX
 (add-to-list 'exec-path (concat (getenv "HAM_HOME") "/toolsets/haskell/osx-x86/ghc.app/Contents/bin"))
 (setenv "HASKELL_BIN_DIR" (concat (getenv "HAM_HOME") "/toolsets/haskell/osx-x86/ghc.app/Contents/bin/"))
)

(setq ghc-core-program (concat (getenv "HASKELL_BIN_DIR") "ghc"))
(setq haskell-process-path-ghci (concat (getenv "HASKELL_BIN_DIR") "ghci"))
(setq haskell-process-path-cabal (concat (getenv "HASKELL_BIN_DIR") "cabal"))
(setq haskell-process-path-stack (concat (getenv "HASKELL_BIN_DIR") "stack"))
(setq haskell-compile-command (concat "\"" ghc-core-program "\"" " -Wall -ferror-spans -fforce-recomp -c \"%s\""))

(add-to-list 'Info-default-directory-list (concat (getenv "HAM_HOME") "/sources/emacs/site/haskell-mode"))

(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(provide 'ni-ham)
(require 'ni-base)

(setenv "HAM_HOME" (concat (agl-getenv "WORK") "/ham"))

(progn
  (Windows
   (setenv "HAM_BIN_LOA" "nt-x86"))
  (OSX
   (if (string-match "^aarch64-.*" system-configuration)
       (setenv "HAM_BIN_LOA" "osx-arm64")
     (setenv "HAM_BIN_LOA" "osx-x64")))
  (Linux
   (setenv "HAM_BIN_LOA" "lin-x64")))

(ni-add-to-PATH-front (concat (getenv "HAM_HOME") "/bin/" (getenv "HAM_BIN_LOA")))
;; add bin last so that wrapper scripts have priority
(ni-add-to-PATH-front (concat (getenv "HAM_HOME") "/bin"))

(add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs"))

(require 'ham-setup)

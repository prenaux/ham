(provide 'ni-ham)
(require 'ni-base)

(setenv "HAM_HOME" (concat (agl-getenv "WORK") "/ham"))

(Windows
 (ni-add-to-PATH-front (concat (getenv "HAM_HOME") "/bin/nt-x86")))
(OSX
 (if (string-match "^aarch64-.*" system-configuration)
     (ni-add-to-PATH-front (concat (getenv "HAM_HOME") "/bin/osx-arm64"))
   (ni-add-to-PATH-front (concat (getenv "HAM_HOME") "/bin/osx-x64"))))
(ni-add-to-PATH-front (concat (getenv "HAM_HOME") "/bin")) ;; add bin last so that wrapper scripts have priority

(add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs"))

(require 'ham-setup)

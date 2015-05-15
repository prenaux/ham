(provide 'ni-ham)
(require 'ni-base)

(setenv "HAM_HOME" (concat (agl-getenv "WORK") "/ham"))
(ni-add-to-PATH-front (concat (getenv "HAM_HOME") "/bin"))
(Windows
 (ni-add-to-PATH-front (concat (getenv "HAM_HOME") "/bin/nt-x86")))
(OSX
 (ni-add-to-PATH-front (concat (getenv "HAM_HOME") "/bin/osx-x86")))

(add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs"))

(require 'ham-setup)

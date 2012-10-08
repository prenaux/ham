(provide 'ni-ham)
(require 'ni-base)

(setenv "HAM_HOME" (concat (agl-getenv "WORK") "/ham"))
(ni-add-to-PATH-front (concat (getenv "HAM_HOME") "/bin"))

(add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs"))

(require 'ham-setup)

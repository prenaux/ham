(provide 'ni-emacs-compat)

;;;======================================================================
;;; Versions
;;;======================================================================
(defmacro Aquamacs (&rest x)
  (list
   'if
   (string-match "Aquamacs" (prin1-to-string (version)))
   (cons 'progn x)))
(defmacro GNUEmacs (&rest x)
  (list
   'if
   (string-match "GNU Emacs" (prin1-to-string (version)))
   (cons 'progn x)))
(defmacro GNUEmacs20 (&rest x)
  (list
   'if
   (string-match "GNU Emacs 20" (prin1-to-string (version)))
   (cons 'progn x)))
(defmacro GNUEmacs21 (&rest x)
  (list
   'if
   (string-match "GNU Emacs 21" (prin1-to-string (version)))
   (cons 'progn x)))
(defmacro GNUEmacs22 (&rest x)
  (list
   'if
   (string-match "GNU Emacs 22" (prin1-to-string (version)))
   (cons 'progn x)))
(defmacro GNUEmacs23 (&rest x)
  (list
   'if
   (string-match "GNU Emacs 23" (prin1-to-string (version)))
   (cons 'progn x)))
(defmacro GNUEmacs24 (&rest x)
  (list
   'if
   (string-match "GNU Emacs 24" (prin1-to-string (version)))
   (cons 'progn x)))
(defmacro GNUEmacs25 (&rest x)
  (list
   'if
   (string-match "GNU Emacs 25" (prin1-to-string (version)))
   (cons 'progn x)))
(defmacro GNUEmacs26 (&rest x)
  (list
   'if
   (string-match "GNU Emacs 26" (prin1-to-string (version)))
   (cons 'progn x)))
(defmacro GNUEmacs28 (&rest x)
  (list
   'if
   (string-match "GNU Emacs 28" (prin1-to-string (version)))
   (cons 'progn x)))
(defmacro XEmacs (&rest x)
  (list
   'if
   (string-match "XEmacs 21" (prin1-to-string (version)))
   (cons 'progn x)))
(defmacro Linux (&rest x)
  (list
   'if
   (string-match "linux" (prin1-to-string system-type))
   (cons 'progn x)))
(defmacro OSX (&rest x)
  (list
   'if
   (string-match "darwin" (prin1-to-string system-type))
   (cons 'progn x)))
(defmacro Windows (&rest x)
  (list
   'if
   (string-match "windows" (prin1-to-string system-type))
   (cons 'progn x)))
(defmacro NotWindows (&rest x)
  (list
   'if
   (not (string-match "windows" (prin1-to-string system-type)))
   (cons 'progn x)))
(defmacro Solaris (&rest x)
  (list
   'if
   (string-match "usg-unix-v" (prin1-to-string system-type))
   (cons 'progn x)))
(defmacro XWindow (&rest x)
  (list
   'if
   (string-match "x" (prin1-to-string window-system))
   (cons 'progn x)))
(defmacro BatchMode (&rest x)
  (list
   'if
   (and ENV_EMACS_BATCHMODE (string-match "1" ENV_EMACS_BATCHMODE))
   (cons 'progn x)))
(defmacro NotBatchMode (&rest x)
  (list
   'if
   (not (and ENV_EMACS_BATCHMODE (string-match "1" ENV_EMACS_BATCHMODE)))
   (cons 'progn x)))

(defmacro IsTerminal (&rest x)
  (list
   'if
   (string-match "nil" (prin1-to-string window-system))
   (cons 'progn x)))

(defmacro IsNotTerminal (&rest x)
  (list
   'if
   (not (string-match "nil" (prin1-to-string window-system)))
   (cons 'progn x)))

(defmacro GNUEmacsMin25 (&rest x)
  (list
   'if (not (version< emacs-version "25.0"))
   (cons 'progn x)))
(defmacro GNUEmacsMin26 (&rest x)
  (list
   'if (not (version< emacs-version "26.0"))
   (cons 'progn x)))

(defmacro DontExecute (&rest x) ())

(defun agl-begin-time-block (name)
  (setq *emacs-time-block-prev-begin* *emacs-time-block-begin*)
  (setq *emacs-time-block-begin* (float-time))
  (NotBatchMode
   (message (concat "[%3.3fs] [%3.3fs] === " name " ===")
            (- (float-time) *emacs-time-start*)
            (- (float-time) *emacs-time-block-prev-begin*))))

;;;======================================================================
;;; Emacs 28 compat
;;;======================================================================

;;
;; Fix 'wrong-number-of-arguments' issue.
;;
;; See:
;; - https://github.com/hlissner/doom-emacs/issues/4534#issuecomment-770340217
;; - https://gist.github.com/hlissner/175c21000114d1dba7a47678191a888a
;;
(GNUEmacs28
 (defmacro define-obsolete-variable-alias (obsolete-name current-name &optional when docstring)
   "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.
WHEN should be a string indicating when the variable was first
made obsolete, for example a date or a release number.
This macro evaluates all its parameters, and both OBSOLETE-NAME
and CURRENT-NAME should be symbols, so a typical usage would look like:
  (define-obsolete-variable-alias 'foo-thing 'bar-thing \"27.1\")
This macro uses `defvaralias' and `make-obsolete-variable' (which see).
See the Info node `(elisp)Variable Aliases' for more details.
If CURRENT-NAME is a defcustom or a defvar (more generally, any variable
where OBSOLETE-NAME may be set, e.g. in an init file, before the
alias is defined), then the define-obsolete-variable-alias
statement should be evaluated before the defcustom, if user
customizations are to be respected.  The simplest way to achieve
this is to place the alias statement before the defcustom (this
is not necessary for aliases that are autoloaded, or in files
dumped with Emacs).  This is so that any user customizations are
applied before the defcustom tries to initialize the
variable (this is due to the way `defvaralias' works).
For the benefit of Customize, if OBSOLETE-NAME has
any of the following properties, they are copied to
CURRENT-NAME, if it does not already have them:
`saved-value', `saved-variable-comment'."
   (declare (doc-string 4)
            (advertised-calling-convention
             (obsolete-name current-name when &optional docstring) "23.1"))
   `(progn
      (defvaralias ,obsolete-name ,current-name ,docstring)
      (dolist (prop '(saved-value saved-variable-comment))
        (and (get ,obsolete-name prop)
             (null (get ,current-name prop))
             (put ,current-name prop (get ,obsolete-name prop))))
      (make-obsolete-variable ,obsolete-name ,current-name ,when)))

 (defmacro define-obsolete-face-alias (obsolete-face current-face &optional when)
   "Make OBSOLETE-FACE a face alias for CURRENT-FACE and mark it obsolete.
WHEN should be a string indicating when the face was first made
obsolete, for example a date or a release number."
   `(progn (put ,obsolete-face 'face-alias ,current-face)
           (put ,obsolete-face 'obsolete-face (or (purecopy ,when) t))))

 (defmacro define-obsolete-function-alias (obsolete-name current-name &optional when docstring)
   "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.
\(define-obsolete-function-alias \\='old-fun \\='new-fun \"22.1\" \"old-fun's doc.\")
is equivalent to the following two lines of code:
\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"22.1\")
WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.
See the docstrings of `defalias' and `make-obsolete' for more details."
   (declare (doc-string 4))
   `(progn (defalias ,obsolete-name ,current-name ,docstring)
           (make-obsolete ,obsolete-name ,current-name ,when)))

 )

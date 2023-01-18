(provide 'ni-base)

;;;======================================================================
;;; Version & Conditional exec macros
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
(defmacro GNUEmacsMin28 (&rest x)
  (list
   'if (not (version< emacs-version "28.1"))
   (cons 'progn x)))

(defmacro DontExecute (&rest x) ())

;;;======================================================================
;;; Emacs devenv
;;;======================================================================
(defun agl-begin-time-block (name)
  (setq *emacs-time-block-prev-begin* *emacs-time-block-begin*)
  (setq *emacs-time-block-begin* (float-time))
  (NotBatchMode
   (message (concat "[%3.3fs] [%3.3fs] === " name " ===")
            (- (float-time) *emacs-time-start*)
            (- (float-time) *emacs-time-block-prev-begin*))))

(defun agl-getenv (str)
  (if (getenv str)
      (replace-regexp-in-string "\\\\" "/" (getenv str))
    ""))

;; If not set "EMACS_DEVENV" is set from HAM_HOME
(if (not (getenv "EMACS_DEVENV"))
    (setenv "EMACS_DEVENV" (getenv "HAM_HOME")))
(defconst EMACS_DEVENV (agl-getenv "EMACS_DEVENV"))
(defconst ENV_DEVENV (agl-getenv "EMACS_DEVENV"))
(defconst ENV_DEVENV_EMACS_SCRIPTS (concat ENV_DEVENV "/sources/emacs/site"))
(defconst ENV_EMACS_BATCHMODE  (agl-getenv "EMACS_BATCHMODE"))
(add-to-list 'load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/misc"))
(require 'emacs-type)
(require 's)

;;;======================================================================
;;; Emacs 24 compat
;;;======================================================================
(if (not (fboundp 'string-empty-p))
 (defalias 'string-empty-p 's-blank-str?))

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

;; If not set "WORK" is the parent folder of EMACS_DEVENV
(if (string-empty-p (getenv "WORK"))
    (setenv "WORK" (expand-file-name (concat EMACS_DEVENV "/.."))))
(defconst ENV_WORK (agl-getenv "WORK"))
(defconst ENV_EMACS_BAK_DIR
  (if (string-empty-p (agl-getenv "EMACS_BAK_DIR"))
    (concat (file-name-as-directory ENV_WORK) "_emacs_bak")
    (agl-getenv "EMACS_BAK_DIR")))

;; Fix a bug with some version of emacs...
(setq warning-suppress-types nil)

(setq *emacs-time-start* (float-time))
(setq *emacs-time-block-begin* *emacs-time-start*)

(defun emacs-is-terminal ()
  "Returns whether emacs runs in a terminal or a window"
  (let ((type (emacs-type)))
    (cond ((or (eq type 'emacs)
               (eq type 'emacs-nt)
               (eq type 'xemacs)
               (eq type 'xemacs-nt)
               (eq type 'emacs-msdos))
           t)
          (nil))))

(defun agl-found-custom-arg (switch)
  (member switch command-line-args))

;;;======================================================================
;;; Utils
;;;======================================================================

;; Flatten a list and remove all the nil elements
(defun agl-list-flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (agl-list-flatten (car mylist)) (agl-list-flatten (cdr mylist))))))

;; Return the default value of search like functions
(defun ni-get-default-search-text ()
  ""
  (substring-no-properties
   (cond
    ;; Marked text
    ((use-region-p)
     (let* ((beg (region-beginning))
            (end (region-end))
            (eol (save-excursion (goto-char beg) (line-end-position))))
       (buffer-substring-no-properties beg (min end eol))))
    ;; None
    (t
     ""))))

;;;======================================================================
;;; OS Environment
;;;======================================================================

(defun ni-print-list (aName aList)
  (message (concat "====== " aName " ======"))
  (dolist (item aList)
    (message item))
  t)

(defun ni-add-to-PATH-front (aValue)
  (setenv
   "PATH"
   (concat aValue ENV_SEP
           (getenv "PATH")))
  (add-to-list 'exec-path aValue))

(defun ni-add-to-PATH-back (aValue)
  (setenv
   "PATH"
   (concat (getenv "PATH") ENV_SEP
           aValue))
  (add-to-list 'exec-path aValue t))

(defun ni-update-exec-path-from-PATH () ""
  (interactive)
  (let ((path-string (getenv "PATH")))
    (dolist (dir (split-string path-string ":"))
      (if (not (member dir exec-path))
        (add-to-list 'exec-path dir))))
  exec-path)

;;
;; (setq exec-path '())
;; (ni-print-list "exec-path" exec-path)
;; (ni-update-exec-path-from-PATH)
;;

(Windows
 (defconst ENV_SEP ";")
 (if (not (getenv "NI_OS"))
     (setenv "NI_OS" "NT"))
 )

(Linux
 (defconst ENV_SEP ":")
 (if (not (getenv "NI_OS"))
     (setenv "NI_OS" "LINUX-X64"))
 )

(OSX
 (defconst ENV_SEP ":")
 (if (not (getenv "NI_OS"))
     (setenv "NI_OS" "OSX"))
 )

;;;======================================================================
;;; Key names for special keys
;;;======================================================================
(agl-begin-time-block "Key names for special keys")
(defvar real-keyboard-keys
  '(("M-<up>"        . "\M-[1;3A")      ;
    ("M-<down>"      . "\M-[1;3B")
    ("M-<right>"     . "\M-[1;3C")
    ("M-<left>"      . "\M-[1;3D")
    ("C-<return>"    . "\C-j")
    ("C-<delete>"    . "\M-[3;5~")
    ("C-<up>"        . "\M-[1;5A")
    ("C-<down>"      . "\M-[1;5B")
    ("C-<right>"     . "\M-[1;5C")
    ("C-<left>"      . "\M-[1;5D"))
  "An assoc list of pretty key strings
and their terminal equivalents.")

(defun key (desc)
  (or (and window-system (read-kbd-macro desc))
      (or (cdr (assoc desc real-keyboard-keys))
          (read-kbd-macro desc))))

(IsNotTerminal
 (NotBatchMode
  ;; The following key-binding iconifies a window -- we disable it:
  (global-unset-key "\C-x\C-z")
  ;; The following key-binding quits emacs -- we disable it too:
  (global-unset-key "\C-x\C-c")))

;;;======================================================================
;;; Various emacs fixup
;;;======================================================================
(defadvice message (after message-tail activate)
  "goto point max after a message"
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (let ((windows (get-buffer-window-list (current-buffer) nil t)))
      (while windows
        (set-window-point (car windows) (point-max))
        (setq windows (cdr windows))))))

;; Dont output .# lock-files...
(setq create-lockfiles nil)

;;;======================================================================
;;; Search utilities
;;;======================================================================
(defvar ni-regexp-history-search nil
  "History of regexp searches.")

(defun ni-find-search-directory (&optional aStartDir)
  "Return GIT_ROOT if this file is a part of a git repo,
else return default-directory"
  (let ((curdir (or aStartDir default-directory))
        (max 20)
        (found nil))
    (while (and (not found) (> max 0))
      (progn
        (if (or (file-directory-p (concat curdir ".git"))
                (file-directory-p (concat curdir ".hg"))
                (file-regular-p (concat curdir "_ham_project"))
                (file-regular-p (concat curdir "TARGETS")))
            (progn
              (setq found t))
          (progn
            (setq curdir (concat curdir "../"))
            (setq max (- max 1))))))
    (if found (expand-file-name curdir) default-directory)))

(defun ni-find-read-regexp (msg)
  (read-from-minibuffer msg
                        (-first-item (-non-nil
                                      (list (thing-at-point 'symbol)
                                            (-first-item ni-regexp-history-search))))
                        nil nil 'ni-regexp-history-search))

;;;======================================================================
;;; Navigation utilities
;;;======================================================================
;; Inspired by http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html

(defvar ni-left-brackets '("(" "{" "[" "<" "\"" "'" "`")
  "List of left bracket chars.")

(defvar ni-right-brackets '(")" "]" "}" ">" "\"" "'" "`")
  "list of right bracket chars.")

(defun ni-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `ni-left-brackets'."
  (interactive)
  (re-search-backward (regexp-opt ni-left-brackets) nil t))

(defun ni-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `ni-right-brackets'."
  (interactive)
  (re-search-forward (regexp-opt ni-right-brackets) nil t))

(defun ni-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `ni-left-brackets' and `ni-right-brackets'."
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp ))
     ((looking-at (regexp-opt ni-left-brackets))
      (forward-sexp))
     ((looking-back (regexp-opt ni-right-brackets) (max (- (point) 1) 1))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

;;;======================================================================
;;; Dark mode
;;;======================================================================
(defun ni-shell-command-to-string-no-stderr (aCommand)
  (with-output-to-string
    (with-current-buffer
        standard-output
      (process-file shell-file-name nil '(t nil)  nil shell-command-switch aCommand))))

;; Usage:
;;   (ni-check-dark-mode
;;    (lambda () (message "Is Dark mode"))
;;    (lambda () (message "Is Light mode")))
(defun ni-check-dark-mode (aDarkMode aLightMode)
  (if (and (string-match "darwin" (prin1-to-string system-type))
           (string= (ni-shell-command-to-string-no-stderr "printf %s \"$( osascript -e \'tell application \"System Events\" to tell appearance preferences to return dark mode\' )\"") "true"))
      (if aDarkMode (funcall aDarkMode))
    (if aLightMode (funcall aLightMode))))

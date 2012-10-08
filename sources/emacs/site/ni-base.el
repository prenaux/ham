(provide 'ni-base)

(defun agl-getenv (str)
  (if (getenv str)
      (replace-regexp-in-string "\\\\" "/" (getenv str))
    ""))

(defconst ENV_DEVENV (agl-getenv "EMACS_DEVENV"))

(defconst ENV_DEVENV_EMACS_SCRIPTS (concat ENV_DEVENV "/sources/emacs/site"))

(defconst ENV_EMACS_BATCHMODE  (agl-getenv "EMACS_BATCHMODE"))
(add-to-list 'load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/misc"))

;; Fix a bug with some version of emacs...
(setq warning-suppress-types nil)

(provide 'aglemacs)
(require 'emacs-type)

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

(defun agl-begin-time-block (name)
  (setq *emacs-time-block-prev-begin* *emacs-time-block-begin*)
  (setq *emacs-time-block-begin* (float-time))
  (message (concat "[%3.3fs] [%3.3fs] === " name " ===")
           (- (float-time) *emacs-time-start*)
           (- (float-time) *emacs-time-block-prev-begin*)))

(defun agl-found-custom-arg (switch)
  (member switch command-line-args))

;;;======================================================================
;;; Versions
;;;======================================================================
(agl-begin-time-block "Versions")
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

(defmacro DontExecute (&rest x) ())

;;;======================================================================
;;; OS Environment
;;;======================================================================
(defun ni-add-to-PATH-front (aValue)
  (setenv
   "PATH"
   (concat aValue ENV_SEP
           (getenv "PATH"))))

(defun ni-add-to-PATH-back (aValue)
  (setenv
   "PATH"
   (concat (getenv "PATH") ENV_SEP
           aValue)))

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

;; If not set "WORK" is the parent folder of the devenv
(if (not (getenv "WORK"))
    (setenv "WORK" (expand-file-name (concat ENV_DEVENV "/.."))))

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

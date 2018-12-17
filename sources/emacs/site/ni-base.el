(provide 'ni-base)

(defun agl-getenv (str)
  (if (getenv str)
      (replace-regexp-in-string "\\\\" "/" (getenv str))
    ""))

;; If not set "EMACS_DEVENV" is set from HAM_HOME
(if (not (getenv "EMACS_DEVENV"))
    (setenv "EMACS_DEVENV" (getenv "HAM_HOME")))
(defconst EMACS_DEVENV (agl-getenv "EMACS_DEVENV"))
(defconst ENV_DEVENV (agl-getenv "EMACS_DEVENV"))

;; If not set "WORK" is the parent folder of EMACS_DEVENV
(if (not (getenv "WORK"))
    (setenv "WORK" (expand-file-name (concat EMACS_DEVENV "/.."))))
(defconst ENV_WORK (agl-getenv "WORK"))

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

(defun agl-begin-time-block (name)
  (setq *emacs-time-block-prev-begin* *emacs-time-block-begin*)
  (setq *emacs-time-block-begin* (float-time))
  (NotBatchMode
   (message (concat "[%3.3fs] [%3.3fs] === " name " ===")
            (- (float-time) *emacs-time-start*)
            (- (float-time) *emacs-time-block-prev-begin*))))

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

(defun ni-find-search-directory ()
  "Return GIT_ROOT if this file is a part of a git repo,
else return default-directory"
  (let ((curdir default-directory)
        (max 20)
        (found nil))
    (while (and (not found) (> max 0))
      (progn
        (if (file-directory-p (concat curdir ".git"))
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

(defun ni-git-grep--run (regexp dir)
  (require 'grep)
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (if (string= command "git grep")
          (setq command nil))
      (setq dir (file-name-as-directory (expand-file-name dir)))
      (setq command
            (grep-expand-template "git grep -n -e <R>"
                                  regexp))
      (when command
        (if (equal current-prefix-arg '(4))
            (setq command
                  (read-from-minibuffer "Confirm: "
                                        command nil nil 'grep-history))
          (add-to-history 'grep-history command)))
      (when command
	(let ((default-directory dir)
	      (compilation-environment (cons "PAGER=" compilation-environment)))
	  ;; Setting process-setup-function makes exit-message-function work
	  ;; even when async processes aren't supported.
	  (compilation-start command 'grep-mode))
	(if (eq next-error-last-buffer (current-buffer))
	    (setq default-directory dir))))))

(defun ni-git-grep-search-dir (regexp &optional dir)
  ""
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
  			           nil nil 'grep-history)
	     nil))
      (t (let* ((regexp (ni-find-read-regexp "Git grep for: "))
		(dir (read-directory-name "In directory: " (ni-find-search-directory))))
	   (list regexp dir))))))
  (ni-git-grep--run regexp dir))

(defun ni-git-grep-current-dir (regexp &optional dir)
  ""
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
  			           nil nil 'grep-history)
	     nil))
      (t (let* ((regexp (ni-find-read-regexp "Git grep for: ")
		        (dir (read-directory-name "In directory: " default-directory)))
	        (list regexp dir))))))
   (ni-git-grep--run regexp dir)))

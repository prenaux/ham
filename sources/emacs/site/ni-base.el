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

;; warf... dont output .# lock-files...
(setq create-lockfiles nil)

;; These are for people who prefer to have closing parens line up with open parens.
;; That is, not have them all bunched up on one line.
;; (Pierre) ... as opposed to "lined up at shitty random and unreadable positions ^^"
(defun ctp ()
  "collect trailing parens"
  ;; bunches the `dangling parens' in your code up into the useless and unnatural
  ;; position expected by the lisp community.  Depends on font-lock to avoid
  ;; moving a trailing paren onto the end of a comment.
  (interactive)
  (while (re-search-forward "^\\s *)")
    (end-of-line 0)
    (if (eq 'font-lock-comment-face (get-char-property (1- (point)) 'face))
        (end-of-line 2)
      (delete-indentation -1)
    )
    (beginning-of-line 2)
  )
)

(defun etp ()
  "Expands trailing parens"
  ;; This function ignores parens within quotes and comments only if
  ;; font-lock is turned on.  If the closing paren is on the same line as
  ;; the open then leave it there otherwise give it it's own line.  The
  ;; trailing parens won't be lined up in the same column with the opening
  ;; paren unless you have my mod for calculate-lisp-indent installed too.
  (interactive)
  (while (re-search-forward ")")
    (if (not (or (eq 'font-lock-comment-face (get-char-property (1- (point)) 'face))
                 (eq 'font-lock-string-face (get-char-property (1- (point)) 'face))
                 (looking-back "^\\s-*)")
             )
        )
        (let ((pos1 (line-beginning-position)))
          (if (save-excursion
                (condition-case () (goto-char (scan-sexps (point) -1)) (error nil))
                (eq pos1 (line-beginning-position))
              )
              ()           ;do nothing if unbalanced or open on same line
            (backward-char)
            (newline-and-indent t)
            (goto-char pos1)
          )
        )
    )
  )
)

(defun calculate-lisp-indent (&optional parse-start)
  "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (if parse-start
          (goto-char parse-start)
        (beginning-of-defun))
      ;; Find outermost containing sexp
      (while (< (point) indent-point)
        (setq state (parse-partial-sexp (point) indent-point 0)))
      ;; Find innermost containing sexp
      (while (and retry
                  state
                  (> (elt state 0) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
                 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                            indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
            ;; indent-point immediately follows open paren.
            ;; Don't call hook.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s(")
                 ;; First element of containing sexp is a list.
                 ;; Indent under that list.
                )
                ((> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp)
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (= (point) calculate-lisp-indent-last-sexp)
                     ;; Containing sexp has nothing before this line
                     ;; except the first element.  Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp
                                              0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `calculate-lisp-indent-last-sexp'.  Again, it's
                 ;; almost certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                     0 t)
                 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column))
            ;; RGB fixed it to work like I want
            (lisp-indent-offset
             (if (save-excursion (goto-char indent-point)
                                 (beginning-of-line)
                                 (looking-at "\\s-*\\s)"))
                 0
               lisp-indent-offset)))
        ;; end of RGB fix
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               nil)
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; If the function has no special alignment
                ;; or it does not apply to this argument,
                ;; try to align a constant-symbol under the last
                ;; preceding constant symbol, if there is such one of
                ;; the last 2 preceding symbols, in the previous
                ;; uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation
                     ;; where it begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace
                       ;; following an open paren.  (Bug#1012)
                       (backward-prefix-chars)
                       (while (and (not (looking-back "^[ \t]*\\|([ \t]+"))
                                   (or (not containing-sexp)
                                       (< (1+ containing-sexp) (point))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position)
                                   (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant
                ;; as defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (t
               normal-indent))))))

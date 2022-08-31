(provide 'ni-base)
(require 'ni-emacs-compat)

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
(if (string-empty-p (getenv "WORK"))
    (setenv "WORK" (expand-file-name (concat EMACS_DEVENV "/.."))))
(defconst ENV_WORK (agl-getenv "WORK"))
(defconst ENV_SHARED_WORK
  (if (string-empty-p (agl-getenv "SHARED_WORK")) ENV_WORK (agl-getenv "SHARED_WORK")))

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
        (if (or (file-directory-p (concat curdir ".git"))
                (file-directory-p (concat curdir ".hg"))
                (file-regular-p (concat curdir "_ham_project")))
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

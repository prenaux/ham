;;; aglscript.el --- Major mode for editing aglScript source text

;;; Commentary:
;;
;; The main features of this aglScript mode are syntactic
;; highlighting (enabled with `font-lock-mode' or
;; `global-font-lock-mode'), automatic indentation and filling of
;; comments.
;;
;; Installation:
;;
;; Put this file in a directory where Emacs can find it (`C-h v
;; load-path' for more info). Then add the following lines to your
;; Emacs initialization file:
;;
;;    (add-to-list 'auto-mode-alist '("\\.aq\\'" . aglscript-mode))
;;    (add-to-list 'auto-mode-alist '("\\.aqw\\'" . aglscript-mode))
;;    (add-to-list 'auto-mode-alist '("\\.aqp\\'" . aglscript-mode))
;;    (autoload 'aglscript-mode "aglScript" nil t)
;;
;; General Remarks:
;;
;; This mode assumes that block comments are not nested inside block
;; comments and that strings do not contain line breaks.
;;
;; Exported names start with "aglscript-" whereas private names start
;; with "aq-".
;;

;;; Code:

(require 'cc-mode)
(require 'font-lock)
(require 'newcomment)

(defgroup aglscript nil
  "Customization variables for `aglscript-mode'."
  :tag "aglScript"
  :group 'languages)

(defcustom aglscript-indent-level 4
  "Number of spaces for each indentation step."
  :type 'integer
  :group 'aglscript)

(defcustom aglscript-auto-indent-flag t
  "Automatic indentation with punctuation characters. If non-nil, the
current line is indented when certain punctuations are inserted."
  :type 'boolean
  :group 'aglscript)


;; --- Keymap ---

(defvar aglscript-mode-map nil
  "Keymap used in aglScript mode.")

(unless aglscript-mode-map
  (setq aglscript-mode-map (make-sparse-keymap)))

(when aglscript-auto-indent-flag
  (mapc (lambda (key)
	  (define-key aglscript-mode-map key 'aglscript-insert-and-indent))
	'(";")))

(defun aglscript-insert-and-indent (key)
  "Run command bound to key and indent current line. Runs the command
bound to KEY in the global keymap and indents the current line."
  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (indent-according-to-mode))


;; --- Syntax Table And Parsing ---

(defvar aglscript-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)

    ;; The syntax class of underscore should really be `symbol' ("_")
    ;; but that makes matching of tokens much more complex as e.g.
    ;; "\\<xyz\\>" matches part of e.g. "_xyz" and "xyz_abc". Defines
    ;; it as word constituent for now.
    (modify-syntax-entry ?_ "w" table)

    table)
  "Syntax table used in aglScript mode.")


(defun aq-re-search-forward-inner (regexp &optional bound count)
  "Auxiliary function for `aq-re-search-forward'."
  (let ((parse)
        (saved-point (point-min)))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (parse-partial-sexp saved-point (point)))
      (cond ((nth 3 parse)
             (re-search-forward
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse)))
              (save-excursion (end-of-line) (point)) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            (t
             (setq count (1- count))))
      (setq saved-point (point))))
  (point))


(defun aq-re-search-forward (regexp &optional bound noerror count)
  "Search forward but ignore strings and comments. Invokes
`re-search-forward' but treats the buffer as if strings and
comments have been removed."
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(aq-re-search-forward-inner regexp bound 1))
               ((< count 0)
                '(aq-re-search-backward-inner regexp bound (- count)))
               ((> count 0)
                '(aq-re-search-forward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defun aq-re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `aq-re-search-backward'."
  (let ((parse)
        (saved-point (point-min)))
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (forward-char))
      (setq parse (parse-partial-sexp saved-point (point)))
      (cond ((nth 3 parse)
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse)))
              (save-excursion (beginning-of-line) (point)) t))
            ((nth 7 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            (t
             (setq count (1- count))))))
  (point))


(defun aq-re-search-backward (regexp &optional bound noerror count)
  "Search backward but ignore strings and comments. Invokes
`re-search-backward' but treats the buffer as if strings and
comments have been removed."
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(aq-re-search-backward-inner regexp bound 1))
               ((< count 0)
                '(aq-re-search-forward-inner regexp bound (- count)))
               ((> count 0)
                '(aq-re-search-backward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defun aq-continued-var-decl-list-p ()
  "Return non-nil if point is inside a continued variable declaration list."
  (interactive)
  (let ((start (save-excursion (aq-re-search-backward "\\<local\\>" nil t))))
    (and start
	 (save-excursion (re-search-backward "\n" start t))
	 (not (save-excursion
		(aq-re-search-backward
		 ";\\|[^, \t][ \t]*\\(/[/*]\\|$\\)" start t))))))


;; --- Font Lock ---

(defun aq-inside-param-list-p ()
  "Return non-nil if point is inside a function parameter list."
  (condition-case err
      (save-excursion
	(up-list -1)
	(and (looking-at "(")
	     (progn (backward-word 1)
		    (or (looking-at "function")
			(progn (backward-word 1) (looking-at "function"))))))
    (error nil)))


(defconst aq-function-heading-1-re
  "[ \t]*function[ \t]+\\(\\w+\\)"
  "Regular expression matching the start of a function header.")

(defconst aq-keyword-re
  (regexp-opt '(
      "break" "case" "catch" "continue" "default"
      "delete" "do" "else" "for" "foreach" "function" "if" "in"
      "return" "switch" "this" "throw" "try" "typeof" "while" "_args_"
  ) 'words)
  "Regular expression matching any aglScript keyword.")

(defconst aq-basic-type-re
  (regexp-opt '("local" "var"
                "void" "bool" "int" "float" "string" "stringarray" "array" "map") 'words)
  "Regular expression matching any predefined type in aglScript.")

(defconst aq-constant-re
  (regexp-opt '("false" "invalid" "null" "true") 'words)
  "Regular expression matching any future reserved words in aglScript.")

;; Lispy stuff @()
(defconst aq-lisp-re
  (regexp-opt '("defun"
                "defvar"
                "defstruct"
                "define"
                "deftemplate"
                "defgeneric"
                "deftpl"
                "defclass"
                "defconst"
                "defenum"
                "defoperator"
                "defmeta"
                "definterface"
                "deferror"
                "defcheck"
                "deftype"
                "defdata"
                "package"
                "let"
                "loop"
                "recur"
                "cond"
                "do"
                "begin"
                "undefined"
                "nop"
                "set-member"
                "get-member"
                "match-type"
                "any-type") 'words)
  "Regular expression matching any future lisp reserved words in aglScript.")

(defconst aq-pp-re
  (regexp-opt '("CreateInstance" "CreateGlobalInstance" "QueryInterface" "Import" "NewImport" "import"
                "Array" "Table" "List" "Map" "Set"  "Vector"
                "Vec2" "Vec3" "Vec4" "RGB" "RGBA" "Quat" "Plane" "Rect" "Matrix" "UUID"
                ) 'words)
  "Regular expression matching any future reserved words in aglScript.")

(defconst aq-builtin-re
  (regexp-opt '(
      ;; Regular stuff
      "namespace"
      "delegate"
      "dbg" "dbgs" "dbgl"
      "log"
      "logd"
      "logConsole"
      "logConsoled"
      "logWarning"
      "logWarningd"
      "logError"
      "logErrord"
      "logServer"
      "logServerd"
      "logClient"
      "logClientd"
      "main"
      "print" "println" "printdebug" "printdebugln"
      "isValid"
      "MessageID"
      "clock"
      "setraiseerrormode"
      "getraiseerrormode"
      "assert"
      "getroottable"
      "compilestring"
      "bind"
      "ultof"
      "ftoul"
      "FourCC"
      "enumStringValue"
      "enumValueString"
      "enumFlagsString"
      "enumDefaultValueString"
      "enumDefaultFlagsString"
      "enumID"
      "enumName"
      "numElements"
      "elementName"
      "elementValue"
      "getdelegate"
      "setdelegate"
      "getparent"
      "invalidate"
      "isinstance"
      "hasdelegate"
      "new"
      "tostring"
      "tointeger"
      "tofloat"
      "tobindname"
      "isbindname"
      "isvalidbindname"
      "toidentifier"
      "isidentifier"
      "isvalididentifier"
      ) 'words)
  "Regular expression matching any future reserved words in aglScript.")

(defconst aq-font-lock-keywords-1
  (list
   (list aq-function-heading-1-re 1 font-lock-function-name-face)
   (list "[=(][ \t]*\\(/.*?[^\\]/\\w*\\)" 1 font-lock-string-face))
  "Level one font lock.")

(defconst aq-font-lock-keywords-2
  (append aq-font-lock-keywords-1
          (list (list aq-keyword-re 1 font-lock-keyword-face)
                (cons aq-lisp-re font-lock-keyword-face)
                (cons aq-builtin-re font-lock-builtin-face)
                (cons aq-pp-re font-lock-preprocessor-face)
                (cons aq-basic-type-re font-lock-type-face)
                (cons aq-constant-re font-lock-constant-face)))
  "Level two font lock.")


;; Limitations with variable declarations: There seems to be no
;; sensible way to highlight variables occuring after an initialized
;; variable in a variable list. For instance, in
;;
;;    var x, y = f(a, b), z
;;
;; z will not be highlighted.

(defconst aq-font-lock-keywords-3
  (append
   aq-font-lock-keywords-2
   (list
    "printdebugln"

    ;; variable declarations
    (list
     (concat "\\<\\(const\\|local\\)\\>\\|" aq-basic-type-re)
     (list "\\(\\w+\\)[ \t]*\\([=;].*\\|,\\|/[/*]\\|$\\)"
	   nil
	   nil
	   '(1 font-lock-variable-name-face)))
;;
    ;; continued variable declaration list
	;; --- Disabled, makes emacs freeze when inputing a mix of "' in some cases... (also doesnt have any notable visible impact...)
;;    (list
;;     (concat "^[ \t]*\\w+[ \t]*\\([,;=]\\|/[/*]\\|$\\)")
;;     (list "\\(\\w+\\)[ \t]*\\([=;].*\\|,\\|/[/*]\\|$\\)"
;;	   '(if (save-excursion (backward-char) (aq-continued-var-decl-list-p))
;;		(backward-word 1)
;;	      (end-of-line))
;;	   '(end-of-line)
;;	   '(1 font-lock-variable-name-face)))

    ;; formal parameters
    (list
     (concat "\\<function\\>\\([ \t]+\\w+\\)?[ \t]*([ \t]*\\w")
     (list "\\(\\w+\\)\\([ \t]*).*\\)?"
	   '(backward-char)
	   '(end-of-line)
	   '(1 font-lock-variable-name-face)))

    ;; continued formal parameter list
    (list
     (concat "^[ \t]*\\w+[ \t]*[,)]")
     (list "\\w+"
	   '(if (save-excursion (backward-char) (aq-inside-param-list-p))
		(backward-word 1)
	      (end-of-line))
	   '(end-of-line)
	   '(0 font-lock-variable-name-face)))

	   ))
  "Level three font lock.")

(defconst aq-font-lock-keywords
  '(aq-font-lock-keywords-3)
  "See `font-lock-keywords'.")


;; --- Indentation ---

(defconst aq-possibly-braceless-keyword-re
  (regexp-opt
   '("active_table" "catch" "do" "else" "finally" "for" "foreach" "function" "if" "try" "while" "with")
   'words)
  "Regular expression matching keywords that are optionally
  followed by an opening brace.")

(defconst aq-indent-operator-re
  (concat "[-+*/%<>=&^|?:]\\([^-+*/]\\|$\\)\\|"
          (regexp-opt '("in" "instanceof") 'words))
  "Regular expression matching operators that affect indentation
  of continued expressions.")


(defun aq-looking-at-operator-p ()
  "Return non-nil if text after point is an operator (that is not
a comma)."
  (save-match-data
    (and (looking-at aq-indent-operator-re)
         (or (not (looking-at ":"))
             (save-excursion
               (and (aq-re-search-backward "[?:{]\\|\\<case\\>" nil t)
                    (looking-at "?")))))))


(defun aq-continued-expression-p ()
  "Returns non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (aq-looking-at-operator-p)
        (and (aq-re-search-backward "\n" nil t)
	     (progn
	       (skip-chars-backward " \t")
	       (backward-char)
	       (and (> (point) (point-min))
                    (save-excursion (backward-char) (not (looking-at "[/*]/")))
                    (aq-looking-at-operator-p)
		    (and (progn (backward-char)
				(not (looking-at "++\\|--\\|/[/*]"))))))))))


(defun aq-end-of-do-while-loop-p ()
  "Returns non-nil if word after point is `while' of a do-while
statement, else returns nil. A braceless do-while statement
spanning several lines requires that the start of the loop is
indented to the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\<while\\>")
	(if (save-excursion
	      (skip-chars-backward "[ \t\n]*}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion
	      (backward-list) (backward-word 1) (looking-at "\\<do\\>"))
	  (aq-re-search-backward "\\<do\\>" (point-at-bol) t)
	  (or (looking-at "\\<do\\>")
	      (let ((saved-indent (current-indentation)))
		(while (and (aq-re-search-backward "^[ \t]*\\<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "[ \t]*\\<do\\>")
		     (not (aq-re-search-forward
			   "\\<while\\>" (point-at-eol) t))
		     (= (current-indentation) saved-indent)))))))))


(defun aq-ctrl-statement-indentation ()
  "Returns the proper indentation of the current line if it
starts the body of a control statement without braces, else
returns nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (looking-at "[{]"))
                 (progn
                   (aq-re-search-backward "[[:graph:]]" nil t)
                   (forward-char)
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w")
                   (looking-at aq-possibly-braceless-keyword-re))
                 (not (aq-end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) aglscript-indent-level)))))


(defun aq-proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (let ((ctrl-stmt-indent (aq-ctrl-statement-indentation))
          (same-indent-p (looking-at "[]})]\\|\\<case\\>\\|\\<default\\>"))
          (continued-expr-p (aq-continued-expression-p)))
      (cond (ctrl-stmt-indent)
	    ((aq-continued-var-decl-list-p)
	     (aq-re-search-backward "\\<var\\>" nil t)
	     (+ (current-indentation) aglscript-indent-level))
            ((nth 1 parse-status)
             (goto-char (nth 1 parse-status))
             (if (looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
                 (progn
                   (skip-syntax-backward " ")
                   (when (= (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (cond (same-indent-p
                          (current-column))
                         (continued-expr-p
                          (+ (current-column) (* 2 aglscript-indent-level)))
                         (t
                          (+ (current-column) aglscript-indent-level))))
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column)))
	    (continued-expr-p aglscript-indent-level)
            (t 0)))))


(defun aglscript-indent-line ()
  "Indent the current line as aglScript source text."
  (interactive)
  (let ((parse-status
         (save-excursion (parse-partial-sexp (point-min) (point-at-bol))))
        (offset (- (current-column) (current-indentation))))
    (when (not (nth 8 parse-status))
      (indent-line-to (aq-proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))


;; --- Filling ---

;; FIXME: It should be possible to use the more sofisticated function
;; `c-fill-paragraph' in `cc-cmds.el' instead. However, just setting
;; `fill-paragraph-function' to `c-fill-paragraph' does not work;
;; inside `c-fill-paragraph', `fill-paragraph-function' evaluates to
;; nil!?

(defun aq-backward-paragraph ()
  "Move backward to start of paragraph. Postcondition: Point is at
beginning of buffer or the previous line contains only whitespace."
  (forward-line -1)
  (while (not (or (bobp) (looking-at "^[ \t]*$")))
    (forward-line -1))
  (when (not (bobp)) (forward-line 1)))


(defun aq-forward-paragraph ()
  "Move forward to end of paragraph. Postcondition: Point is at
end of buffer or the next line contains only whitespace."
  (forward-line 1)
  (while (not (or (eobp) (looking-at "^[ \t]*$")))
    (forward-line 1))
  (when (not (eobp)) (backward-char 1)))


(defun aq-fill-block-comment-paragraph (parse-status justify)
  "Fill current paragraph as a block comment. PARSE-STATUS is the
result of `parse-partial-regexp' from beginning of buffer to
point. JUSTIFY has the same meaning as in `fill-paragraph'."
  (let ((offset (save-excursion
                  (goto-char (nth 8 parse-status)) (current-indentation))))
    (save-excursion
      (save-restriction
        (narrow-to-region (save-excursion
                            (goto-char (nth 8 parse-status)) (point-at-bol))
                          (save-excursion
			    (goto-char (nth 8 parse-status))
			    (re-search-forward "*/")))
        (narrow-to-region (save-excursion
                            (aq-backward-paragraph)
                            (when (looking-at "^[ \t]*$") (forward-line 1))
                            (point))
                          (save-excursion
                            (aq-forward-paragraph)
                            (when (looking-at "^[ \t]*$") (backward-char))
                            (point)))
        (goto-char (point-min))
        (while (not (eobp))
          (delete-horizontal-space)
          (forward-line 1))
        (let ((fill-column (- fill-column offset))
              (fill-paragraph-function nil))
          (fill-paragraph justify))

        ;; In Emacs 21.4 as opposed to CVS Emacs 22,
        ;; `fill-paragraph' seems toadd a newline at the end of the
        ;; paragraph. Remove it!
        (goto-char (point-max))
        (when (looking-at "^$") (backward-delete-char 1))

        (goto-char (point-min))
        (while (not (eobp))
          (indent-to offset)
          (forward-line 1))))))


(defun aq-sline-comment-par-start ()
  "Return point at the beginning of the line where the current
single-line comment paragraph starts."
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp))
                (looking-at "^[ \t]*//[ \t]*[[:graph:]]"))
      (forward-line -1))
    (unless (bobp) (forward-line 1))
    (point)))


(defun aq-sline-comment-par-end ()
  "Return point at end of current single-line comment paragraph."
  (save-excursion
    (beginning-of-line)
    (while (and (not (eobp))
                (looking-at "^[ \t]*//[ \t]*[[:graph:]]"))
      (forward-line 1))
    (unless (bobp) (backward-char))
    (point)))


(defun aq-sline-comment-offset (line)
  "Return the column at the start of the current single-line
comment paragraph."
  (save-excursion
    (goto-line line)
    (re-search-forward "//" (point-at-eol))
    (goto-char (match-beginning 0))
    (current-column)))


(defun aq-sline-comment-text-offset (line)
  "Return the column at the start of the text of the current
single-line comment paragraph."
  (save-excursion
    (goto-line line)
    (re-search-forward "//[ \t]*" (point-at-eol))
    (current-column)))


(defun aq-at-empty-sline-comment-p ()
  "Return non-nil if inside an empty single-line comment."
  (and (save-excursion
         (beginning-of-line)
         (not (looking-at "^.*//.*[[:graph:]]")))
       (save-excursion
         (re-search-backward "//" (point-at-bol) t))))


(defun aq-fill-sline-comments (parse-status justify)
  "Fill current paragraph as a sequence of single-line comments.
PARSE-STATUS is the result of `parse-partial-regexp' from
beginning of buffer to point. JUSTIFY has the same meaning as in
`fill-paragraph'."
  (when (not (aq-at-empty-sline-comment-p))
    (let* ((start (aq-sline-comment-par-start))
           (start-line (1+ (count-lines (point-min) start)))
           (end (aq-sline-comment-par-end))
           (offset (aq-sline-comment-offset start-line))
           (text-offset (aq-sline-comment-text-offset start-line)))
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*//[ \t]*" nil t)
            (replace-match "")
            (forward-line 1))
          (let ((fill-paragraph-function nil)
                (fill-column (- fill-column text-offset)))
            (fill-paragraph justify))

          ;; In Emacs 21.4 as opposed to CVS Emacs 22,
          ;; `fill-paragraph' seems toadd a newline at the end of the
          ;; paragraph. Remove it!
          (goto-char (point-max))
          (when (looking-at "^$") (backward-delete-char 1))

          (goto-char (point-min))
          (while (not (eobp))
            (indent-to offset)
            (insert "//")
            (indent-to text-offset)
            (forward-line 1)))))))


(defun aq-trailing-comment-p (parse-status)
  "Return non-nil if inside a trailing comment. PARSE-STATUS is
the result of `parse-partial-regexp' from beginning of buffer to
point."
  (save-excursion
    (when (nth 4 parse-status)
      (goto-char (nth 8 parse-status))
      (skip-chars-backward " \t")
      (not (bolp)))))


(defun aq-block-comment-p (parse-status)
  "Return non-nil if inside a block comment. PARSE-STATUS is the
result of `parse-partial-regexp' from beginning of buffer to
point."
  (save-excursion
    (save-match-data
      (when (nth 4 parse-status)
        (goto-char (nth 8 parse-status))
        (looking-at "/\\*")))))


(defun aglscript-fill-paragraph (&optional justify)
  "If inside a comment, fill the current comment paragraph.
Trailing comments are ignored."
  (interactive)
  (let ((parse-status (parse-partial-sexp (point-min) (point))))
    (when (and (nth 4 parse-status)
               (not (aq-trailing-comment-p parse-status)))
      (if (aq-block-comment-p parse-status)
          (aq-fill-block-comment-paragraph parse-status justify)
        (aq-fill-sline-comments parse-status justify))))
  t)


;; --- Imenu ---

(defconst aq-imenu-generic-expression
  (list
   (list
    nil
    "function\\s-+\\(\\w+\\)\\s-*("
    1))
  "Regular expression matching top level procedures. Used by imenu.")


;; --- Main Function ---

;;;###autoload
(defun aglscript-mode ()
  "Major mode for editing aglScript source text.

Key bindings:

\\{aglscript-mode-map}"
  (interactive)
  (kill-all-local-variables)

  (use-local-map aglscript-mode-map)
  (set-syntax-table aglscript-mode-syntax-table)

  (set (make-local-variable 'indent-line-function) 'aglscript-indent-line)
  (set (make-local-variable 'font-lock-defaults) (list aq-font-lock-keywords))

  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  ;; Comments
  (setq comment-start "// ")
  (setq comment-end "")
  (set (make-local-variable 'fill-paragraph-function)
       'aglscript-fill-paragraph)

  ;; Imenu
  (setq imenu-case-fold-search nil)
  (set (make-local-variable 'imenu-generic-expression)
      aq-imenu-generic-expression)

  (setq major-mode 'aglscript-mode)
  (setq mode-name "aglScript")
  (run-hooks 'aglscript-mode-hook))


(provide 'aglscript-mode)
;;; aglscript.el ends here

;;; niscript.el --- Major mode for editing niScript source text

;;; Commentary:
;;
;; The main features of this niScript mode are syntactic
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
;;    (add-to-list 'auto-mode-alist '("\\.ni\\'" . niscript-mode))
;;    (add-to-list 'auto-mode-alist '("\\.nip\\'" . niscript-mode))
;;    (add-to-list 'auto-mode-alist '("\\.niw\\'" . niscript-mode))
;;    (add-to-list 'auto-mode-alist '("\\.nil\\'" . niscript-mode))
;;    (add-to-list 'auto-mode-alist '("\\.nit\\'" . niscript-mode))
;;    (autoload 'niscript-mode "niScript" nil t)
;;
;; General Remarks:
;;
;; This mode assumes that block comments are not nested inside block
;; comments and that strings do not contain line breaks.
;;
;; Exported names start with "niscript-" whereas private names start
;; with "nip-".
;;

;;; Code:

(require 'cc-mode)
(require 'font-lock)
(require 'newcomment)

(defgroup niscript nil
  "Customization variables for `niscript-mode'."
  :tag "niScript"
  :group 'languages)

(defcustom niscript-indent-level 2
  "Number of spaces for each indentation step."
  :type 'integer
  :group 'niscript)

(defcustom niscript-auto-indent-flag t
  "Automatic indentation with punctuation characters. If non-nil, the
   current line is indented when certain punctuations are inserted."
  :type 'boolean
  :group 'niscript)


;; --- Keymap ---

(defvar niscript-mode-map nil
  "Keymap used in niScript mode.")

(unless niscript-mode-map
  (setq niscript-mode-map (make-sparse-keymap)))

(when niscript-auto-indent-flag
  (mapc (lambda (key)
	  (define-key niscript-mode-map key 'niscript-insert-and-indent))
	'(";")))

(defun niscript-insert-and-indent (key)
  "Run command bound to key and indent current line. Runs the command
bound to KEY in the global keymap and indents the current line."
  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (indent-according-to-mode))


;; --- Syntax Table And Parsing ---

(defvar niscript-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)

    ;; The syntax class of underscore should really be `symbol' ("_")
    ;; but that makes matching of tokens much more complex as e.g.
    ;; "\\<xyz\\>" matches part of e.g. "_xyz" and "xyz_abc". Defines
    ;; it as word constituent for now.
    (modify-syntax-entry ?_ "w" table)

    table)
  "Syntax table used in niScript mode.")


(defun nip-re-search-forward-inner (regexp &optional bound count)
  "Auxiliary function for `nip-re-search-forward'."
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


(defun nip-re-search-forward (regexp &optional bound noerror count)
  "Search forward but ignore strings and comments. Invokes
`re-search-forward' but treats the buffer as if strings and
comments have been removed."
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(nip-re-search-forward-inner regexp bound 1))
               ((< count 0)
                '(nip-re-search-backward-inner regexp bound (- count)))
               ((> count 0)
                '(nip-re-search-forward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defun nip-re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `nip-re-search-backward'."
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


(defun nip-re-search-backward (regexp &optional bound noerror count)
  "Search backward but ignore strings and comments. Invokes
`re-search-backward' but treats the buffer as if strings and
comments have been removed."
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(nip-re-search-backward-inner regexp bound 1))
               ((< count 0)
                '(nip-re-search-forward-inner regexp bound (- count)))
               ((> count 0)
                '(nip-re-search-backward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defun nip-continued-var-decl-list-p ()
  "Return non-nil if point is inside a continued variable declaration list."
  (interactive)
  (let ((start (save-excursion nil)))
    (and start
	 (save-excursion (re-search-backward "\n" start t))
	 (not (save-excursion
		(nip-re-search-backward
		 ";\\|[^, \t][ \t]*\\(/[/*]\\|$\\)" start t))))))

;; --- Font Lock ---

(defun nip-inside-param-list-p ()
  "Return non-nil if point is inside a function parameter list."
  (condition-case err
      (save-excursion
	(up-list -1)
	(and (looking-at "(")
	     (progn (backward-word 1)
		    (or (looking-at "function")
			(progn (backward-word 1) (looking-at "function"))))))
    (error nil)))

(defconst nip-function-heading-1-re
  "[ \t]*function[ \t]+\\(\\w+\\)"
  "Regular expression matching the start of a function header.")

(defconst nip-keyword-re
  (regexp-opt '(
      "break" "case" "catch" "continue" "default"
      "delete" "do" "else" "for" "foreach" "function" "if" "in"
      "return" "switch" "this" "throw" "try" "typeof" "while" "_args_"
  ) 'words)
  "Regular expression matching any niScript keyword.")

(defconst nip-basic-type-re
  (regexp-opt '("local" "let"
                "void" "any" "bool" "int" "float" "string"
                "mut" "var" "out"
                ) 'words)
  "Regular expression matching any predefined type in niScript.")

(defconst nip-constant-re
  (regexp-opt '("false" "invalid" "null" "true") 'words)
  "Regular expression matching any future reserved words in niScript.")

;; Lispy stuff @()
(defconst nip-lisp-re
  (regexp-opt '("function"
                "static"
                "operator"
                "meta"
                "macro"
                "macrolib"
                "module"
                "new"
                "class"
                "struct"
                "import"
                "interface"
                "cond"
                "scope"
                "not_implemented"
                "set_local_value"
                "set_local_ref"
                "set_member_value"
                "set_member_ref"
                "get_member"
                "match_type"
                "defer"
                ) 'words)
  "Regular expression matching any future lisp reserved words in niScript.")

(defconst nip-pp-re
  (regexp-opt '("CreateInstance" "CreateGlobalInstance" "QueryInterface" "Import" "NewImport"
                "Array" "Table" "List" "Map" "Set"  "Vector"
                "Vec2" "Vec3" "Vec4" "RGB" "RGBA" "Quat" "Plane" "Rect" "Matrix" "UUID"
                ) 'words)
  "Regular expression matching any future reserved words in niScript.")

(defconst nip-builtin-re
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
      "hasdelegate"
      "tostring"
      "toint"
      "tofloat"
      ) 'words)
  "Regular expression matching any future reserved words in niScript.")


(defconst nip-lisp-builtin-re
  (regexp-opt
   '(
     "alloc"
     ) 'words)
  "Regular expression matching any future reserved words in niScript.")

(defconst nip-font-lock-keywords-1
  (list
   (list "[=(][ \t]*\\(/.*?[^\\]/\\w*\\)" 1 font-lock-string-face))
  "Level one font lock.")

(defconst nip-font-lock-keywords-2
  (append nip-font-lock-keywords-1
          (list
           (list nip-function-heading-1-re 1 font-lock-function-name-face)
           (list nip-keyword-re 1 font-lock-keyword-face)
           (cons nip-lisp-re font-lock-keyword-face)
           (cons nip-builtin-re font-lock-builtin-face)
           (cons nip-lisp-builtin-re font-lock-builtin-face)
           (cons nip-pp-re font-lock-preprocessor-face)
           (cons nip-basic-type-re font-lock-type-face)
           (cons nip-constant-re font-lock-constant-face)))
  "Level two font lock.")

;; Limitations with variable declarations: There seems to be no
;; sensible way to highlight variables occuring after an initialized
;; variable in a variable list. For instance, in
;;
;;    var x, y = f(a, b), z
;;
;; z will not be highlighted.

(defconst nip-font-lock-keywords-3
  (append
   nip-font-lock-keywords-2
   (list
    "printdebugln"

    ;; variable declarations
    (list
     (concat "\\<\\(const\\|local\\)\\>\\|" nip-basic-type-re)
     (list "\\(\\w+\\)[ \t]*\\([=;].*\\|,\\|/[/*]\\|$\\)"
	   nil
	   nil
	   '(1 font-lock-variable-name-face)))

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
	   '(if (save-excursion (backward-char) (nip-inside-param-list-p))
		(backward-word 1)
	      (end-of-line))
	   '(end-of-line)
	   '(0 font-lock-variable-name-face)))

	   ))
  "Level three font lock.")

(defconst nip-font-lock-keywords
  '(nip-font-lock-keywords-3)
  "See `font-lock-keywords'.")

;; --- Indentation ---

(defconst nip-possibly-braceless-keyword-re
  (regexp-opt
   '("catch" "do" "else" "function" "if" "try")
   'words)
  "Regular expression matching keywords that are optionally
  followed by an opening brace.")

(defconst nip-indent-operator-re
  (concat "[-+*/%<>=&^|?:]\\([^-+*/:]\\|$\\)\\|"
          (regexp-opt '("in") 'words))
  "Regular expression matching operators that affect indentation
  of continued expressions.")

(defun nip-looking-at-operator-p ()
  "Return non-nil if text after point is an operator (that is not
a comma)."
  (save-match-data
    (and (looking-at nip-indent-operator-re)
         (or (not (looking-at ":"))
             (save-excursion
               (and (nip-re-search-backward "[?:{]" nil t)
                    (looking-at "?")))))))


(defun nip-continued-expression-p ()
  "Returns non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (nip-looking-at-operator-p)
        (and (nip-re-search-backward "\n" nil t)
	     (progn
	       (skip-chars-backward " \t")
	       (backward-char)
	       (and (> (point) (point-min))
                    (save-excursion (backward-char) (not (looking-at "[/*]/")))
                    (nip-looking-at-operator-p)
		    (and (progn (backward-char)
				(not (looking-at "++\\|--\\|/[/*]"))))))))))


(defun nip-end-of-do-while-loop-p ()
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
	  (nip-re-search-backward "\\<do\\>" (point-at-bol) t)
	  (or (looking-at "\\<do\\>")
	      (let ((saved-indent (current-indentation)))
		(while (and (nip-re-search-backward "^[ \t]*\\<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "[ \t]*\\<do\\>")
		     (not (nip-re-search-forward
			   "\\<while\\>" (point-at-eol) t))
		     (= (current-indentation) saved-indent)))))))))


(defun nip-ctrl-statement-indentation ()
  "Returns the proper indentation of the current line if it
starts the body of a control statement without braces, else
returns nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (looking-at "[{]"))
                 (progn
                   (nip-re-search-backward "[[:graph:]]" nil t)
                   (forward-char)
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w")
                   (looking-at nip-possibly-braceless-keyword-re))
                 (not (nip-end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) niscript-indent-level)))))


(defun nip-proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (let ((ctrl-stmt-indent (nip-ctrl-statement-indentation))
          (same-indent-p (looking-at "[]})]"))
          (continued-expr-p (nip-continued-expression-p)))
      (cond (ctrl-stmt-indent)
	    ((nip-continued-var-decl-list-p)
	     (nip-re-search-backward "\\<local\\>" nil t)
	     (+ (current-indentation) niscript-indent-level))
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
                          (+ (current-column) (* 2 niscript-indent-level)))
                         (t
                          (+ (current-column) niscript-indent-level))))
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column)))
	    (continued-expr-p niscript-indent-level)
            (t 0)))))


(defun niscript-indent-line ()
  "Indent the current line as niScript source text."
  (interactive)
  (let ((parse-status
         (save-excursion (parse-partial-sexp (point-min) (point-at-bol))))
        (offset (- (current-column) (current-indentation))))
    (when (not (nth 8 parse-status))
      (indent-line-to (nip-proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))


;; --- Filling ---

;; FIXME: It should be possible to use the more sofisticated function
;; `c-fill-paragraph' in `cc-cmds.el' instead. However, just setting
;; `fill-paragraph-function' to `c-fill-paragraph' does not work;
;; inside `c-fill-paragraph', `fill-paragraph-function' evaluates to
;; nil!?

(defun nip-backward-paragraph ()
  "Move backward to start of paragraph. Postcondition: Point is at
beginning of buffer or the previous line contains only whitespace."
  (forward-line -1)
  (while (not (or (bobp) (looking-at "^[ \t]*$")))
    (forward-line -1))
  (when (not (bobp)) (forward-line 1)))


(defun nip-forward-paragraph ()
  "Move forward to end of paragraph. Postcondition: Point is at
end of buffer or the next line contains only whitespace."
  (forward-line 1)
  (while (not (or (eobp) (looking-at "^[ \t]*$")))
    (forward-line 1))
  (when (not (eobp)) (backward-char 1)))


(defun nip-fill-block-comment-paragraph (parse-status justify)
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
                            (nip-backward-paragraph)
                            (when (looking-at "^[ \t]*$") (forward-line 1))
                            (point))
                          (save-excursion
                            (nip-forward-paragraph)
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


(defun nip-sline-comment-par-start ()
  "Return point at the beginning of the line where the current
single-line comment paragraph starts."
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp))
                (looking-at "^[ \t]*//[ \t]*[[:graph:]]"))
      (forward-line -1))
    (unless (bobp) (forward-line 1))
    (point)))


(defun nip-sline-comment-par-end ()
  "Return point at end of current single-line comment paragraph."
  (save-excursion
    (beginning-of-line)
    (while (and (not (eobp))
                (looking-at "^[ \t]*//[ \t]*[[:graph:]]"))
      (forward-line 1))
    (unless (bobp) (backward-char))
    (point)))


(defun nip-sline-comment-offset (line)
  "Return the column at the start of the current single-line
comment paragraph."
  (save-excursion
    (goto-line line)
    (re-search-forward "//" (point-at-eol))
    (goto-char (match-beginning 0))
    (current-column)))


(defun nip-sline-comment-text-offset (line)
  "Return the column at the start of the text of the current
single-line comment paragraph."
  (save-excursion
    (goto-line line)
    (re-search-forward "//[ \t]*" (point-at-eol))
    (current-column)))


(defun nip-at-empty-sline-comment-p ()
  "Return non-nil if inside an empty single-line comment."
  (and (save-excursion
         (beginning-of-line)
         (not (looking-at "^.*//.*[[:graph:]]")))
       (save-excursion
         (re-search-backward "//" (point-at-bol) t))))


(defun nip-fill-sline-comments (parse-status justify)
  "Fill current paragraph as a sequence of single-line comments.
PARSE-STATUS is the result of `parse-partial-regexp' from
beginning of buffer to point. JUSTIFY has the same meaning as in
`fill-paragraph'."
  (when (not (nip-at-empty-sline-comment-p))
    (let* ((start (nip-sline-comment-par-start))
           (start-line (1+ (count-lines (point-min) start)))
           (end (nip-sline-comment-par-end))
           (offset (nip-sline-comment-offset start-line))
           (text-offset (nip-sline-comment-text-offset start-line)))
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


(defun nip-trailing-comment-p (parse-status)
  "Return non-nil if inside a trailing comment. PARSE-STATUS is
the result of `parse-partial-regexp' from beginning of buffer to
point."
  (save-excursion
    (when (nth 4 parse-status)
      (goto-char (nth 8 parse-status))
      (skip-chars-backward " \t")
      (not (bolp)))))


(defun nip-block-comment-p (parse-status)
  "Return non-nil if inside a block comment. PARSE-STATUS is the
result of `parse-partial-regexp' from beginning of buffer to
point."
  (save-excursion
    (save-match-data
      (when (nth 4 parse-status)
        (goto-char (nth 8 parse-status))
        (looking-at "/\\*")))))


(defun niscript-fill-paragraph (&optional justify)
  "If inside a comment, fill the current comment paragraph.
Trailing comments are ignored."
  (interactive)
  (let ((parse-status (parse-partial-sexp (point-min) (point))))
    (when (and (nth 4 parse-status)
               (not (nip-trailing-comment-p parse-status)))
      (if (nip-block-comment-p parse-status)
          (nip-fill-block-comment-paragraph parse-status justify)
        (nip-fill-sline-comments parse-status justify))))
  t)


;; --- Imenu ---

(defconst nip-imenu-generic-expression
  (list
   (list
    nil
    "function\\s-+\\(\\w+\\)\\s-*("
    1))
  "Regular expression matching top level procedures. Used by imenu.")


;; --- Main Function ---

;;;###autoload
(defun niscript-mode ()
  "Major mode for editing niScript source text.

Key bindings:

\\{niscript-mode-map}"
  (interactive)
  (kill-all-local-variables)

  (use-local-map niscript-mode-map)
  (set-syntax-table niscript-mode-syntax-table)

  (set (make-local-variable 'indent-line-function) 'niscript-indent-line)
  (set (make-local-variable 'font-lock-defaults) (list nip-font-lock-keywords))

  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  ;; Comments
  (setq comment-start "// ")
  (setq comment-end "")
  (set (make-local-variable 'fill-paragraph-function)
       'niscript-fill-paragraph)

  ;; Imenu
  (setq imenu-case-fold-search nil)
  (set (make-local-variable 'imenu-generic-expression)
      nip-imenu-generic-expression)

  (setq major-mode 'niscript-mode)
  (setq mode-name "niScript")
  (run-hooks 'niscript-mode-hook))


(provide 'niscript-mode)
;;; niscript.el ends here

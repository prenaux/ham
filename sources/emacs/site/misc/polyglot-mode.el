(defvar polyglot-mode-hook nil)

(defconst polyglot-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; SableCC uses java-style comments.
    ;; Follows the C++ example in the major mode tutorial
    ;; //. /*...*/
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; add underscores as word constituents, prevents odd highlighting
    (modify-syntax-entry ?_ "w" table)
    ;; Only ' should be used as a string delimiter.
    ;; This may require further change since ''' is allowed in
    ;; sablecc, analogous to what would usually be  '\'' in other languages.
    ;; Currently, this will break syntax highlighting etc., so use an alternative to '''
    ;; (let's face it, ''' is a bit odd)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\' "\"'" table)
    (modify-syntax-entry ?\" "w" table)
    ;; there is no magic escape character
    (modify-syntax-entry ?\\ "w" table)
    table))

;; keywords/syntax definitions
;;   literals
(defvar polyglot--syntax-hex "0[xX][[:xdigit:]]+")
;;  constructs/keywords
;;    sections
(defvar polyglot--syntax-sections-keywords
      '("Package"
	"States"
	"Helpers"
	"Tokens"
	"Ignored Tokens"
	"Productions"
	"Abstract Syntax Tree"))
(defvar polyglot--syntax-sections (regexp-opt polyglot--syntax-sections-keywords 'words))
;;  specifiers (T. and P.)
(defvar polyglot--syntax-specifiers "\\(T\\.\\)\\|\\(P\\.\\)")
;;  identifiers, names
(defvar polyglot--syntax-id "\\([a-z]+[a-z0-9_]*[a-z0-9]\\)")
;; todo make polyglot--syntax-name more specific so only valid names are highlighted
(defvar polyglot--syntax-name "{[^{}]*}")
(defvar polyglot--syntax-name-same-element (concat"\\(\\["
						 polyglot--syntax-id
						 "\\]\\):"))
(defvar polyglot--syntax-idfirstuse
  (concat "[[:space:]\n]*"
	   polyglot--syntax-id
	   "[[:space:]\n]*\\("
	   polyglot--syntax-name
	   "\\)?[[:space:]\n]*="))
;;  package ids (split to make it a bit more readable, mimic polyglot- rule)
(defvar polyglot--syntax-packageid "[[:alpha:]][[:alnum:]]+")
(defvar polyglot--syntax-package
  (concat "Package[[:space:]]+\\("
	  polyglot--syntax-packageid
	  "\\(\\."
	  polyglot--syntax-packageid "\\)*\\)"))
;; define font-lock
(defvar polyglot--font-lock
  `(( ,polyglot--syntax-package 1 font-lock-builtin-face)
    ( ,polyglot--syntax-idfirstuse 1 font-lock-variable-name-face)
    ( ,polyglot--syntax-name . font-lock-type-face)
    ( ,polyglot--syntax-name-same-element 1 font-lock-type-face)
    ( ,polyglot--syntax-specifiers . font-lock-function-name-face)
    ( ,polyglot--syntax-sections . font-lock-builtin-face)
    ( ,polyglot--syntax-hex . font-lock-constant-face)))

(define-derived-mode polyglot-mode prog-mode "Polyglot Mode"
 :syntax-table polyglot-mode-syntax-table
 (setq indent-tabs-mode t)
 (setq tab-width 2)
 (set (make-local-variable 'font-lock-defaults) '(polyglot--font-lock))
 (set (make-local-variable 'comment-start) "//")
 (font-lock-fontify-buffer)
 (run-hooks 'polyglot-mode-hook))

(provide 'polyglot-mode)

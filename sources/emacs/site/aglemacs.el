(provide 'aglemacs)
(require 'ni-base)
(require 'diminish)

;;;======================================================================
;;; Files handling...
;;;======================================================================

;; Disable the version control things, slows down stuff and not that usefull
(setq vc-handled-backends nil)

(NotBatchMode
 ;; Editing files larger than ~500K is really too painfull if
 ;; font-lock and a language mode is enabled so just disable it in
 ;; that case.
 (defun ni-find-file-hook-on-file-opened ()
   "If a file is over a given size, make the buffer read only."
   (if (> (buffer-size) (* 1024 1024))
       (progn
         ;; (setq buffer-read-only t)
         ;; (buffer-disable-undo)
         (ni-word-wrap-off)
         (fundamental-mode)
         (setq truncate-lines t)
         (message "Buffer is set to fundamental mode because it is large.")
       )
     (progn
       (ni-word-wrap-on)
     )
   ))

 (add-hook 'find-file-hook 'ni-find-file-hook-on-file-opened)

 (defun ni-delete-file-and-buffer ()
   "Kill the current buffer and deletes the file it is visiting."
   (interactive)
   (let ((filename (buffer-file-name)))
     (when filename
       (if (vc-backend filename)
           (vc-delete-file filename)
         (progn
           (delete-file filename)
           (message "Deleted file %s" filename)
           (kill-buffer))))))


 (OSX
  (defun ni-show-in-finder ()
    (interactive)
    (shell-command (concat "open -R \"" buffer-file-name "\"")))

  (defun ni-open-from-pbpaste ()
    (interactive)
    (let ((filename (shell-command-to-string "pbpaste")))
      (cond
       ((file-exists-p filename)
        (message (concat "Opening file path from clipboard: " filename))
        (find-file filename)
        )
       (t (message "No existing file in clipboard."))))))

 (Windows
  (defun ni-show-in-finder ()
    (interactive)
    (cond
      ;; In buffers with file name
      ((buffer-file-name)
        (shell-command (concat "start explorer /e,/select,\"" (replace-regexp-in-string "/" "\\\\" (buffer-file-name)) "\"")))
      ;; In dired mode
      ((eq major-mode 'dired-mode)
        (shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" (dired-current-directory)) "\"")))
      ;; In eshell mode
      ((eq major-mode 'eshell-mode)
        (shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" (eshell/pwd)) "\"")))
      ;; Use default-directory as last resource
      (t
        (shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" default-directory) "\""))))))

)

;;;======================================================================
;;; Encoding
;;;======================================================================
(agl-begin-time-block "Encoding")

(prefer-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)

(require 'whitespace)
(autoload 'whitespace-mode "whitespace" "Toggle Whitespace viz" t)

(setq nuke-trailing-whitespace-p t)
(add-hook 'before-save-hook 'agl-to-utf8)

(defun agl-to-utf8 ()
  "Cleanup file : remove all ^M, trailing spaces and make sure it's encoded in UTF-8."
  (interactive)
  (let ((cur (point)))
    (beginning-of-buffer)
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8-unix 't)
    (goto-char cur)))

;;;======================================================================
;;; Font lock (syntax highlighting)
;;;======================================================================
(agl-begin-time-block "Font lock (syntax highlighting)")
(global-font-lock-mode t)
(setq font-lock-maximum-size 256000)

;; Improve performance of emacs with long lines
(setq-default bidi-display-reordering nil)

;; global-so-long-mode is available in Emacs 27+
(if (boundp 'global-so-long-mode)
    (global-so-long-mode 1))

;;;======================================================================
;;; C-mode
;;;======================================================================

;; Indent C preprocessor
;; Two functions are provided: 'ppindent-c' and 'ppindent-h' which
;; does not indent the first level, assuming that .h/.hpp files use an
;; #ifdef guard around the entire file.
(require 'ppindent)

;; This should be the same as the default value of `cc-other-file-alist' except
;; for the addition of Objective-C ".m" and ".mm" files.
(setq cc-other-file-alist
      '(("\\.cc\\'"  (".hh" ".h"))
        ("\\.hh\\'"  (".cc" ".C"))

        ("\\.c\\'"   (".h"))
        ("\\.h\\'"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))

        ("\\.m\\'"    (".h"))
        ("\\.mm\\'"    (".h"))

        ("\\.C\\'"   (".H"  ".hh" ".h"))
        ("\\.H\\'"   (".C"  ".CC"))

        ("\\.CC\\'"  (".HH" ".H"  ".hh" ".h"))
        ("\\.HH\\'"  (".CC"))

        ("\\.c\\+\\+\\'" (".h++" ".hh" ".h"))
        ("\\.h\\+\\+\\'" (".c++"))

        ("\\.cpp\\'" (".hpp" ".hh" ".h"))
        ("\\.hpp\\'" (".cpp"))

        ("\\.cxx\\'" (".hxx" ".hh" ".h"))
        ("\\.hxx\\'" (".cxx"))))

(defun ni-add-custom-c-cc-keywords ()
  "adds a few special keywords for c and c++ modes"
  (font-lock-add-keywords
   nil
   '(("\\<\\(niImpl\\|niOverride\\|niVisitor\\|niVisit\\)\\>" . font-lock-keyword-face))
  )
)

(add-hook 'c++-mode-hook 'ni-add-custom-c-cc-keywords)
(add-hook 'c-mode-hook 'ni-add-custom-c-cc-keywords)

;;;======================================================================
;;; Htmlize
;;;======================================================================
(agl-begin-time-block "Htmlize")
;; Makes sure the CSS isnt inlined so we can customize the html's source code color
(require 'htmlize)

;;;======================================================================
;;; Modes
;;;======================================================================
(agl-begin-time-block "Modes")

(setq standard-indent 2)

;;*** DOS BATCH FILES ***************************************************
(require 'batch-mode)

;;*** Bash Script *******************************************************
(add-to-list 'auto-mode-alist '("\\.sed\\'" . sh-mode))

;;*** C/C++ Style *******************************************************
(NotBatchMode
 (require 'talansoft-c-style)
 (add-hook 'c-mode-common-hook 'talansoft-set-c-style)
)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cni\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ixx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp2\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp2\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c2\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h2\\'" . c++-mode))

(setq auto-mode-alist
      (cons '("\\.m$" . objc-mode) auto-mode-alist))

;;*** Java **************************************************************
(NotBatchMode
 (require 'eclipse-java-style)
 (add-hook 'java-mode-hook (lambda () (eclipse-set-java-style)))
)

;;*** Groovy ************************************************************
(NotBatchMode
 (autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
 (defun groovy-custom ()
   "groovy-mode-hook"
   (set (make-local-variable 'tab-width) 4))
 (add-hook 'groovy-mode-hook
           '(lambda() (groovy-custom)))
 (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
 (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
)

;;*** LUA ***************************************************************
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;*** JavaScript ********************************************************
(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsw\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsr\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))

(add-hook 'js-mode-hook (lambda ()
  (modify-syntax-entry ?` "\"" js-mode-syntax-table) ;; Handle backquote in JS
 ))

;;*** Json **************************************************************
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;;*** GraphQL ***********************************************************
(require 'graphql-mode)

;;*** CoffeeScript ******************************************************
(require 'coffee-mode)
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;;*** niScript **********************************************************
(autoload 'niscript-mode "niscript" nil t)
(add-to-list 'auto-mode-alist '("\\.ni\\'" . niscript-mode))
(add-to-list 'auto-mode-alist '("\\.nim\\'" . niscript-mode))
(add-to-list 'auto-mode-alist '("\\.nip\\'" . niscript-mode))
(add-to-list 'auto-mode-alist '("\\.niw\\'" . niscript-mode))
(add-to-list 'interpreter-mode-alist '("ni" . niscript-mode))
(add-to-list 'interpreter-mode-alist '("[a-zA-Z0-9\-_]*-ni" . niscript-mode))

;;*** TypeScript ********************************************************
(autoload 'typescript-mode "typescript" nil t)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;;*** C# ****************************************************************
(autoload 'csharp-mode "csharp-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;;*** VB.NET ************************************************************
(autoload 'vbnet-mode "vbnet-mode" "Visual Basic .NET Mode" t)
(setq auto-mode-alist (append '(("\\.\\(vb\\|frm\\|bas\\|cls\\)$" .
                                 vbnet-mode)) auto-mode-alist))

;;*** VB ****************************************************************
(autoload 'visual-basic-mode "vb-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vba\\'" . visual-basic-mode))
(add-to-list 'auto-mode-alist '("\\.vbs\\'" . visual-basic-mode))

;;*** XML ***************************************************************
;; associate xml, xsd, etc with nxml-mode
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt
                                  '("xml" "xsd" "rng"
                                    "xslt" "xsl" "gml"
                                    "vlk" "level" "plist"
                                    "nish" "niui"
                                   ) t) "\\'")
                   'nxml-mode))
(setq nxml-slash-auto-complete-flag t)
(setq nxml-attribute-indent 2)
(setq nxml-child-indent 2)

;;*** CSS ***************************************************************
(setq css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.rcss\\'" . css-mode))
(autoload 'css-mode "css-mode" nil t)

;;*** ISS ***************************************************************
(autoload 'iss-mode "iss-mode" "Innosetup Script Mode" t)
(add-to-list 'auto-mode-alist '("\\.iss\\'" . iss-mode))

;;*** CG/HLSL/GLSL ******************************************************
(add-to-list 'auto-mode-alist '("\\.cg\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cgc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cgh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hlsl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.metal\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.fsl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.nisl\\'" . c++-mode))

;;*** Actionscript ******************************************************
(autoload 'actionscript-mode "actionscript-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.es\\'" . actionscript-mode))

;;*** Objective-C *******************************************************
(add-to-list 'auto-mode-alist '("\\.mm\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.lnt\\'" . c++-mode))

;;*** Rust **************************************************************
(require 'rust-compile)
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(autoload 'toml-mode "toml-mode" nil t) ;; used by cargo (rust's build system)
(add-to-list 'auto-mode-alist '("\\.toml$" . toml-mode))

;;*** Python ************************************************************
(add-to-list 'auto-mode-alist '("\\wscript$" . python-mode))
(add-to-list 'auto-mode-alist '("\\wscript_build$" . python-mode))
(add-to-list 'auto-mode-alist '("\\SConscript$" . python-mode))
(add-to-list 'auto-mode-alist '("\\SConstruct$" . python-mode))
(add-to-list 'auto-mode-alist '("\\SCsub$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.gd\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\BUCK$" . python-mode))

;; Python Hook, set indent to two spaces...
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

;;*** Cypher (Neo4J) ****************************************************
(NotBatchMode
 (autoload 'cypher-mode "cypher-mode" "Major mode for editing Neo4J Cypher code." t)
 (add-to-list 'auto-mode-alist '("\\.cypher\\'" . cypher-mode))
 (add-to-list 'auto-mode-alist '("\\.cyp\\'" . cypher-mode))
)

;;*** SableCC/Polyglot **************************************************
(autoload 'polyglot-mode "polyglot-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.polyglot\\'" . polyglot-mode))
(add-to-list 'auto-mode-alist '("\\.productions\\'" . polyglot-mode))
(add-to-list 'auto-mode-alist '("\\.sablecc\\'" . polyglot-mode))

;;*** PowerShell Script *************************************************
(autoload 'powershell-mode "powershell-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))

;;*** Nginx config ******************************************************
(autoload 'nginx-mode "nginx-mode" nil t)

;;*** Swift *************************************************************
(autoload 'swift-mode "swift-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))

;;*** Dockerfile ********************************************************
(require 'dockerfile-mode)

;;*** Solidity **********************************************************
(autoload 'solidity-mode "solidity-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.sol\\'" . solidity-mode))

;;*** Clojure ***********************************************************
(autoload 'clojure-mode "clojure-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

;;*** Thrift ************************************************************
(autoload 'thrift-mode "thrift" nil t)
(add-to-list 'auto-mode-alist '("\\.thrift\\'" . thrift-mode))

;;;======================================================================
;;; Utils
;;;======================================================================
(agl-begin-time-block "Utils")

(defun uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))

(defun agl-search-word-backward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (goto-char
     (if (re-search-backward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

(defun agl-search-word-forward ()
  "Find the next occurrance of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (goto-char
     (if (re-search-forward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

(defun agl-comment-and-go-down ()
  "Comments the current line and goes to the next one" (interactive)
  (condition-case nil (comment-region (point-at-bol) (point-at-eol)) (error nil))
  (end-of-line)
  (next-line 1)
  (back-to-indentation))

(defun agl-uncomment-and-go-up ()
  "Uncomments the current line and goes to the previous one" (interactive)
  (condition-case nil (uncomment-region (point-at-bol) (point-at-eol)) (error nil))
  (back-to-indentation)
  (next-line -1))

(defun ni-comment-dwim ()
  "Like 'comment-dwim' when there's a region otherwise comment down."
  (interactive)
  (if (region-active-p)
    (comment-dwim nil)
    (agl-comment-and-go-down)))

(defun agl-increment-number-at-point (&optional amount)
  "Increment the number under point by `amount'"
  (interactive "p")
  (let ((num (number-at-point)))
    (when (numberp num)
      (let ((newnum (+ num amount))
            (p (point)))
        (save-excursion
          (skip-chars-backward "-.0123456789")
          (delete-region (point) (+ (point) (length (number-to-string num))))
          (insert (number-to-string newnum)))
        (goto-char p)))))

(defun agl-decrement-number-at-point (&optional amount)
  (interactive "p")
  "Decrement the number under point by `amount'"
  (agl-increment-number-at-point (- (abs amount))))

(defun agl-increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun agl-decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                  (face-attribute 'default :height)))))

(defun agl-previous-input ()
  "Console previous input"
  (interactive)
  (goto-char (point-max))
  (comint-previous-input 1))

(defun agl-next-input ()
  "Console previous input"
  (interactive)
  (goto-char (point-max))
  (comint-next-input 1))

(defun day-name ()
  (let ((date (calendar-day-of-week
               (calendar-current-date)))) ; 0-6
    (catch 'return
      (case date
        (0
         (throw 'return "Sunday"))
        (6
         (throw 'return "Saturday"))
        (t
         (throw 'return "weekday"))))))

(defun agl-bash-cmd-to-string (aCmd)
  (PrognInHamShell
   (shell-command-to-string aCmd)))

(defun agl-uuid1 ()
  "Generate a type 1 UUID and return it."
  (interactive)
  (agl-bash-cmd-to-string "genuuid 1"))
(defun agl-uuid2 ()
  "Generate a type 2 UUID and return it."
  (interactive)
  (agl-bash-cmd-to-string "genuuid 2"))
(defun agl-uuid3 ()
  "Generate a type 3 UUID and return it."
  (interactive)
  (agl-bash-cmd-to-string "genuuid 3"))

(defun agl-uuid1-to-buffer ()
  "Generate a type 1 UUID and copy it in the clipboard."
  (interactive)
  (insert (agl-uuid1)))
(defun agl-uuid2-to-buffer ()
  "Generate a type 2 UUID and copy it in the clipboard."
  (interactive)
  (insert (agl-uuid2)))
(defun agl-uuid3-to-buffer ()
  "Generate a type 3 UUID and copy it in the clipboard."
  (interactive)
  (insert (agl-uuid3)))

;; Useful for times where indentation doesnt work - common in multiline strings
(defun agl-newline-and-indent-same-level ()
  "Insert a newline, then indent to the same column as the current line."
  (interactive)
  (let ((col (save-excursion
               (back-to-indentation)
               (current-column))))
    (newline)
    (indent-to-column col)))

;;;======================================================================
;;; Time
;;;======================================================================

(defvar ni-iso-datetime-format "%FT%H:%M:%S%Z")

(defun ni-insert-iso-datetime ()
  "insert the current date and time into current buffer."
  (interactive)
  (insert (format-time-string ni-iso-datetime-format (current-time))))

(defun ni-insert-timestamp ()
  "insert a timestamp."
  (interactive)
  (insert "<" (format-time-string ni-iso-datetime-format (current-time)) "> "))

;;;======================================================================
;;; Search / Find in files
;;;======================================================================
(agl-begin-time-block "Search / Find in files")

;; hippie expand functions
;; 28.11.2003: from MicheleBini (emacs wiki page)
;; completes via calc:
;; You must be on the end of the line. The line must end with: " = "
;; Then you can invoke this completion function!!
(defun agl-try-complete-with-calc-result (arg)
  "Try the complete an expression using the calculator"
  (and
   (not arg) (eolp)
   (save-excursion
     (beginning-of-line)
     (when (and (boundp 'comment-start)
		comment-start)
       (when (looking-at
	      (concat
	       "[ \n\t]*"
	       (regexp-quote comment-start)))
	 (goto-char (match-end 0))
	 (when (looking-at "[^\n\t ]+")
	   (goto-char (match-end 0)))))
     (looking-at ".* \\(\\([;=]\\) +$\\)")) ;
   (save-match-data
     (require 'calc nil t))
   ;;(require 'calc-aent)
   (let ((start (match-beginning 0))
	 (op (match-string-no-properties 2)))
   (save-excursion
     (goto-char (match-beginning 1))
     (if (re-search-backward (concat "[\n" op "]") start t)
	 (goto-char (match-end 0)) (goto-char start))
     (looking-at (concat " *\\(.*[^ ]\\) +" op "\\( +\\)$"))
     (goto-char (match-end 2))
     (let* ((b (match-beginning 2))
	    (e (match-end 2))
	    (a (match-string-no-properties 1))
	    (r (calc-do-calc-eval a nil nil)))
       (when (string-equal a r)
	 (let ((b (save-excursion
		    (and (search-backward "\n\n" nil t)
			 (match-end 0))))
	       (p (current-buffer))
	       (pos start)
	       (s nil))
	   (setq r
		 (calc-do-calc-eval
		  (with-temp-buffer
		    (insert a)
		    (goto-char (point-min))
		    (while (re-search-forward
			    "[^0-9():!^ \t-][^():!^ \t]*" nil t)
		      (setq s (match-string-no-properties 0))
		      (let ((r
			     (save-match-data
			       (save-excursion
				 (set-buffer p)
				 (goto-char pos)
				 (and
				  ;; TODO: support for line
				  ;; indentation
				  (re-search-backward
				   (concat "^" (regexp-quote s)
					   " =")
				   b t)
				  (progn
				    (end-of-line)
				    (search-backward "=" nil t)
				    (and (looking-at "=\\(.*\\)$")
					 (match-string-no-properties 1))))))))
			(if r (replace-match (concat "(" r ")") t t))))
		    (buffer-substring (point-min) (point-max)))
		  nil nil))))
       (and
	r
	(progn
	  (he-init-string b e)
	  (he-substitute-string (concat " " r))
	  t)))))))

;;;======================================================================
;;; Move to visible shell
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "MoveToVisibleShell")

 (defun agl-find-visible-shell ()
   "find the first visible shell"
   ;; (save-some-buffers)
   (let ((hasVisibleShell nil)
         (currentFrame (selected-frame))
         (currentWindow (selected-window))
         (currentBufferName (buffer-name)))
     (dolist (elFrame (frame-list))
       ;; (message "... currentBufferName: %s" currentBufferName)
       (select-frame elFrame)
       ;; (message "... frame: %s" elFrame)
       (dolist (elWindow (window-list))
         (select-window elWindow)
         ;; (message "... window: %s" elWindow)
         ;; (message "... window-buffer-name: %s" (buffer-name))
         ;; (message "... window-major-mode: %s" major-mode)
         (if (not hasVisibleShell)
             (if (string= "ham-shell-mode" major-mode)
                 (progn
                   ;; (message "... window-is-visible-shell: %s" major-mode)
                   (set 'hasVisibleShell (list elFrame elWindow)))
             )
           ;; (message "... window-already-ran-command")
         )
       )
     )
     (select-window currentWindow)
     hasVisibleShell
   )
 )

 (defun agl-select-visible-shell-window ()
   "select the first visible shell and make it active"
   (interactive)
   (let ((visibleShell (agl-find-visible-shell)))
     (let ((visibleFrame (car visibleShell))
           (visibleWindow (car (cdr visibleShell))))
       (if visibleShell
           (progn
             (raise-frame visibleFrame)
             (select-window visibleWindow)
             (end-of-buffer)
             visibleWindow
           )
         (progn
           (message "No ham-shell-mode window visible!")
           nil)
       )))
 )

 (defun agl-run-last-shell-command ()
   "select the first visible shell and run the last command ran in it"
   (interactive)
   (if (not (string= "ham-shell-mode" major-mode))
       (save-buffer))
   (save-some-buffers)
   (let ((visibleShellWindow (agl-select-visible-shell-window)))
     (if visibleShellWindow
         (progn
           (agl-previous-input)
           (comint-send-input)
         )
     ))
 )
)

;;;======================================================================
;;; Basic stuffs
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Basic stuffs")

 (setq-default fill-column 78)

 (put 'erase-buffer 'disabled nil)
 (put 'upcase-region 'disabled nil)

 (show-paren-mode t)
 (setq next-line-add-newlins nil)
 ;; Set the characters displayed in the modeline for each of the encoding
 (setq eol-mnemonic-dos ?\\
       eol-mnemonic-unix ?/
       eol-mnemonic-mac ?:
       eol-mnemonic-undecided ??)

 (IsTerminal
  ;; Update the terminal's title bar
  (require 'term-title)
  (term-title-mode)
  ;; Hide the menu bar
  (if (boundp 'menu-bar-mode)
      (menu-bar-mode -1)))

 (if (boundp 'scroll-bar-mode)
     (scroll-bar-mode -1))
 (if (boundp 'tool-bar-mode)
     (tool-bar-mode -1))
 (if (boundp 'tabbar-mode)
     (tabbar-mode -1))
)

;; tab size
(setq default-tab-width 2)
(setq-default tab-width 2)

;; use this to change the indentation offset
(setq c-basic-offset 2)
;; use tabs for indentation (t)
(setq-default indent-tabs-mode nil)

; Show the column number
(column-number-mode nil)

;; get rid of the default messages on startup
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Cursor
(blink-cursor-mode -1)
(setq-default cursor-type 'box)

(transient-mark-mode 1)         ; make the current 'selection' visible
(delete-selection-mode 1)       ; delete the selection area with a keypress
(fset 'yes-or-no-p 'y-or-n-p)   ; enable one letter y/n answers to yes/no
(global-font-lock-mode 1)       ; always do syntax highlighting
(file-name-shadow-mode 1)       ; be smart about filenames (understand ~/ etc.)
(set-language-environment "UTF-8") ; prefer utf-8 for language settings
(setq x-select-enable-clipboard t) ; copy-paste should work
(setq confirm-nonexistent-file-or-buffer nil) ; annoying confirmation if a file or buffer does not exist when you use C-x C-f or C-x b

;; format the title-bar to always include the buffer name
;; (setq frame-title-format "emacs - %b")
;; format the title-bar to show the full path name of the buffer
(NotBatchMode
 (setq-default frame-title-format
              '(:eval
                (format "%s@%s %s"
                        (or (file-remote-p default-directory 'user)
                            user-real-login-name)
                        (or (file-remote-p default-directory 'host)
                            system-name)
                        (cond
                         (buffer-file-truename
                          (concat "[" buffer-file-truename "]"))
                         (dired-directory
                          (concat "{" dired-directory "}"))
                         (t
                          "[no file]")))))
)

;;;======================================================================
;;; Auto-revert
;;;======================================================================
(NotBatchMode

 (setq auto-revert-interval 1)
 (global-auto-revert-mode 1)

 ;; Revert without prompt
 ;; (setq revert-without-query '(".*"))

 ;; Also auto refresh dired, but be quiet about it
 ;; (setq global-auto-revert-non-file-buffers t)
 ;; (setq auto-revert-verbose nil)

 ;; Try to ignore modification-time-only changes in files, i.e. ones that
 ;; don't really change the contents. This happens often with switching
 ;; between different VC buffers.
 (defun update-buffer-modtime-if-byte-identical ()
   (let* ((size      (buffer-size))
          (byte-size (position-bytes size))
          (filename  buffer-file-name))
     (when (and byte-size (<= size 1000000))
       (let* ((attributes (file-attributes filename))
              (file-size  (nth 7 attributes)))
         (when (and file-size
                    (= file-size byte-size)
                    (string= (buffer-substring-no-properties 1 (1+ size))
                             (with-temp-buffer
                               (insert-file-contents filename)
                               (buffer-string))))
           (set-visited-file-modtime (nth 5 attributes))
           t)))))

 (defun verify-visited-file-modtime--ignore-byte-identical (original &optional buffer)
   (or (funcall original buffer)
       (with-current-buffer buffer
         (update-buffer-modtime-if-byte-identical))))
 (advice-add 'verify-visited-file-modtime :around #'verify-visited-file-modtime--ignore-byte-identical)

 (defun ask-user-about-supersession-threat--ignore-byte-identical (original &rest arguments)
   (unless (update-buffer-modtime-if-byte-identical)
     (apply original arguments)))
 (advice-add 'ask-user-about-supersession-threat :around #'ask-user-about-supersession-threat--ignore-byte-identical)
)

;;;======================================================================
;;; Word wrap
;;;======================================================================
(NotBatchMode
 (require 'adaptive-wrap)
 (setq adaptive-wrap-extra-indent 2)

 (set-default 'truncate-partial-width-windows nil)
 (set-default 'truncate-lines t)
 (setq visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))

 (defun ni-word-wrap-on ()
   (interactive)
   (setq truncate-lines t)
   (visual-line-mode t)
   (adaptive-wrap-prefix-mode t)
 )

 (defun ni-word-wrap-off ()
   (interactive)
   (setq truncate-lines nil)
   (visual-line-mode -1)
   (adaptive-wrap-prefix-mode -1)
 )

 (defun ni-word-wrap-toggle ()
   (interactive)
   (if (bound-and-true-p visual-line-mode)
       (ni-word-wrap-off)
     (ni-word-wrap-on))
   (recenter))

 (add-hook 'visual-line-mode-hook
           (lambda ()
             (adaptive-wrap-prefix-mode +1)
             (diminish 'visual-line-mode)))

)

;;;======================================================================
;;; CMake
;;;======================================================================
(NotBatchMode
 (require 'cmake-mode)
 (setq auto-mode-alist
       (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                 ("\\.cmake\\'" . cmake-mode))
               auto-mode-alist)))

;;;======================================================================
;;; Buffer name uniquify
;;;======================================================================
(NotBatchMode
 (require 'uniquify)
 (setq uniquify-buffer-name-style 'forward)
 )

;;;======================================================================
;;; MarkDown
;;;======================================================================
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(NotBatchMode
  (require 'markdown-dnd-images)
  (setq markdown-max-image-size '(400 . 400))
  (setq dnd-save-directory "images/")
  (setq dnd-view-inline t)
  (setq dnd-capture-source nil))

;;;======================================================================
;;; Misc
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Misc")

 ;; kill current buffer without confirmation
 (defun kill-current-buffer ()
   "Kill the current buffer, without confirmation."
   (interactive)
   (save-buffer)
   (kill-buffer (current-buffer)))

 ;; alias qrr to query-replace-regexp
 (defalias 'qrr 'query-replace-regexp)

 ;; alias y to yes and n to no
 (defalias 'yes-or-no-p 'y-or-n-p)
)

;;;======================================================================
;;; mark-multiple & expand-region
;;;======================================================================
(NotBatchMode
 (add-to-list 'load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/multiple-cursors.el"))
 (require 'multiple-cursors)

 (add-to-list 'load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/expand-region.el"))
 (require 'expand-region)

 (setq mc/always-run-for-all t)
 (setq mc/always-repeat-command t)

)

;;;======================================================================
;;; Macros
;;;======================================================================
(NotBatchMode
 (defun save-macro (name)
   "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
   (interactive "Name of the macro :")  ; ask for the name of the macro
   (kmacro-name-last-macro name)         ; use this name for the macro
   (find-file "~/.emacs")                ; open ~/.emacs
   (goto-char (point-max))               ; go to the end of the .emacs
   (newline)                             ; insert a newline
   (insert-kbd-macro name)               ; copy the macro
   (newline)                             ; insert a newline
   (switch-to-buffer nil))               ; return to the initial buffer

 (fset 'macro-join-line
       (lambda (&optional arg)
         "Keyboard macro."
         (interactive "p")
         (kmacro-exec-ring-item
          (quote ([5 67108896 down 134217837 32] 0 "%d")) arg)))
 (OSX
  (defun ni-macos-pbcopy-text (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (defun ni-macos-pbcopy ()
    (interactive)
    (let ((deactivate-mark t))
      (call-process-region (point) (mark) "pbcopy")))

  (defun ni-macos-pbpaste ()
    (interactive)
    (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

  (defun ni-macos-pbcut ()
    (interactive)
    (pbcopy)
    (delete-region (region-beginning) (region-end))))

 (defun ni-clipboard-copy-text (text &optional push)
   (if (fboundp 'ni-macos-pbcopy-text)
       (ni-macos-pbcopy-text text))
   (kill-new text))

 (defun ni-copy-file-path (&optional *dir-path-only-p)
   "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-01-27"
   (interactive "P")
   (let ((-fpath (if (equal major-mode 'dired-mode)
                     (expand-file-name default-directory)
                   (if (buffer-file-name) (buffer-file-name)
                     (user-error "Current buffer is not associated with a file.")))))
     (ni-clipboard-copy-text
      (if *dir-path-only-p
          (progn
            (message "Directory path copied: 「%s」" (file-name-directory -fpath))
            (file-name-directory -fpath))
        (progn
          (message "File path copied: 「%s」" -fpath)
          -fpath )))))
)

;;;======================================================================
;;; Delete without putting it in the kill ring
;;;======================================================================
(NotBatchMode
 (defun agl-delete-word (arg)
   "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
   (interactive "p")
   (delete-region
    (point)
    (progn
      (forward-word arg)
      (point))))

 (defun agl-backward-delete-word (arg)
   "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
   (interactive "p")
   (agl-delete-word (- arg)))

 (defun agl-delete-line ()
   "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
   (interactive)
   (delete-region
    (point)
    (progn (end-of-line 1) (point)))
   (delete-char 1))

 (defun agl-delete-line-backward ()
   "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
   (interactive)
   (let (p1 p2)
     (setq p1 (point))
     (beginning-of-line 1)
     (setq p2 (point))
     (delete-region p1 p2)))
)

;;;======================================================================
;;; Sort words
;;;======================================================================
(defun ni-sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun ni-sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

;;;======================================================================
;;; Transpose arguments
;;;======================================================================

;;; Transpose arguments in c-like mode. Credits: https://emacs.stackexchange.com/a/47934/2671
(defun ni-c-forward-to-argsep ()
  "Move to the end of the current c function argument.
Returns point."
  (interactive)
  (while
    (progn
      (comment-forward most-positive-fixnum)
      (looking-at "[^,)]"))
    (condition-case ex (forward-sexp)
      ('scan-error (if (looking-at "[<>]") ;; likely c++ template
                       (forward-char)
                     (throw ex))))
    )
  (point)
  )

(defun ni-c-backward-to-argsep ()
  "Move to the beginning of the current c function argument.
Returns point."
  (interactive)
  (let ((pt (point)) cur)
    (up-list -1) ;; try to quit first balanced expression
    (while (looking-at "<") ;; c++ template opening bracket
      (up-list -1))
    (forward-char)
    (while
      (progn
        (setq cur (point))
        (> pt (ni-c-forward-to-argsep))
        )
      (forward-char)
      )
    (goto-char cur))
  )
(defun ni-c-transpose-args-direction (is_forward)
  "Transpose two arguments of a c-function.
The first arg is the one with point in it."
  (interactive)
  (let*
    (
      ;; only different to pt when not 'is_forward'
      (pt-original (point))
      (pt
        (progn
          (when (not is_forward)
            (goto-char (- (ni-c-backward-to-argsep) 1))
            (unless (looking-at ",")
              (goto-char pt-original)
              (user-error "Argument separator not found"))
            )
          (point))
        )
      (b (ni-c-backward-to-argsep))
      (sep
        (progn (goto-char pt)
          (ni-c-forward-to-argsep)))
      (e
        (progn
          (unless (looking-at ",")
            (goto-char pt-original)
            (user-error "Argument separator not found"))
          (forward-char)
          (ni-c-forward-to-argsep))
        )
      (ws-first
        (buffer-substring-no-properties
          (goto-char b)
          (progn (skip-chars-forward "[[:space:]\n]")
            (point))
          )
        )
      (first (buffer-substring-no-properties (point) sep))
      (ws-second
        (buffer-substring-no-properties
          (goto-char (1+ sep))
          (progn (skip-chars-forward "[[:space:]\n]")
            (point))
          )
        )
      (second (buffer-substring-no-properties (point) e))
      )
    (delete-region b e)
    (insert ws-first second "," ws-second first)

    ;; Correct the cursor location to be on the same character.
    (if is_forward
      (goto-char
        (+
          ;; word start.
          (- (point) (length first))
          ;; Apply initial offset within the word.
          (- pt b (length ws-first))
          )
        )
      (goto-char
        (+
          b (length ws-first)
          ;; Apply initial offset within the word.
          (- pt-original (+ pt 1 (length ws-second)))
          )
        )
      )
    )
  )

(defun ni-c-transpose-args-forward () (interactive) (ni-c-transpose-args-direction t))
(defun ni-c-transpose-args-backward () (interactive) (ni-c-transpose-args-direction nil))

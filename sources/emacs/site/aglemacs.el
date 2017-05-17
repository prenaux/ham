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
   (if (> (buffer-size) (* 512 1024))
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
   '(("niLambdaEx" . font-lock-keyword-face)
     ("niLambdaByRef" . font-lock-keyword-face)
     ("niLambdaByVal" . font-lock-keyword-face)
     ("niImpl" . font-lock-keyword-face)
     ("niOverride" . font-lock-keyword-face)
     ("fun" . font-lock-keyword-face)
    )
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

;; Used by muse, don't remove
(defun htmlize-region-for-paste (beg end)
  "Htmlize the region and return just the HTML as a string.
This forces the `inline-css' style and only returns the HTML body,
but without the BODY tag.  This should make it useful for inserting
the text to another HTML buffer."
  (let ((htmlbuf (htmlize-region beg end)))
    (unwind-protect
        (with-current-buffer htmlbuf
          (buffer-substring (plist-get htmlize-buffer-places 'content-start)
                            (plist-get htmlize-buffer-places 'content-end)))
      (kill-buffer htmlbuf))))

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
 (require 'google-c-style)
 (add-hook 'c-mode-common-hook 'google-set-c-style)
)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cni\\'" . c++-mode))

(setq auto-mode-alist
      (cons '("\\.m$" . objc-mode) auto-mode-alist))

;;*** Java **************************************************************
(NotBatchMode
 (require 'eclipse-java-style)
 (add-hook 'java-mode-hook (lambda () (eclipse-set-java-style)))
)

;;*** LUA ***************************************************************
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;*** JavaScript ********************************************************
(require 'json-mode)
(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsw\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsr\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js-mode))

;;*** Json **************************************************************
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;;*** CoffeeScript ******************************************************
(require 'coffee-mode)
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;;*** PHP ***************************************************************
(autoload 'php-mode "php-mode-improved" nil t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.php3\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.php4\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.php5\\'" . php-mode))

;;*** niScript **********************************************************
(autoload 'niscript-mode "niscript" nil t)
(add-to-list 'auto-mode-alist '("\\.ni\\'" . niscript-mode))
(add-to-list 'auto-mode-alist '("\\.nim\\'" . niscript-mode))
(add-to-list 'auto-mode-alist '("\\.nip\\'" . niscript-mode))
(add-to-list 'auto-mode-alist '("\\.niw\\'" . niscript-mode))
(add-to-list 'interpreter-mode-alist '("ni" . niscript-mode))

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
                                    "vlk" "level"
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

;;*** YAWS **************************************************************
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

;;*** Actionscript ****************************************************************
(autoload 'actionscript-mode "actionscript-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.es\\'" . actionscript-mode))


;;*** Objective-C *****************************************************************
(add-to-list 'auto-mode-alist '("\\.mm\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.lnt\\'" . c++-mode))

;;*** Rust ************************************************************************
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(autoload 'toml-mode "toml-mode" nil t) ;; used by cargo (rust's build system)
(add-to-list 'auto-mode-alist '("\\.toml$" . toml-mode))

;;*** Python **********************************************************************
(add-to-list 'auto-mode-alist '("\\wscript$" . python-mode))
(add-to-list 'auto-mode-alist '("\\wscript_build$" . python-mode))
(add-to-list 'auto-mode-alist '("\\SConscript$" . python-mode))
(add-to-list 'auto-mode-alist '("\\SConstruct$" . python-mode))
(add-to-list 'auto-mode-alist '("\\SCsub$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.gd\\'" . python-mode))

;; Python Hook, set indent to two spaces...
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

;;;======================================================================
;;; IDO completion
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "IDO completion")

 (ido-mode t)

 ;; Ohhh, yes... please thanks god, this makes sure that ido won't switch to
 ;; an active frame (window) if the buffer is already opened there. It'll just
 ;; open in the current frame - as god intended.
 (setq ido-default-buffer-method 'selected-window)

 ;; Fix the Freeze when having a long list of buffer in the fuzzy
 ;; match and typing a miss-spelled name.
 ;; ---- from http://www.emacswiki.org/emacs/TextMate ----
 ;; I found this had terrible behavior (emacs seizing up) if I typoed
 ;; and my typo was not a match in the TAGS file. The following fixed
 ;; the issue for me:
 ;; https://bitbucket.org/durin42/dotfiles/src/tip/.elisp/settings/50.localfuncs.el#cl-9
 (defvar af-ido-flex-fuzzy-limit (* 2000 5))
 (defadvice ido-set-matches-1 (around my-ido-set-matches-1 activate)
   (let ((ido-enable-flex-matching (< (* (length (ad-get-arg 0)) (length ido-text))
                                      af-ido-flex-fuzzy-limit)))
     ad-do-it))

 (add-hook 'ido-define-mode-map-hook 'ido-my-keys)
 (defun ido-my-keys ()
   (define-key ido-mode-map "\t" 'ido-complete) ; tab is better for completion lists
   (define-key ido-mode-map (kbd "tab") 'ido-complete)
   (define-key ido-mode-map "\C-t" 'ido-toggle-regexp) ; same as in isearch
   (define-key ido-mode-map "\C-d" 'ido-enter-dired))  ; cool

 (setq
  ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
  ido-save-directory-list-file nil
  ido-ignore-buffers '("\\` " "^\*Back" "^\*Compile-Log" "^\*Ido")
  ;; ido-confirm-unique-completion t    ; Wait for RET, even on unique
  ido-everywhere t                      ; Enabled for various dialogs
  ido-case-fold  t                      ; Case-insensitive
  ido-use-filename-at-point nil         ; Use filename at point
  ido-use-url-at-point nil              ; Don't use url at point
  ido-enable-flex-matching t            ; More flexible
  ido-max-prospects 8                   ; Keep minibuffer clean
  ido-create-new-buffer 'always
  ido-enable-tramp-completion nil
  ido-enable-last-directory-history nil
  ido-record-commands nil
  ido-max-work-directory-list 0
  ido-max-work-file-list 0
  )

 ;; Seems there's an issue with th max directory size on OSX
 (GNUEmacs23
  (setq ido-max-directory-size 100000000))
)

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

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert the character typed."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
    ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
    (t                    (self-insert-command (or arg 1))) ))

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
;;; Overlays
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Overlays")
 (require 'custom)

 (defvar all-overlays ())

 (defun agl-delete-this-overlay(overlay is-after begin end &optional len)
   (delete-overlay overlay)
   )

 (defun agl-highlight-current-line()
   (interactive)
   (interactive)
   (setq current-point (point))
   (beginning-of-line)
   (setq beg (point))
   (forward-line 1)
   (setq end (point))
   ;; Create and place the overlay
   (setq error-line-overlay (make-overlay 1 1))
   ;; Append to list of all overlays
   (setq all-overlays (cons error-line-overlay all-overlays))

   (if (= agl-kColorTheme 1)
       (overlay-put error-line-overlay
                    'face '(background-color . "#AAEEAA"))
     (overlay-put error-line-overlay
                  'face '(background-color . "#115511"))
     )

   (overlay-put error-line-overlay
                'modification-hooks (list 'agl-delete-this-overlay))
   (move-overlay error-line-overlay beg end)
   (goto-char current-point)
   )

 (defun agl-delete-all-overlays()
   (interactive)
   (while all-overlays
     (delete-overlay (car all-overlays))
     (setq all-overlays (cdr all-overlays))
     )
   )

 (defun highlight-error-lines(compilation-buffer, process-result)
   (interactive)
   (delete-all-overlays)
   (condition-case nil
       (while t
         (next-error)
         (highlight-current-line)
         )
     (error nil))
   )
)

;;;======================================================================
;;; WindMove
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "WindMove")

 (defconst agl-kMadeFrame 0)

 (defun agl-make-frame () ""
   (interactive)
   (defconst agl-kMadeFrame 1)
   (make-frame-command)
   (tool-bar-mode -1)
   (other-frame 1)
   (ham-shell-unique))

 (defun agl-other-frame () ""
   (interactive)
   (if (< (length (visible-frame-list)) 2)
       (agl-make-frame)
     (other-frame 1)))

 ;; Save the current buffer, switch to the other frame, and run the latest command.
 ;; This works only if the other frame is a shell.
 (defun agl-other-frame-and-run-last-shell-command () ""
   (interactive)
   (save-buffer)
   (agl-other-frame)
   (agl-previous-input)
   (comint-send-input))
)

;;;======================================================================
;;; Basic stuffs
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Basic stuffs")

 (global-auto-revert-mode 1)

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
)

;; tab size
(setq default-tab-width 2)
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

;; Remove the menu bar, scroll bar, tool bar, tab bar...
;; (if (boundp 'menu-bar-mode)
    ;; (menu-bar-mode -1))
(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (boundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (boundp 'tabbar-mode)
    (tabbar-mode -1))

;; no blinkies...
(blink-cursor-mode -1)

(transient-mark-mode 1)         ; make the current 'selection' visible
(delete-selection-mode 1)       ; delete the selection area with a keypress
(fset 'yes-or-no-p 'y-or-n-p)   ; enable one letter y/n answers to yes/no
(global-font-lock-mode 1)       ; always do syntax highlighting
(file-name-shadow-mode 1)       ; be smart about filenames (understand ~/ etc.)
(set-language-environment "UTF-8") ; prefer utf-8 for language settings
(setq x-select-enable-clipboard t) ; copy-paste should work
(setq confirm-nonexistent-file-or-buffer nil) ; annoying confirmation if a file or buffer does not exist when you use C-x C-f or C-x b

;; format the title-bar to always include the buffer name
;;(setq frame-title-format "emacs - %b")
;; format the title-bar to show the full path name of the buffer
(NotBatchMode
 (setq-default
  frame-title-format
  (list '((buffer-file-name "emacs - %f"
                            (dired-directory
                             dired-directory
                             (revert-buffer-function " %b"
                                                     ("%b - Dir:  " default-directory)))))))
)

;;;======================================================================
;;; Word wrap
;;;======================================================================
(NotBatchMode
 (require 'adaptive-wrap)
 (setq adaptive-wrap-extra-indent 2)

 (set-default 'truncate-partial-width-windows nil)
 (set-default 'truncate-lines nil)
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
;;; Autoindent yank
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Autoindent yank")

;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes '(emacs-lisp-mode
                            erlang-mode
                            niscript-mode
                            c-mode c++-mode
                            perl-mode cperl-mode
                            java-mode jde-mode
                            lisp-interaction-mode
                            LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped)")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
    (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))
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
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;;;======================================================================
;;; Misc
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Misc")

 (defun make-agl-expand ()
   (make-hippie-expand-function
    '(agl-try-complete-with-calc-result
      try-expand-dabbrev-visible
      try-expand-dabbrev
      try-expand-dabbrev-all-buffers) t))

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
 (add-to-list 'load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/mark-multiple.el"))

 (require 'inline-string-rectangle)
 (require 'mark-more-like-this)

 (defun mark-next-like-this (arg)
   "Find and mark the next part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
   (interactive "p")
   (unless (or (region-active-p)
               mm/master)
     (er/mark-symbol)
     (error "Nothing marked, marked symbol."))
   (if (< arg 0)
       (mm/remove-mirror (mm/furthest-mirror-after-master)))
   (if (>= arg 0)
       (progn
         (when (null mm/master)
           (mm/create-master (region-beginning) (region-end)))

         (save-excursion
           (goto-char (mm/last-overlay-end))
           (if (= arg 0)
               (mm/remove-mirror (mm/furthest-mirror-after-master)))
           (let ((case-fold-search nil)
                 (master-str (mm/master-substring)))
             (if (search-forward master-str nil t)
                 (mm/add-mirror (- (point) (length master-str)) (point))
               (error "no more found \"%s\" forward"
                      (substring-no-properties master-str))))))))

 (add-to-list 'load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/expand-region.el"))
 (require 'expand-region)
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

 (defun goto-match-paren2 (arg)
   "Go to the matching parenthesis if on parenthesis. Else go to the
   opening parenthesis one level up."
   (interactive "p")
   (cond ((looking-at "\\s\(") (forward-list 1))
         (t
          (backward-char 1)
          (cond ((looking-at "\\s\)")
                 (forward-char 1) (backward-list 1))
                (t
                 (while (not (looking-at "\\s("))
                   (backward-char 1)
                   (cond ((looking-at "\\s\)")
                          (message "->> )")
                          (forward-char 1)
                          (backward-list 1)
                          (backward-char 1)))
                   ))))))
)

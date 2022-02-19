(provide 'ham-flymake)

;;;======================================================================
;;; Flymake
;;;======================================================================
(require 'aflymake)

(defun ham-flymake-get-ham-cmdline (source base-dir)
  (string-match "src/\\(.*\\)\\." source)
  (list (concat (getenv "HAM_HOME") "/bin/ham-flymake")
	    (list "FLYMAKE=1"
              (concat "CHK_SOURCES=" source)
              (concat "FLYMAKE_BASEDIR=" base-dir)
              "check-syntax")))

; Set flymake to start only when saving the buffer
(setq aflymake-no-changes-timeout 999999
      aflymake-start-syntax-check-on-newline nil)

; Error pattern matching :
;   regexp file-idx line-idx col-idx (optional) text-idx(optional), match-end to end of string is error text

(defun ham-flymake-goto-prev-error-disp ()
 "Flymake, goto the previous error and display it in the minibuffer."
  (interactive)
  (aflymake-goto-prev-error)
  (ham-flymake-display-err-minibuf)
)

(defun ham-flymake-goto-next-error-disp ()
 "Flymake, goto the next error and display it in the minibuffer."
  (interactive)
  (aflymake-goto-next-error)
  (ham-flymake-display-err-minibuf)
)

(defun ham-flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (aflymake-current-line-no))
         (line-err-info-list  (nth 0 (aflymake-find-err-info aflymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (aflymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (aflymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (aflymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (aflymake-ler-line (nth (1- count) line-err-info-list))))
          (message "%s:%s: %s" full-file line text)
          )
        )
      (setq count (1- count)))))

(defun ham-flymake-follow-err ()
  "Open the file/line in which the error is described."
  (interactive)
  (let* ((line-no             (aflymake-current-line-no))
         (line-err-info-list  (nth 0 (aflymake-find-err-info aflymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (aflymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (aflymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (aflymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (aflymake-ler-line (nth (1- count) line-err-info-list))))
           ;;(message "%s:%s: %s" full-file line text)
		   (find-file full-file)
		   (goto-line line)
          )
        )
      (setq count (1- count)))))

(defun ham-flymake-goto-prev-error-disp ()
 "Flymake, goto the previous error and display it in the minibuffer."
  (interactive)
  (aflymake-goto-prev-error)
  (ham-flymake-display-err-minibuf)
)

(defun ham-flymake-goto-next-error-disp ()
 "Flymake, goto the next error and display it in the minibuffer."
  (interactive)
  (aflymake-goto-next-error)
  (ham-flymake-display-err-minibuf)
)


;; Return the filename directly, ignore the prefixe
;; Ex: (ham-flymake-create-passthrough "c:/machin/roger.java" "xxx") -> "c:/machin/roger.java"
(defun ham-flymake-create-temp-passthrough (file-name prefix)
  file-name)

;; Generates a tempfile in the temp folder
;; Ex: (ham-flymake-create-temp-intmp "c:/machin/roger.java" "xxx") -> "c:/.tmp/78979879/roger.java"
(defun ham-flymake-create-temp-intmp (file-name prefix)
  (file-truename (expand-file-name (file-name-nondirectory file-name)
                                   (expand-file-name (int-to-string (abs (random))) (aflymake-get-temp-dir)))))

(progn
   (defun ham-flymake-get-temp-filename-extension (ext)
     (cond
      ;; rename the extensions to extensions that get compiled
      ((string= ext "h") "cpp")
      ((string= ext "hpp") "cpp")
      ((string= ext "hxx") "cpp")
      ((string= ext "inl") "cpp")
      ((string= ext "ixx") "cpp")
      ((string= ext "cppm") "cpp")
      (t ext)))

   (defun ham-flymake-get-temp-filename (file-name prefix)
     (let ((dir (file-name-directory file-name))
           (basename (file-name-sans-extension (file-name-nondirectory  file-name)))
           (baseext (file-name-extension file-name))
           (ext (ham-flymake-get-temp-filename-extension (file-name-extension file-name)))
           )
       (concat dir
               "_"
               basename
               "_" (if (string= baseext ext) prefix (concat baseext "_" prefix))
               (and ext (concat "." ext)))))

  (ham-flymake-get-temp-filename "stuff/something.hpp" "aflymake"))

;; Generates a _FILENAME_.EXT tempfile (adds a _ in front so that the Glob of Ham skips it always)
;; Ex: (ham-flymake-create-temp-inplace "c:/machin/roger.java" "xxx") -> "c:/machin/_roger_xxx.java"
(defun ham-flymake-create-temp-inplace (file-name prefix)
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((temp-name (ham-flymake-get-temp-filename file-name prefix)))
    (aflymake-log 3 "create-temp-inplace: file=%s temp=%s" file-name temp-name)
    temp-name))

;;**********************************************************************
;; Flymake - C++
;;**********************************************************************

 (defun ham-flymake-cpp-make-init ()
   (aflymake-simple-make-init-impl 'ham-flymake-create-temp-inplace t t "_build.ham" 'ham-flymake-get-ham-cmdline))

 (push '(".+\\.c$" ham-flymake-cpp-make-init) aflymake-allowed-file-name-masks)
 (push '(".+\\.cc$" ham-flymake-cpp-make-init) aflymake-allowed-file-name-masks)
 (push '(".+\\.cpp$" ham-flymake-cpp-make-init) aflymake-allowed-file-name-masks)
 (push '(".+\\.cxx$" ham-flymake-cpp-make-init) aflymake-allowed-file-name-masks)
 (push '(".+\\.cni$" ham-flymake-cpp-make-init) aflymake-allowed-file-name-masks)
 (push '(".+\\.m$" ham-flymake-cpp-make-init) aflymake-allowed-file-name-masks)
 (push '(".+\\.mm$" ham-flymake-cpp-make-init) aflymake-allowed-file-name-masks)
 (push '(".+\\.h$" ham-flymake-cpp-make-init) aflymake-allowed-file-name-masks)
 (push '(".+\\.hpp$" ham-flymake-cpp-make-init) aflymake-allowed-file-name-masks)
 (push '(".+\\.hxx$" ham-flymake-cpp-make-init) aflymake-allowed-file-name-masks)
 (push '(".+\\.ixx$" ham-flymake-cpp-make-init) aflymake-allowed-file-name-masks)
 (push '(".+\\.cppm$" ham-flymake-cpp-make-init) aflymake-allowed-file-name-masks)

;;**********************************************************************
;; Flymake - Java & Scala
;;**********************************************************************

(defun ham-flymake-make-inplace-init (use-relative-base-dir use-relative-source build-file-name get-cmdline-f)
  "Create syntax check command line for a directly checked source file."
  (let* ((args nil)
         (buildfile-dir      (aflymake-init-find-buildfile-dir buffer-file-name build-file-name)))
    (if buildfile-dir
          (setq args (aflymake-get-syntax-check-program-args buffer-file-name buildfile-dir
                                                            use-relative-base-dir use-relative-source
                                                            get-cmdline-f)))
    args))

(defun ham-flymake-java-init ()
  (ham-flymake-make-inplace-init t t "_build.ham" 'ham-flymake-get-ham-cmdline))

;; do nothing because we compile inplace
(defun ham-flymake-java-cleanup () t)

(push '("\\(.*?\\):\\([0-9]+\\): error: \\(.*?\\)\n" 1 2 nil 2 3 (6 compilation-error-face)) aflymake-err-line-patterns)
(push '("\\(.*?\\):\\([0-9]+\\): warning: \\(.*?\\)\n" 1 2 nil 2 3 (6 compilation-warning-face)) aflymake-err-line-patterns)
(push '(".+\\.java$" ham-flymake-java-init ham-flymake-java-cleanup) aflymake-allowed-file-name-masks)
(push '(".+\\.scala$" ham-flymake-java-init ham-flymake-java-cleanup) aflymake-allowed-file-name-masks)

;;**********************************************************************
;; Flymake - JavaScript
;;**********************************************************************
(require 'aflymake-eslint)

;; There's so many way to run eslint depending on the javascript project setup
;; that we're bow delegating that to ham-lint to figure out. ham-lint looks
;; for a _lint.sh bash script in any of the parent folder and then runs that
;; with the JS file as parameter. The goal of _lint.sh is to run the
;; appropriate linting tool.
(setq aflymake-eslint-executable (concat (getenv "WORK") "/ham/bin/ham-lint"))

(defun ham-flymake-eslint-init ()
  (aflymake-easy-load 'aflymake-eslint-command
                     aflymake-eslint-err-line-patterns
                     ;; this did not work with the 'tempdir, apparently eslint
                     ;; got lost and couldn't find its .eslintrc files
                     'inplace
                     "js"))

;; do nothing because we compile inplace
(defun ham-flymake-eslint-cleanup () t)

(push '(".+\\.js$" ham-flymake-eslint-init ham-flymake-eslint-cleanup) aflymake-allowed-file-name-masks)
(push '(".+\\.jsx$" ham-flymake-eslint-init ham-flymake-eslint-cleanup) aflymake-allowed-file-name-masks)

;;**********************************************************************
;; Flymake - Typescript
;;**********************************************************************
(defconst aflymake-typescript-err-line-patterns
  '(("\\(^[^\\.]+.ts\\)[^(]*(\\([0-9]+\\),\\([0-9]+\\)): \\(.+\\)" 1 2 3 4))) ;; default typescript reporter format

(defun aflymake-typescript-command (filename)
  "Construct a command that flymake can use to run typescript on a file."
  (list aflymake-typescript-executable filename))

(setq aflymake-typescript-executable (concat (getenv "WORK") "/ham/toolsets/nodejs/ts-lint"))

(defun ham-flymake-typescript-init ()
  (aflymake-easy-load 'aflymake-typescript-command
                     aflymake-typescript-err-line-patterns
                     ;; this did not work with the 'tempdir, apparently typescript
                     ;; got lost and couldn't find its .typescriptrc files
                     'inplace
                     "ts"))

;; do nothing because we compile inplace
(defun ham-flymake-typescript-cleanup () t)

(push '(".+\\.ts$" ham-flymake-typescript-init ham-flymake-typescript-cleanup) aflymake-allowed-file-name-masks)

;;**********************************************************************
;; Flymake - Json
;;**********************************************************************
(require 'aflymake-json)

;; If not already installed:
;; > . hat nodejs
;; > npm install -g jsonlint@1.6.x
(Windows
 (setq aflymake-json-executable (concat (getenv "WORK") "/ham/toolsets/nodejs/nt-x86/jsonlint")))
(OSX
 (setq aflymake-json-executable (concat (getenv "WORK") "/ham/toolsets/nodejs/osx-x86/bin/json")))

(defun ham-flymake-json-init ()
  (aflymake-easy-load 'aflymake-json-command
                     aflymake-json-err-line-patterns
                     'inplace
                     "json"))

;; do nothing because we compile inplace
(defun ham-flymake-json-cleanup () t)

(push '(".+\\.json$" ham-flymake-json-init ham-flymake-json-cleanup) aflymake-allowed-file-name-masks)

;;**********************************************************************
;; Flymake - ham-lint
;;**********************************************************************
;;
;; err-line-patterns format:
;;   regexp file-idx line-idx col-idx(optional) text-idx(optional)
;;
;; `-idx` is 1 based, 0 is the whole regexp match
;;
;; match-end to end of string is error text
;;

(require 'aflymake-easy)

(defvar aflymake-ham-lint-executable
  (concat (getenv "WORK") "/ham/bin/ham-lint")
  "The ham-lint executable to use for syntax checking.")

;; TODO add some options for eslint CLI
(defun aflymake-ham-lint-command (filename)
  "Construct a command that flymake can use to run eslint on a file."
  (list aflymake-ham-lint-executable filename))

;; do nothing because we compile inplace
(defun ham-flymake-ham-lint-cleanup () t)

;;*** PHP **************************************************************

;; phpstan error parser
(defvar aflymake-ham-lint-err-line-patterns-php
  '(
    ("^[ ]*\\(.*\.php:\\)\\([0-9]+\\):[ ]*\\(.*\\)" nil 2 nil 3)
    ("PHP Fatal error:[ ]*\\(.*\\) in \\(.*\.php\\) on line \\([0-9]+\\)" nil 3 nil 1)
    ))

(defun ham-flymake-ham-lint-php-init ()
  (aflymake-easy-load 'aflymake-ham-lint-command
                      aflymake-ham-lint-err-line-patterns-php
                      'inplace "php"))

(push '(".+\\.php$" ham-flymake-ham-lint-php-init ham-flymake-ham-lint-cleanup) aflymake-allowed-file-name-masks)

;;*** Haskell **********************************************************

;; haskell error parser
(defvar aflymake-ham-lint-err-line-patterns-haskell
  '(
    ("^[ ]*\\(.*\.hs\\):\\([0-9]+\\):[0-9]+:warning:[ ]*\\(.*\\)" nil 2 nil 3)
    ("^[ ]*\\(.*\.hs\\):\\([0-9]+\\):[0-9]+:error*\\(.*\\)" nil 2 nil 3)
    ))

(defun ham-flymake-ham-lint-haskell-init ()
  (aflymake-easy-load 'aflymake-ham-lint-command
                      aflymake-ham-lint-err-line-patterns-haskell
                      'inplace "hs"))

(push '(".+\\.hs$" ham-flymake-ham-lint-haskell-init ham-flymake-ham-lint-cleanup) aflymake-allowed-file-name-masks)

;;*** Rust *************************************************************

;; resut error parser
(defvar aflymake-ham-lint-err-line-patterns-rust
  '(("^\\(.*\\)\n[ ]+--> \\(.*.rs\\):\\([0-9]+\\):\\([0-9]+\\)$" 2 3 4 1)
    ("^\\(.*.rs\\):\\([0-9]+\\):[0-9]+: [0-9]+:[0-9]+ [a-z]+: \\(.*\\)$" 1 2 nil 3)
    ("^\\(.*.rs\\):\\([0-9]+\\) \\(.*\\)$" 1 2 nil 3)))

(defun ham-flymake-ham-lint-rust-init ()
  (interactive)
  (aflymake-easy-load 'aflymake-ham-lint-command
                      aflymake-ham-lint-err-line-patterns-rust
                      'inplace "rs"))

(push '(".+\\.rs$" ham-flymake-ham-lint-rust-init ham-flymake-ham-lint-cleanup) aflymake-allowed-file-name-masks)

;;*** niScript *********************************************************

;; niscript lint warning
;; (regexp file-idx line-idx col-idx(optional) text-idx(optional))
(defvar aflymake-ham-lint-err-line-patterns-niscript
  '(("^.*Lint: \\(.*\\) \\[\\(.*\\):L\\([0-9]+\\)\\].*$" 2 3 nil 1)
    ("^.*\\[\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)\\] compile error: \\(.*\\)$" 1 2 3 4)))

(defun ham-flymake-ham-lint-niscript-init ()
  (interactive)
  (aflymake-easy-load 'aflymake-ham-lint-command
                      aflymake-ham-lint-err-line-patterns-niscript
                      'inplace "niscript"))

(push '(".+\\.ni$" ham-flymake-ham-lint-niscript-init ham-flymake-ham-lint-cleanup) aflymake-allowed-file-name-masks)
(push '(".+\\.nip$" ham-flymake-ham-lint-niscript-init ham-flymake-ham-lint-cleanup) aflymake-allowed-file-name-masks)
(push '(".+\\.niw$" ham-flymake-ham-lint-niscript-init ham-flymake-ham-lint-cleanup) aflymake-allowed-file-name-masks)

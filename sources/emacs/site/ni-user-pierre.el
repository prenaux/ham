;;
;; Example .emacs used in OSX:
;;
;; (setenv "WORK" "/Users/pierre/Documents/Work")
;; (setenv "HAM_HOME" "/Users/pierre/Documents/Work/ham")
;; (setenv "EMACS_DEVENV" (getenv "HAM_HOME"))
;;
;; (add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs/site"))
;;
;; (require 'ni-user-pierre)
;;

(provide 'ni-user-pierre)

(require 'aglemacs)
(require 'ni-org)
(require 'ni-muse)
(require 'ni-templates)
(require 'ni-file-cache)
(require 'ni-ham)
(require 'ni-flymake)
(require 'ham-flymake)
(require 'go-mode)

(NotBatchMode
 (require 'ni-sql)
)

;;;======================================================================
;;; Server for single instance run
;;;======================================================================
(NotBatchMode
 (Windows
  ;;
  ;; On Windows:
  ;;  1) gnuserv & e.exe don't work anymore with Emacs 24.x - as in it'll kind of work but a lot of things will be broken (such as magit, etc... great ^^)
  ;;  2) Use emacsclientw.exe to open files
  ;;
  (require 'server)
  (unless (server-running-p)
    (server-start))))

;;;======================================================================
;;; Font Lock
;;;======================================================================
(NotBatchMode
 ;; (ni-turn-off-christmas) if you don't like syntax highlighting
 (defun ni-turn-off-christmas ()
   (interactive)
   ;; turn off christmas
   (progn
     (setq font-lock-maximum-decoration 1)
     (global-font-lock-mode t)
     (setq font-lock-maximum-decoration
           '((c-mode . 1)
             (c++-mode . 1)
             (niscript-mode . 1)
             (js-mode . 1)
             (-mode . 1)
            ))))
)

;;;======================================================================
;;; Disable all the auto-indent, eletric mode and other BS that drives me nuts
;;;======================================================================
(NotBatchMode
 (setq c-electric-pound-behavior nil)
 (setq css-electric-keys nil)
 (setq minibuffer-electric-default-mode nil)
 (setq xml-lite-electric-slash nil)
 ;; Ohhh yes, don't beep you F***... errr mmm...
 (setq ring-bell-function #'ignore)
)

;;;======================================================================
;;; Keyboard
;;;======================================================================
(NotBatchMode

 ;; Compile command
 (defun save-all-and-compile ()
   (interactive)
   (save-some-buffers 1)
   (recompile))

 ;; Set compile mode to scroll to the first error
 (setq compilation-scroll-output 'first-error)
 ;; Set compile mode to scroll with the output
 (setq compilation-scroll-output t)

 ;; Yes... close everything... but not the buffers
 (defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
   (let (orig-one-window-p)
     (fset 'orig-one-window-p (symbol-function 'one-window-p))
     (fset 'one-window-p (lambda (&optional nomini all-frames) t))
     (unwind-protect
         ad-do-it
       (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

 ;; Cancel the mini buffer if it loses focus after clicking the mouse or
 ;; when switching to another window with C-1/2
 (defun stop-using-minibuffer ()
   "kill the minibuffer"
   (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
     (abort-recursive-edit)))

 (defun my-other-window ()
   ""
   (interactive)
   (stop-using-minibuffer)
   (other-window 1))
)

;;;======================================================================
;;; Backups
;;;======================================================================
(NotBatchMode
 ;;
 ;; Disable Emacs's built-in backup system and hook our own function after
 ;; save so that we have a simple and reliable backup system everytime we save
 ;; a file.
 ;;
 ;; Note that this will backup all files saved with Emacs, this could be
 ;; improved by filtering somehow so that sensitive files aren't backed up.
 ;;

 (setq make-backup-files nil) ; stop creating those backup~ files
 (setq auto-save-default nil) ; stop creating those #autosave# files

 (defun ni-backup-file-name (fpath)
   "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
   (let* (
          (backupRootDir (concat ENV_WORK "/_emacs_bak/"))
          (filePath (replace-regexp-in-string ":" "" fpath)) ; remove ':' from path
          (backupFilePath
           (replace-regexp-in-string
            "//" "/"
            (concat backupRootDir
                    (replace-regexp-in-string
                     "/" "_"
                     (concat filePath "."
                             ;; (format-time-string "bak_%Y%m%d_%H%M") ;; Use the current time as save stamp
                             (ham-hash-file-md5 fpath) ;; Use a MD5 of the buffer as save stamp
                     )))))
         )
     (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
     backupFilePath
   )
 )

 (defun ham-hash-file-md5 (fpath)
   (agl-bash-cmd-to-string (concat "hash_md5 \"" fpath "\"")))

 (defun ni-backup-current-file-handler ()
   (let ((destBackupFileName (ni-backup-file-name buffer-file-name)))
     (if (not (file-exists-p destBackupFileName))
         (progn
           (copy-file buffer-file-name destBackupFileName t)
           ;; (message (concat "made backup: " destBackupFileName))
         )
       (progn
         (agl-bash-cmd-to-string (concat "touch \"" (ni-backup-file-name buffer-file-name) "\""))
         ;; (message (concat "already backed up: " destBackupFileName))
       )
     )))

 (add-hook 'after-save-hook 'ni-backup-current-file-handler)
)

;;;======================================================================
;;; Search in files
;;;======================================================================
(NotBatchMode
 (require 'pt)
 (Windows
  (setq pt-executable (concat "\"" HAM_HOME "/bin/nt-x86/pt.exe" "\"")))
 (OSX
  (setq pt-executable (concat "\"" HAM_HOME "/bin/osx-x86/pt"  "\"")))
 (Linux
  (setq pt-executable (concat "\"" HAM_HOME "/bin/lin-x64/pt"  "\"")))
)

;;;======================================================================
;;; Proper handling of automatic window splits
;;;======================================================================
(NotBatchMode

 ;; Makes sure that compilations, occur, etc. don't split the window
 ;; vertically when creating their output buffer.
 ;;
 ;; - Solution found at: http://stackoverflow.com/questions/6619375/how-can-i-tell-emacs-to-not-split-the-window-on-m-x-compile-or-elisp-compilation
 ;;
 ;;     My guess is that you want to customize the 'split-window-preferred-function'
 ;;     variable. The default value is split-window-sensibly. Uou should change it
 ;;     to a custom version which just switches the current buffer.
 ;;
 (defun no-split-window () (interactive) nil)
 (setq split-window-preferred-function 'no-split-window)
)

;;;======================================================================
;;; Font
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Set Font")

 (Windows
  ;; for 1080p
  (set-face-attribute 'default nil :family "Consolas" :height 105 :weight 'regular)
  ;; for Unicode support
  (set-fontset-font
   "fontset-default" 'unicode
   "-outline-Arial Unicode MS-normal-normal-normal-sans-*-*-*-*-p-*-gb2312.1980-0")
 )
)

;;;======================================================================
;;; Look & Customizations
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Look & Customizations")

 (setq custom-file "~/emacs.d/my-custom.el")

 ;; mode line
 (setq default-mode-line-format
       (list "%Z"
             'mode-line-modified
             " %b "
             'global-mode-string
             "- %[(" 'mode-name
             'minor-mode-alist
             "%n"
             'mode-line-process
             ")%] -"
             " L%l C%c - " ;; C%c to add the column number
             '(-3 . "%p")
             " -%-"))
)

;;;======================================================================
;;; Buffer cleanup and indentation
;;;======================================================================
(agl-begin-time-block "Buffer cleanup and indentation")

(defun xsteve-remove-control-M ()
  "Remove ^M at end of line in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward (concat (char-to-string 13) "$") (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
      ))))

(defun dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't)
  (xsteve-remove-control-M))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun my-indent-buffer ()
  (interactive)
  ;; (begining-of-buffer)
  (untabify (point-min) (point-max))
  (dos2unix)
  (remove-dos-eol)
  (indent-region (point-min) (point-max)))

(defun my-indent-marked-files ()
  (interactive)
  (setq num-files (length (dired-get-marked-files)))
  (setq count 1)
  (dolist (file (dired-get-marked-files))
    (message (format "Indenting %s (%d/%d)" file count num-files))
    (find-file file)
    (ignore-errors
      (my-indent-buffer))
    (save-buffer)
    (kill-buffer nil)
    (setq count (+ count 1))
  )
  (message (format "Done indenting %d files" num-files))
)

;;;======================================================================
;;; --- Disable unneeded warnings ---
;;;======================================================================
(agl-begin-time-block "Warnings")
(put 'dired-find-alternate-file 'disabled nil)

;;;======================================================================
;;; Rainbow delimiters
;;;======================================================================
(agl-begin-time-block "Rainbow")
(require 'rainbow-delimiters)
(add-hook 'niscript-mode-hook 'rainbow-delimiters-mode)
(add-hook 'c++-mode-hook 'rainbow-delimiters-mode)
(add-hook 'c-mode-hook 'rainbow-delimiters-mode)
(add-hook 'js-mode-hook 'rainbow-delimiters-mode)
(add-hook 'niscript-mode-hook 'rainbow-delimiters-mode)

;;;======================================================================
;;; Jump to line with feedback
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Jump to line")

 (defun goto-line-with-feedback ()
   "Show line numbers temporarily, while prompting for the line number input"
   (interactive)
   (unwind-protect
       (progn
         (linum-mode 1)
         (goto-line (read-number "Goto line: ")))
     (linum-mode -1)))
)

;;;======================================================================
;;; Web mode
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Web mode")

 (require 'web-mode)

 (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.htm?\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))

 (setq web-mode-enable-current-element-highlight t)
 (setq web-mode-enable-auto-quoting nil)

 (set-face-attribute 'web-mode-current-element-highlight-face nil :background "LightYellow2")
 (set-face-attribute 'web-mode-current-column-highlight-face nil :background "LightYellow2")

 (require 'yaml-mode)
 (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

 (require 'haml-mode)
 (add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

 (require 'scss-mode)
 (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

 (require 'less-css-mode)
 (add-to-list 'auto-mode-alist '("\\.scss$" . less-css-mode))

 ;; web-mode please close all the tags...
 (setq web-mode-void-elements '())
 (setq web-mode-enable-auto-indentation nil)

 ;; for flow errors in compile buffer (F5 & C-F5)
 (add-to-list 'compilation-error-regexp-alist '("^\\(.*?\\):\\([0-9]+\\):.*$" 1 2))
)

;;;======================================================================
;;; Autocomplete
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "auto-complete")

 ;; pabbrev by default
 (require 'ni-autocomplete-pabbrev)

 ;; company-mode only when Ctrl+/ is pressed
 (require 'ni-autocomplete-company)
 (setq company-ni-idl-merge-dabbrev  nil)

 ;; put the following in your .emacs if you want company to show up automatically without pressing Ctrl+/
 ;; (setq company-idle-delay-default 0.15)
 ;; (setq company-ni-idl-merge-dabbrev  t)

 ;; you'll want this in your .emacs you can also call it by hand when you want
 ;; if it takes too long for you to have at every emacs startup
 ;; (ni-idl-build-cache)
)

;;;======================================================================
;;; Direx (dired tree view) & Dired setup
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "direx")
 (require 'direx)

 (add-hook 'dired-mode-hook 'ni-set-dired-buffer-name)
 (defun ni-set-dired-buffer-name ()
   "change buffer name to end with slash"
   (let ((name (buffer-name)))
     (if (not (string-match "/$" name))
         (rename-buffer (concat "*dired: " name "/*") t))))
)

;;;======================================================================
;;; Diff
;;;======================================================================
(NotBatchMode

 ;; diff-region* - Diff two regions
 ;;
 ;;  To compare two regions, select the first region
 ;; and run `ni-diff-region-select`.  The region is now copied
 ;; to a seperate diff-ing buffer.  Next, navigate
 ;; to the next region in question (even in another file).
 ;; Mark the region and run `diff-region-now`, the diff
 ;; of the two regions will be displayed by ediff.
 ;;
 ;;  You can re-select the first region at any time
 ;; by re-calling `ni-diff-region-select`.
 (defun ni-diff-region-select ()
   "Select a region to compare"
   (interactive)
   (when (use-region-p)  ; there is a region
     (let (buf)
       (setq buf (get-buffer-create "*Diff-regionA*"))
       (save-current-buffer
         (set-buffer buf)
         (erase-buffer))
       (append-to-buffer buf (region-beginning) (region-end)))
   )
   (message "Now select other region to compare and run `ni-diff-region-now`")
 )

 (defun ni-diff-region-now ()
   "Compare current region with region already selected by `diff-region`"
   (interactive)
   (when (use-region-p)
     (let (bufa bufb)
       (setq bufa (get-buffer-create "*Diff-regionA*"))
       (setq bufb (get-buffer-create "*Diff-regionB*"))
       (save-current-buffer
         (set-buffer bufb)
         (erase-buffer))
       (append-to-buffer bufb (region-beginning) (region-end))
       (ediff-buffers bufa bufb))
   )
 )
)

;;;======================================================================
;;; Git
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Git")
 (Windows
  (add-to-list 'exec-path (concat (getenv "HAM_HOME") "/toolsets/repos/nt-x86/git/bin/"))
 )
 (require 'git)
 (require 'git-blame)
)

;;;======================================================================
;;; Flymake hooks
;;;======================================================================
(add-hook 'js-mode-hook 'flymake-mode)
(add-hook 'typescript-mode-hook 'flymake-mode)

;;;======================================================================
;;; Keymap
;;;======================================================================
(NotBatchMode
 (require 'ni-keymap-pierre))

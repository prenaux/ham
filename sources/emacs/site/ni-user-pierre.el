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
(require 'ni-templates)
(require 'ni-file-cache)
(require 'ni-ham)
(require 'ni-flymake)
(require 'ni-gdb)
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
  ;;
  ;;  1) gnuserv & e.exe don't work anymore with Emacs 24.x - as in it'll kind
  ;;     of work but a lot of things will be broken (such as magit, etc... great
  ;;     ^^)
  ;;
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
             (java-mode . 1)
             (-mode . 1)
            ))))

 ;; (ni-turn-on-christmas) if you like syntax highlighting
 (defun ni-turn-on-christmas ()
   (interactive)
   ;; turn on christmas
   (progn
     (setq font-lock-maximum-decoration 3)
     (global-font-lock-mode t)
     (setq font-lock-maximum-decoration
           '((c-mode . 3)
             (c++-mode . 3)
             (niscript-mode . 3)
             (js-mode . 3)
             (java-mode . 3)
             (-mode . 3)
            ))))
 )

;;;======================================================================
;;; Disable all the auto-indent, eletric mode and other BS that drives me nuts
;;;======================================================================
(NotBatchMode
 (setq max-lisp-eval-depth 1000000)
 (setq max-specpdl-size 20000)
 (setq tab-width 2)
 (setq default-tab-width 2)
 (setq c-electric-pound-behavior nil)
 (setq css-electric-keys nil)
 (setq minibuffer-electric-default-mode nil)
 (setq xml-lite-electric-slash nil)
 ;; Ohhh yes, don't beep you F***... errr mmm...
 (setq ring-bell-function #'ignore)

 ;; Turn off the "hover" tooltip when hovering the modeline, because the
 ;; tooltip box stays even after switching app on macOS
 (setq show-help-function nil)

 (setq org-startup-folded nil)

 (setq-default electric-indent-inhibit t)

 ;; Eldoc "Show function arglist or variable docstring in echo area".
 ;; Not useful to me and slows down the editor.
 (when (fboundp 'global-eldoc-mode)
   (global-eldoc-mode -1))
)

;;;======================================================================
;;; Setup sort
;;;======================================================================
(NotBatchMode

 ;; by default ignore case when sorting
 (custom-set-variables
  '(sort-fold-case t t)
  )

 (defun sort-lines-nocase ()
   (interactive)
   (let ((sort-fold-case t))
     (call-interactively 'sort-lines)))

 (defun sort-lines-case ()
   (interactive)
   (let ((sort-fold-case nil))
     (call-interactively 'sort-lines)))

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

 ;;; Use pt on Windows & Linux
 (Windows
  (setq pt-executable (concat "\"" HAM_HOME "/bin/nt-x86/pt.exe" "\"")))
 (Linux
  (setq pt-executable (concat "\"" HAM_HOME "/bin/lin-x64/pt"  "\"")))
 (setq pt-arguments
      (list
       "--smart-case" "-e"
       "--ignore" "\"node_modules\""
       "--ignore" "\"*.min.js\""
       "--ignore" "\"*.min.css\""
       "--ignore" "\"_*_ModuleDef.*\""
       "--ignore" "\"_*_JNI.*\""
       "--ignore" "\"*.idl.inl\""
       "--ignore" "\"*.idl.xml\""
       "--ignore" "\"static/build/*\""
       ))

 ;;; Use hamgrep on macOS
 (OSX
  (if (string-match "^aarch64-.*" system-configuration)
      (setq pt-executable (concat "\"" HAM_HOME "/bin/osx-arm64/hamgrep"  "\""))
    (setq pt-executable (concat "\"" HAM_HOME "/bin/osx-x64/hamgrep"  "\"")))
  (setq pt-arguments (list "--smart-case")))

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
 (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
 (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

 (setq web-mode-enable-current-element-highlight t)
 (setq web-mode-enable-auto-quoting nil)

 (set-face-attribute 'web-mode-current-element-highlight-face nil :background "LightYellow2")
 (set-face-attribute 'web-mode-current-column-highlight-face nil :background "LightYellow2")

 (defun pierre-yaml-mode ()
   (interactive)
   (text-mode)
   (require 'highlight-indentation)
   (highlight-indentation-mode 1)
   (set-variable 'tab-width 2))
 (add-to-list 'auto-mode-alist '("\\.yml$" . pierre-yaml-mode))
 (add-to-list 'auto-mode-alist '("\\.yaml$" . pierre-yaml-mode))
 (add-to-list 'auto-mode-alist '("\\.yaml[a-z]*$" . pierre-yaml-mode))

 (require 'haml-mode)
 (add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

 (require 'scss-mode)
 (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

 (require 'less-css-mode)
 (add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

 ;; web-mode please close all the tags...
 (setq web-mode-void-elements '())
 (setq web-mode-enable-auto-indentation nil)

 ;; skip warnings
 ;; (setq 'compilation-skip-threshold 2)
)

;;;======================================================================
;;; Autocomplete
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "auto-complete")

 ;; pabbrev by default
 (require 'ni-autocomplete-pabbrev)

 ;; company-mode only when Ctrl+/ is pressed
 ;; Note: Disabled because its just not very useful and just makes things
 ;; slower and sometimes break macros when its triggered
 ;; (require 'ni-autocomplete-company)
 ;; (setq company-ni-idl-merge-dabbrev  nil)

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
  (add-to-list 'exec-path (concat (getenv "HAM_HOME") "/toolsets/repos/nt-x86/git/bin/")))

 (require 'git)
 (require 'git-blame)

 (GNUEmacs26
  (add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs/site/magit"))
  (require 'magit))
)

;;;======================================================================
;;; Flymake hooks
;;;======================================================================
(add-hook 'js-mode-hook 'aflymake-mode)
(add-hook 'typescript-mode-hook 'aflymake-mode)

;;;======================================================================
;;; Dark mode
;;;======================================================================
(NotBatchMode
 (set '--dark-mode-state "initial")
 (add-to-list 'custom-theme-load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/themes"))

 (defun ni-theme-set-light-mode ()
   (interactive)
   (set '--dark-mode-state "light")
   (load-theme 'solarized-light t)
   (set-cursor-color "#cc0000")
   (message "Changed to light mode"))

 (defun ni-theme-set-dark-mode ()
   (interactive)
   (set '--dark-mode-state "dark")
   ;;(load-theme 'solarized-dark t)
   (load-theme 'vscode-dark-plus t)
   (set-cursor-color "#cc0000")
   (message "Changed to dark mode"))

 (defun ni-dark-mode-update-theme ()
   (interactive)
   (require 'ni-theme-solarized)
   (ni-check-dark-mode
    (lambda ()
      (if (not (string= --dark-mode-state "dark"))
          (progn (ni-theme-set-dark-mode))
        (message "Is already dark mode")))
    (lambda ()
      (if (not (string= --dark-mode-state "light"))
          (progn (ni-theme-set-light-mode))
        (message "Is already light mode")))
    ))
)

;;;======================================================================
;;; Ivy
;;;======================================================================
(NotBatchMode
 (require 'ni-ivy))

;;;======================================================================
;;; Projectile
;;;======================================================================
(NotBatchMode
 (require 'projectile)
 (projectile-mode +1)
 (setq projectile-completion-system 'ivy)
)

;;;======================================================================
;;; Treemacs
;;;======================================================================
(NotBatchMode
 (add-to-list 'load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/treemacs"))
 (require 'treemacs)
 (require 'treemacs-projectile)

 (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
       treemacs-deferred-git-apply-delay        0.5
       treemacs-directory-name-transformer      #'identity
       treemacs-display-in-side-window          t
       treemacs-eldoc-display                   t
       treemacs-file-event-delay                5000
       treemacs-file-extension-regex            treemacs-last-period-regex-value
       treemacs-file-follow-delay               0.2
       treemacs-file-name-transformer           #'identity
       treemacs-follow-after-init               t
       treemacs-expand-after-init               t
       treemacs-git-command-pipe                ""
       treemacs-goto-tag-strategy               'refetch-index
       treemacs-indentation                     2
       treemacs-indentation-string              " "
       treemacs-is-never-other-window           nil
       treemacs-max-git-entries                 5000
       treemacs-missing-project-action          'ask
       treemacs-move-forward-on-expand          nil
       treemacs-no-png-images                   nil
       treemacs-no-delete-other-windows         t
       treemacs-project-follow-cleanup          nil
       treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
       treemacs-position                        'left
       treemacs-read-string-input               'from-child-frame
       treemacs-recenter-distance               0.1
       treemacs-recenter-after-file-follow      nil
       treemacs-recenter-after-tag-follow       nil
       treemacs-recenter-after-project-jump     'always
       treemacs-recenter-after-project-expand   'on-distance
       treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
       treemacs-show-cursor                     nil
       treemacs-show-hidden-files               t
       treemacs-silent-filewatch                nil
       treemacs-silent-refresh                  nil
       treemacs-sorting                         'alphabetic-asc
       treemacs-select-when-already-in-treemacs 'move-back
       treemacs-space-between-root-nodes        t
       treemacs-tag-follow-cleanup              t
       treemacs-tag-follow-delay                1.5
       treemacs-text-scale                      nil
       treemacs-user-mode-line-format           nil
       treemacs-user-header-line-format         nil
       treemacs-width                           35
       treemacs-width-is-initially-locked       nil
       treemacs-workspace-switch-cleanup        nil)

 (treemacs-follow-mode t)

 (defun ni-treemacs-find-file () ""
        (interactive)
        (treemacs-find-file)
        (treemacs-select-window))

 ;; You might need to run `M-x all-the-icons-install-fonts` and restart emacs
 ;; for the font icons to be setup properly.
 (IsNotTerminal
  (require 'all-the-icons)
  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons"))
)

;;;======================================================================
;;; Keymap
;;;======================================================================
(NotBatchMode
 (require 'ni-keymap-pierre))

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
(require 'ham-fix)
(require 'ham-grep)
(require 'go-mode)
(require 's)

(NotBatchMode
 (require 'ni-sql)
 (require 'ni-haskell)
 (require 'ni-theme)
 (require 'tramp-term)
)

;;;======================================================================
;;; Server for single instance run
;;;======================================================================
(NotBatchMode
 (require 'server)

 ;; (server-force-delete) ;; Stop the server

 (defun ni-emacs-server-start ()
  (interactive)
  (unless (server-running-p)
    (server-start)))
)

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
             (csharp-mode . 1)
             (rust-mode . 1)
             (haskell-mode . 1)
             (python-mode . 1)
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
             (csharp-mode . 3)
             (niscript-mode . 3)
             (js-mode . 3)
             (java-mode . 3)
             (csharp-mode . 3)
             (rust-mode . 3)
             (haskell-mode . 3)
             (python-mode . 3)
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

 ;; Auto-detect indentation
 (require 'dtrt-indent)
 ;; (dtrt-indent-global-mode 1)
 ;; (diminish 'dtrt-indent-mode)
 ;; (diminish-undo 'dtrt-indent-mode)

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
 ;; Is our default compilation command
 (setq compile-command "ham-lint")

 ;; Compilation buffer opens with word wrap on
 (defun ni-pierre-compilation-mode-hook ()
   (ni-word-wrap-on))
 (add-hook 'compilation-mode-hook 'ni-pierre-compilation-mode-hook)

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

 ;; Show terminal colors in compilation buffer
 (GNUEmacsMin28
    (require 'ansi-color)
    (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))
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
   (let* ((backupRootDir (file-name-as-directory ENV_EMACS_BAK_DIR))
           (filePath (replace-regexp-in-string ":" "" (buffer-file-name))) ; remove ':' from path
           (backupFileDir (concat (replace-regexp-in-string "/" "_" (system-name)) "/"
                            (string-trim (replace-regexp-in-string "/" "_"
                                           (file-name-directory filePath))
                              "_+" "_+")))
           (backupFileName (replace-regexp-in-string "\\." "_"
             (file-name-sans-extension (file-name-nondirectory filePath))))
           (backupFileExt (file-name-extension filePath))
           (backupFilePath (concat backupRootDir (replace-regexp-in-string "//" "/"
                             (concat backupFileDir "/" backupFileName "--"
                               (ham-hash-file-md5 filePath)
                               (if (s-blank-str? backupFileExt)
                                 "" (concat "." backupFileExt))
                               ".txt")))))
     (make-directory (file-name-directory backupFilePath)
       (file-name-directory backupFilePath))
     ;; (message (concat "... BACKUP: " backupFilePath))
     backupFilePath))

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
 ;; (remove-hook 'after-save-hook 'ni-backup-current-file-handler)
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
  (message (format "Done indenting %d files" num-files)))

;;;======================================================================
;;; --- Disable unneeded warnings ---
;;;======================================================================
(agl-begin-time-block "Warnings")
(put 'dired-find-alternate-file 'disabled nil)

;;;======================================================================
;;; Rainbow delimiters
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Rainbow")
 (require 'rainbow-delimiters)
 (add-hook 'niscript-mode-hook 'rainbow-delimiters-mode)
 (add-hook 'c++-mode-hook 'rainbow-delimiters-mode)
 (add-hook 'c-mode-hook 'rainbow-delimiters-mode)
 (add-hook 'js-mode-hook 'rainbow-delimiters-mode)
 (add-hook 'niscript-mode-hook 'rainbow-delimiters-mode)
)

;;;======================================================================
;;; Jump to line with feedback
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Jump to line")
 (require 'goto-line-preview)

 ;; (setq goto-line-preview-before-hook '())
 ;; (setq goto-line-preview-after-hook '())

 (add-hook 'goto-line-preview-before-hook
   (lambda ()
     (IsNotTerminal
       (setq --goto-line-linum-mode-was-active (bound-and-true-p linum-mode))
       (linum-mode 1))))

 (add-hook 'goto-line-preview-after-hook
   (lambda ()
     (IsNotTerminal
       (if (not --goto-line-linum-mode-was-active)
         linum-mode -1))))
)

;;;======================================================================
;;; Web mode
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Web mode")

 (require 'web-mode)
 (setq web-mode-enable-current-element-highlight t)
 (setq web-mode-enable-auto-quoting nil)
 ;; please close all the tags...
 (setq web-mode-void-elements '())
 (setq web-mode-enable-auto-indentation nil)

 ;; html
 (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.htm?\\'" . web-mode))
 ;; ejs
 (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
 ;; Javascript / JSX, only use web-mode for JS/JSX in Emacs24
 (GNUEmacs24
  (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))
 ;; Misc
 (add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))

 ;; Yaml
 (defun pierre-yaml-mode ()
   (interactive)
   (text-mode)
   (require 'highlight-indentation)
   (highlight-indentation-mode 1)
   (set-variable 'tab-width 2))
 (add-to-list 'auto-mode-alist '("\\.yml$" . pierre-yaml-mode))
 (add-to-list 'auto-mode-alist '("\\.yaml$" . pierre-yaml-mode))
 (add-to-list 'auto-mode-alist '("\\.yaml[a-z]*$" . pierre-yaml-mode))
 (add-to-list 'auto-mode-alist '("\\.neon$" . pierre-yaml-mode))

 ;; Haml
 (require 'haml-mode)
 (add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

 ;; Saas / Scss
 (require 'scss-mode)
 (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

 ;; Less
 (require 'less-css-mode)
 (add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

 ;; php web-mode
 (add-to-list 'auto-mode-alist '("\\.phtml\\'" . php-mode))
 (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . php-mode))
 (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . php-mode))

 ;; php-mode
 (add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs/site/php-mode"))
 (autoload 'php-mode "php-mode" nil t)
 (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
)

;;;======================================================================
;;; Autocomplete
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "auto-complete")

 ;; (require 'ni-autocomplete-company)
 ;; (require 'ni-autocomplete-fancy-dabbrev)
 (require 'ni-autocomplete-pabbrev)

 (defun make-ni-expand ()
   (make-hippie-expand-function
    '(agl-try-complete-with-calc-result
      try-expand-dabbrev-visible
      try-expand-dabbrev
      try-expand-dabbrev-all-buffers) t))

)

;;;======================================================================
;;; Direx (dired tree view) & Dired setup
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "direx")
 (require 'ni-direx)
 (require 'ni-dired)
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

 (GNUEmacsMin26
   (add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs/site/magit"))
   (require 'magit))

)

;;;======================================================================
;;; Ivy & Swiper
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Ivy")

 (require 'ni-ivy)

 (require 'swiper)

 ;; Search what's been marked otherwise isearch
 (defun ni-swiper-isearch ()
   "`swiper' with `ivy-thing-at-point'."
   (interactive)
   (let ((thing (ni-get-default-search-text)))
     (cond
      ((not (string-empty-p thing))
       (progn
         (when (use-region-p)
           (deactivate-mark))
         (swiper (regexp-quote thing))))
      (t (swiper-isearch)))))

)

;;;======================================================================
;;; Back button mode
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "back-button")

 (require 'smartrep)
 (require 'back-button)
 (back-button-mode 1)
 (diminish 'back-button-mode)
)

;;;======================================================================
;;; Editorconfig
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "editorconfig")

 (require 'editorconfig)
 (editorconfig-mode 1)
 (diminish 'editorconfig-mode))

;;;======================================================================
;;; Dumbjump
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "dumb-jump")

 (require 'dumb-jump)
 ;; (setq dumb-jump-debug t)
 (setq dumb-jump-disable-obsolete-warnings t)
 (setq dumb-jump-selector 'ivy)
)

;;;======================================================================
;;; Ace Window
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "ace-window")

 (require 'ace-window)
 (global-set-key (kbd "M-o") 'ace-window)
)

;;;======================================================================
;;; Pierre utils
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "git-auto-commit-mode")

  ;; git-auto-commit-mode
  ;;
  ;; Add as header in file:
  ;; -*- eval: (git-auto-commit-mode 1); gac-automatically-push-p: t; -*-
  ;;
  ;; Add in the .dir-locals.el of a directory:
  ;; ((nil . ((eval git-auto-commit-mode 1)
  ;;          (gac-automatically-push-p . t)
  ;;          (gac-automatically-add-new-files-p . t))))
  ;;
  (require 'git-auto-commit-mode)

  (defun pierre-list-modified-buffers ()
    "Show a list of all modified and unsaved buffers in a separate buffer."
    (interactive)
    (let ((modified-buffers))
      (dolist (buffer (buffer-list))
        (when (and (buffer-modified-p buffer)
                (not (buffer-base-buffer buffer))
                (buffer-file-name buffer))
          (push (buffer-name buffer) modified-buffers)))
      (if modified-buffers
        (with-current-buffer (get-buffer-create "*Modified Buffers*")
          (erase-buffer)
          (dolist (buf modified-buffers)
            (insert (propertize buf 'font-lock-face '(:underline t)
                      'mouse-face 'highlight
                      'keymap (let ((map (make-sparse-keymap)))
                                (define-key map [mouse-1]
                                  `(lambda ()
                                     (interactive)
                                     (switch-to-buffer ,buf)))
                                (define-key map "o"
                                  `(lambda ()
                                     (interactive)
                                     (switch-to-buffer-other-window ,buf)))
                                (define-key map (kbd "C-o")
                                  `(lambda ()
                                     (interactive)
                                     (switch-to-buffer ,buf)))
                                map)))
            (insert "\n"))
          (goto-char (point-min))
          (pop-to-buffer (current-buffer)))
        (message "No modified buffers"))))
  )

;;;======================================================================
;;; Keymap
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "pierre-keymap")
 (require 'ni-keymap-pierre)
)

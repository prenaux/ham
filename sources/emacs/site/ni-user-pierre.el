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
(require 'ni-emacs24-fixup)
(require 'ni-ham)
(require 'ni-flymake)
(require 'ham-flymake)
(require 'go-mode)

;;;======================================================================
;;; Keyboard
;;;======================================================================
(NotBatchMode

 ;; Disabled the insert key, remap it to control + insert.
 (define-key global-map [(insert)] nil)
 (define-key global-map [(control insert)] 'overwrite-mode)

 ;; Map the Escape key to "actually stop whatever NOW" or "please don't screw
 ;; up my environment randomly...".
 ;;
 ;; Without this you're going to have a bad time mmk...
 ;;
 (define-key global-map [escape] 'keyboard-quit)
 (global-set-key [escape] 'keyboard-quit)

 ; Yes... close everything... but not the buffers ^^ (yeah really... wtf...)
 (defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
   (let (orig-one-window-p)
     (fset 'orig-one-window-p (symbol-function 'one-window-p))
     (fset 'one-window-p (lambda (&optional nomini all-frames) t))
     (unwind-protect
         ad-do-it
       (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

 (global-set-key [remap keyboard-quit] 'keyboard-escape-quit)

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

 (defun my-backup-file-name (fpath)
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

 (defun my-backup-current-file-handler ()
   (let ((destBackupFileName (my-backup-file-name buffer-file-name)))
     (if (not (file-exists-p destBackupFileName))
         (progn
           (copy-file buffer-file-name destBackupFileName t)
           (message (concat "made backup: " destBackupFileName))
           )
       (progn
         (agl-bash-cmd-to-string (concat "touch \"" (my-backup-file-name buffer-file-name) "\""))
         (message (concat "already backed up: " destBackupFileName))
         )
       )))

 (add-hook 'after-save-hook 'my-backup-current-file-handler)

 )

;;;======================================================================
;;; Search in files
;;;======================================================================
(NotBatchMode
 (require 'pt)
 (Windows
  (setq pt-executable (concat "\"" HAM_HOME "/bin/nt-x86/pt.exe" "\"")))
 (OSX
  (setq pt-executable (concat HAM_HOME "/bin/osx-x86/pt")))
 (global-set-key "\C-h\C-j" 'pt-regexp)
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

 (defun rotate-windows ()
   "Rotate your windows"
   (interactive)
   (cond ((not (> (count-windows)1))
          (message "You can't rotate a single window!"))
         (t
          (setq i 1)
          (setq numWindows (count-windows))
          (while  (< i numWindows)
            (let* (
                   (w1 (elt (window-list) i))
                   (w2 (elt (window-list) (+ (% i numWindows) 1)))

                   (b1 (window-buffer w1))
                   (b2 (window-buffer w2))

                   (s1 (window-start w1))
                   (s2 (window-start w2))
                   )
              (set-window-buffer w1  b2)
              (set-window-buffer w2 b1)
              (set-window-start w1 s2)
              (set-window-start w2 s1)
              (setq i (1+ i)))))))

 (global-set-key (key "C-3") 'rotate-windows)

 )

;;;======================================================================
;;; Font
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "Set Font")

 (Windows
  (set-face-attribute 'default nil :family "Consolas" :height 105 :weight 'bold)
  )

 (Linux
  (setq default-frame-alist
        '((font . "-*-Consolas-*-r-*-*-11-108-120-120-c-*-*-*"))))
 )

;;;======================================================================
;;; Easy input of math symbols
;;;======================================================================
(NotBatchMode
(require 'xmsi-math-symbols-input)
(global-set-key (kbd "\C-x\C-x") 'xmsi-change-to-symbol)

(Windows
 (set-fontset-font
  "fontset-default" 'unicode
  "-outline-Arial Unicode MS-normal-normal-normal-sans-*-*-*-*-p-*-gb2312.1980-0"))
)

;;;======================================================================
;;; Look & Customizations
;;;======================================================================
(NotBatchMode
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
;;; Remove crap from the mode-line
;;;======================================================================
(NotBatchMode
 (require 'diminish))

;;;======================================================================
;;; Auto completion
;;;======================================================================
(agl-begin-time-block "auto-completion")

(NotBatchMode
 (GNUEmacs23
  (require 'pabbrev)
  (global-pabbrev-mode)
  (setq pabbrev-idle-timer-verbose nil)
  (diminish 'pabbrev-mode)
  (global-set-key (kbd "C-/") 'make-agl-expand))

 (GNUEmacs24
  (add-to-list 'load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/company-mode"))
  (require 'company)

  (define-global-minor-mode my-global-company-mode company-mode
    (lambda ()
      (when (not (memq major-mode
                       (list 'slime-repl-mode 'shell-mode 'ham-shell-mode)))
        (company-mode-on))
      ))
  (add-hook 'after-init-hook 'my-global-company-mode) ;; enable globaly

  ;; setup company mode to show up instantly
  (setq company-idle-delay 0.01)
  (setq company-minimum-prefix-length 2)
  ;; LOL, seriously what's the point of fucking up the text's upper/lower case by default ???
  ;; Without this set to nil the symbols are lower-cased !?!?
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)

  (defun my-auto-complete-off()
    (setq company-idle-delay nil))
  (defun my-auto-complete-on()
    (setq company-idle-delay 0.01))

  (add-hook 'mark-multiple-enabled-hook 'my-auto-complete-off)
  (add-hook 'mark-multiple-disabled-hook 'my-auto-complete-on)

  (global-set-key (kbd "C-/") 'company-complete-common)

  (diminish 'company-mode)

  )
)

;;;======================================================================
;;; Buffer cleanup and indentation
;;;======================================================================
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

(define-key global-map (kbd "C-S-i") 'my-indent-buffer)

;;;======================================================================
;;; --- Disable unneeded warnings ---
;;;======================================================================
(put 'dired-find-alternate-file 'disabled nil)

;;;======================================================================
;;; --- Multi web mode ---
;;;======================================================================
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script[^>]*>" "</script>")
                  (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;;;======================================================================
;;; Rainbow delimiters
;;;======================================================================
(require 'rainbow-delimiters)
(add-hook 'niscript-mode-hook 'rainbow-delimiters-mode)
(add-hook 'c++-mode-hook 'rainbow-delimiters-mode)
(add-hook 'c-mode-hook 'rainbow-delimiters-mode)

;;;======================================================================
;;; Jump to line with feedback
;;;======================================================================
(NotBatchMode
 ;; (global-set-key [remap goto-line] 'goto-line-with-feedback)

 (global-set-key (key "C-l") 'goto-line) ;; Ctrl-l goto line, more convenient than C-c C-g...
 (global-set-key (key "C-S-l") 'recenter-top-bottom)  ;; Remap recenter-top-bottom (which is map to Ctrl-l by default) to Ctrl-Shift-L

 (defun goto-line-with-feedback ()
   "Show line numbers temporarily, while prompting for the line number input"
   (interactive)
   (unwind-protect
       (progn
         (linum-mode 1)
         (goto-line (read-number "Goto line: ")))
     (linum-mode -1)))

 )

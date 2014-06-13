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
(require 'xmsi-math-symbols-input)
(global-set-key (kbd "\C-x\C-x") 'xmsi-change-to-symbol)

(Windows
 (set-fontset-font
  "fontset-default" 'unicode
  "-outline-Arial Unicode MS-normal-normal-normal-sans-*-*-*-*-p-*-gb2312.1980-0"))

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
(require 'diminish)
(diminish 'pabbrev-mode)

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

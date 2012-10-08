(provide 'ni-user-pierre)

(require 'aglemacs)
(require 'ni-org)
(require 'ni-js2)
(require 'ni-muse)
(require 'ni-templates)
(require 'ni-file-cache)
(require 'ni-emacs24-fixup)
(require 'ni-ham)

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

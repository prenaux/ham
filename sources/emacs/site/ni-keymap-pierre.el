(provide 'ni-keymap-pierre)
(NotBatchMode

;;;======================================================================
;;; aglemacs.el
;;;======================================================================
 (GNUEmacs23
  (global-set-key (kbd "RET") 'newline-and-indent)
  (global-set-key (kbd "<S-return>") 'newline)
  (OSX
   (global-set-key (kbd "<kp-delete>") 'delete-char)
  )
 )

 (GNUEmacs24
  ;; Disable electric mode so that 'enter' insert a new line without modifying
  ;; the current line but indents the actual new line.
  (when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
  (global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)
  (global-set-key (kbd "<C-j>") 'newline)
 )

 (OSX
  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line)
 )
 (Aquamacs
  (define-key osx-key-mode-map [home] 'beginning-of-line)
  (define-key osx-key-mode-map [end] 'end-of-line)
  (global-set-key [(control right)] 'forward-word)
  (global-set-key [(control left)] 'backward-word)
 )

 ;; isearch - the defaults are _so_ annoying...
 (define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char) ; bs means bs
 (define-key isearch-mode-map (kbd "<delete>")    'isearch-delete-char)  ; delete means delete

 (global-set-key '[(control meta up)] 'agl-search-word-backward)
 (global-set-key '[(control meta down)] 'agl-search-word-forward)
 (global-set-key '[(meta up)] 'agl-search-word-backward)
 (global-set-key '[(meta down)] 'agl-search-word-forward)

 ;; Move to the 'next' window (in clockwise order)
 (global-set-key (key "C-1") 'other-window)
 (global-set-key (key "C-2") 'other-window)

 ;; Forward/Backward paragraph
 (global-set-key (key "M-p") 'backward-paragraph)
 (global-set-key (key "C-{") 'backward-paragraph)
 (global-set-key (key "M-n") 'forward-paragraph)
 (global-set-key (key "C-}") 'forward-paragraph)

 (global-set-key (key "C-x C-r") 'revert-buffer)

 (global-set-key (kbd "C-6") 'ni-word-wrap-toggle)

 (global-set-key [(meta delete)] 'kill-current-buffer)

 ;; Shell
 (global-set-key (kbd "M-0") 'erase-buffer)
 (global-set-key (kbd "C-0") 'ham-shell)
 (global-set-key (kbd "C-)") 'ham-shell-other-frame) ;; C-S-0
 (global-set-key (key "M-`") 'other-frame)
 (global-set-key (key "C-`") 'other-frame)
 (define-key global-map [(meta return)] 'agl-select-visible-shell-window)
 (define-key global-map [(control return)] 'agl-run-last-shell-command)

 ;; PgUp/Dn
 (global-set-key (kbd "C-v") 'scroll-up-command)
 (global-set-key (kbd "C-S-v") 'scroll-down-command)

 ;; Scroll one line at a time
 (global-set-key (quote [C-M-down]) (quote scroll-up-line))
 (global-set-key (quote [C-M-up]) (quote scroll-down-line))

 ;; Toggle word wrap
 (global-set-key (kbd "M-6") 'whitespace-mode)
 ;; Ctrl-=/- increase/decrease font size
 (global-set-key (kbd "C-=") 'agl-increase-font-size)
 (global-set-key (kbd "C--") 'agl-decrease-font-size)

 ;; Make the sequence "C-c g" execute the `goto-line' command,
 ;; which prompts for a line number to jump to.
 (global-set-key "\C-c\C-g" 'goto-line)

 ;; undo on C-z, move the suspend/iconify to C-/
 (global-set-key "\C-z" 'undo)

 ;; remap regex search to Atl-s/r
 (global-set-key "\M-s" 'isearch-forward-regexp)
 (global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
 (global-set-key "\M-r" 'isearch-backward-regexp)
 (global-set-key (kbd "C-M-r") 'isearch-forward-regexp)

 (global-set-key "\C-h\C-h" 'qrr)

 ;; extended expand
 (global-set-key [(meta /)] (make-agl-expand))

 ;; Matching parenthesis
 (global-set-key "\M-5" 'goto-match-paren)

 ;; Previous/Next errors
 (define-key global-map "\M-3" 'previous-error)
 (define-key global-map "\M-4" 'next-error)

 ;; shift-down comments the current line and goes down
 (define-key global-map [(shift down)] 'agl-comment-and-go-down)
 ;; shift-up uncomments the current line and goes up
 (define-key global-map [(shift up)] 'agl-uncomment-and-go-up)
 ;; inc number under cursor
 (define-key global-map (kbd "M-=") 'agl-increment-number-at-point)
 ;; dec number under cursor
 (define-key global-map (kbd "M--") 'agl-decrement-number-at-point)
 ;; UUID generation
 (global-set-key (kbd "C-M-g")   'agl-uuid1-to-buffer)
 (global-set-key (kbd "C-M-S-g") 'agl-uuid2-to-buffer)
 (global-set-key (kbd "M-G")     'agl-uuid3-to-buffer)

 ;; Begin/end of buffer
 (define-key global-map (kbd "C-S-a") 'beginning-of-buffer)
 (define-key global-map (kbd "C-S-e") 'end-of-buffer)

 (global-set-key (kbd "C-S-j") 'macro-join-line)

 (global-set-key (kbd "C-.") 'goto-match-paren2)

 (global-set-key (kbd "C-S-y") '(lambda ()
                                  (interactive)
                                  (popup-menu 'yank-menu)))

;;;======================================================================
;;; aglemacs.el: mark-multiple, expand-region
;;;======================================================================
 (GNUEmacs24
  ;; Emacs 25+ already do inline rectangle replace
  (require 'inline-string-rectangle)
  (global-set-key (kbd "C-x r t") 'inline-string-rectangle))
 (global-set-key (kbd "C-<") 'mark-previous-like-this)
 (global-set-key (kbd "C->") 'mark-next-like-this)
 (global-set-key (kbd "C-*") 'mark-all-like-this)
 (global-set-key (kbd "C-@") 'er/expand-region)

;;;======================================================================
;;; ni-autocomplete-company.el
;;;======================================================================
 (if (boundp 'company-ni-idl-complete)
     (progn
       ;; (global-set-key (kbd "C-/") 'company-complete-common)
       (global-set-key (kbd "C-/") 'company-ni-idl-complete)
     ))

;;;======================================================================
;;; ni-flymake.el
;;;======================================================================
 (global-set-key [f3] 'flymake-goto-prev-error)
 (global-set-key [f4] 'flymake-goto-next-error)
 (global-set-key [(control f3)] 'flymake-start-syntax-check)
 (global-set-key [(control f4)] 'flymake-mode)

;;;======================================================================
;;; ni-muse.el
;;;======================================================================
 (global-set-key "\C-xp" 'agl-muse-publish-to-html)
 (global-set-key "\C-xP" 'agl-muse-publish-to-html)
 ;; (global-set-key "\C-x\C-p" 'agl-muse-publish-to-pdf)

;;;======================================================================
;;; ni-autocomplete-pabbrev.el
;;;======================================================================
 (if (boundp 'make-agl-expand)
     (progn
       (global-set-key (kbd "C-/") 'make-agl-expand)
     ))

;;;======================================================================
;;; ni-org.el
;;;======================================================================
 (if (boundp 'org-store-link)
     (progn
       (global-set-key "\C-cl" 'org-store-link)
       (global-set-key "\C-ca" 'org-agenda)
     ))

;;;======================================================================
;;; ni-sql.el
;;;======================================================================
 (global-set-key (kbd "C-x C-c") 'ni-sql-connect-server)

;;;======================================================================
;;; ni-file-cache.el
;;;======================================================================

 ;; C-xC-f repmapped to ni-ffap, which opens the file at point be default, if
 ;; not found calls the default open file
 (global-set-key (key "C-x C-f") 'jcl-file-cache-ido-find-file)
 (define-key global-map (kbd "C-S-r") 'file-cache-ido-find-file)
 (define-key global-map (kbd "C-S-M-r") 'file-cache-update-my-cache)

;;;======================================================================
;;; ni-user-pierre.el
;;;======================================================================
 (global-set-key [f5] 'save-all-and-compile)
 (global-set-key [(control f5)] 'compile)
 (global-set-key [f6] 'previous-error)
 (global-set-key [f7] 'next-error)

 ;; Disabled the insert key, remap it to control + insert.
 (define-key global-map [(insert)] nil)
 (define-key global-map [(control meta insert)] 'overwrite-mode)
 (define-key global-map [(control shift insert)] 'overwrite-mode)
 (define-key global-map (key "C-o") 'overwrite-mode)

 ;; Map the Escape key to "actually stop whatever NOW" or "please don't screw
 ;; up my environment randomly...".
 ;;
 ;; Without this you're going to have a bad time mmk...
 ;;
 (define-key global-map [escape] 'keyboard-quit)
 (global-set-key [escape] 'keyboard-quit)

 (global-set-key [remap keyboard-quit] 'keyboard-escape-quit)

 (add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

 (global-set-key (key "C-1") 'my-other-window)
 (global-set-key (key "C-2") 'my-other-window)

 (global-set-key "\C-h\C-j" 'pt-regexp)
 (global-set-key "\C-h\C-y" 'pt-work-regexp)
 (global-set-key "\C-h\C-g" 'occur)

 ;; (global-set-key [remap goto-line] 'goto-line-with-feedback)

 (global-set-key (key "C-l") 'goto-line) ;; Ctrl-l goto line, more convenient than C-c C-g...
 (global-set-key (key "C-S-l") 'recenter-top-bottom)  ;; Remap recenter-top-bottom (which is mapped to Ctrl-l by default) to Ctrl-Shift-L

 ;; Flowtype
 ;; (global-set-key (key "C-t") 'tpl-js-flow-type)

 ;; Git
 (global-set-key (key "C-x g") 'git-status)

 (define-key global-map (kbd "C-S-i") 'my-indent-buffer)

 (define-key global-map "\C-x\C-d" 'direx:jump-to-directory)

)

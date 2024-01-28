(provide 'ni-keymap-pierre)

(NotBatchMode
 (require 'bind-key)
 (require 'golden-ratio-scroll-screen)
 (require 'expand-region)

;;;======================================================================
;;; aglemacs.el
;;;======================================================================
 ;; Disable electric mode so that 'enter' insert a new line without modifying
 ;; the current line but indents the actual new line.
 (when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
 (global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)
 (global-set-key [(control return)] 'agl-newline-and-indent-same-level)

 (OSX
  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line)
  ;; disable Cmd-w & Cmd-q which close emacs and the current frame. In
  ;; particular Cmd-w is a common mistake.
  (global-unset-key (kbd "s-w"))
  (global-unset-key (kbd "s-q"))
 )
 (Aquamacs
  (define-key osx-key-mode-map [home] 'beginning-of-line)
  (define-key osx-key-mode-map [end] 'end-of-line)
  (global-set-key [(control right)] 'forward-word)
  (global-set-key [(control left)] 'backward-word)
 )

 ;; Disable C-z which will put emacs in the bg, drives you to insanity if you
 ;; miss hit it.
 (global-unset-key (kbd "C-z"))

 ;; ibuffer
 (global-set-key (key "C-x C-b") 'ibuffer)

 ; Delete without passing by the killring
 (global-set-key (kbd "C-S-k") 'agl-delete-line-backward) ; Ctrl+Shift+k
 (global-set-key (kbd "C-k") 'agl-delete-line)
 (global-set-key (kbd "M-d") 'agl-delete-word)
 (global-set-key (kbd "<M-backspace>") 'agl-backward-delete-word)
 (global-set-key (kbd "<C-backspace>") 'agl-backward-delete-word)

 ;; isearch - the defaults are _so_ annoying...
 (define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char) ; bs means bs
 (define-key isearch-mode-map (kbd "<delete>")    'isearch-delete-char)  ; delete means delete

 ;; There's as many variations of 'Meta' as they are stars in the sky...
 (global-set-key [(meta right)] 'forward-word)
 (global-set-key [(meta left)] 'backward-word)
 (global-set-key [(control right)] 'forward-word)
 (global-set-key [(control left)] 'backward-word)
 (global-set-key '[(control up)] 'backward-paragraph)
 (global-set-key '[(control down)] 'forward-paragraph)
 (global-set-key '[(meta up)] 'backward-paragraph)
 (global-set-key '[(meta down)] 'forward-paragraph)
 (global-set-key (key "ESC <up>") 'backward-paragraph)
 (global-set-key (key "ESC <down>") 'forward-paragraph)

 ;; Move to the 'next' window (in clockwise order)
 (global-set-key (key "M-`") 'other-window)
 (global-set-key (kbd "M-o") 'other-window)

 ;; Forward/Backward word under point
 (global-set-key (key "M-p") 'agl-search-word-backward)
 (global-set-key (key "M-n") 'agl-search-word-forward)

 (global-set-key (key "C-h C-r") 'revert-buffer)

 (global-set-key "\C-h\C-w" 'ni-word-wrap-toggle)

 ;; Shell
 (global-set-key "\C-h\C-s" 'ham-shell)
 (global-set-key "\C-h\C-t" 'agl-select-visible-shell-window)
 (global-set-key "\C-h\C-p" 'agl-run-last-shell-command)
 (global-set-key (key "C-h C-0") 'erase-buffer)

 ;; PgUp/Dn
 (global-set-key (kbd "C-v") 'golden-ratio-scroll-screen-up)
 (global-set-key (kbd "M-v") 'golden-ratio-scroll-screen-down)

 ;; Scroll one line at a time
 (global-set-key (quote [C-M-down]) (quote scroll-up-line))
 (global-set-key (quote [C-M-up]) (quote scroll-down-line))

 ;; Ctrl +/- increase/decrease font size
 (global-set-key (kbd "C-=") 'agl-increase-font-size)
 (global-set-key (kbd "C--") 'agl-decrease-font-size)

 ;; Goto line
 (global-set-key (key "C-l") 'goto-line-preview)
 ;; Remap recenter-top-bottom (which is mapped to Ctrl-l by default) to Ctrl-Shift-L
 (global-set-key (key "C-S-l") 'recenter-top-bottom)

 ;; remap regex search to Atl-s/r
 (global-set-key "\M-s" 'isearch-forward-regexp)
 (global-set-key "\M-r" 'isearch-backward-regexp)

 ;; Search and replace in the current buffer
 (global-set-key "\C-h\C-h" 'qrr)

 ;; extended expand, tab & shift-tab expand are handled in fancy-dabbrev
 (global-set-key (kbd "M-/") (make-ni-expand))

 ;; ni-comment-dwim
 (global-set-key "\C-c\C-c" 'ni-comment-dwim)
 (global-set-key (kbd "C-;") 'ni-comment-dwim)
 (global-set-key (kbd "M-;") 'ni-comment-dwim)
 ;; shift-down comments the current line and goes down
 (define-key global-map (kbd "M-+") 'agl-comment-and-go-down)
 ;; shift-up uncomments the current line and goes up
 (define-key global-map (kbd "M-_") 'agl-uncomment-and-go-up)

 ;; inc number under cursor
 (define-key global-map (kbd "M-=") 'agl-increment-number-at-point)
 ;; dec number under cursor
 (define-key global-map (kbd "M--") 'agl-decrement-number-at-point)

 ;; UUID generation
 (global-set-key (kbd "C-M-g")   'agl-uuid1-to-buffer)
 (global-set-key (kbd "C-M-S-g") 'agl-uuid2-to-buffer)
 (global-set-key (kbd "M-G")     'agl-uuid3-to-buffer)

 ;; Begin/end of buffer
 (define-key global-map "\C-h\C-a" 'beginning-of-buffer)
 (define-key global-map "\C-h\C-e" 'end-of-buffer)

 ;; Join line
 (defun pierre-join-line () ""
   (interactive)
   (end-of-line)
   (next-line)
   (join-line))

 (bind-key* "C-j" 'pierre-join-line)

;;;======================================================================
;;; aglemacs.el: mark-multiple, expand-region
;;;======================================================================
 (global-set-key (kbd "M-9") 'mc/mark-previous-like-this-symbol) ;; M-9, M-(
 (global-set-key (kbd "M-(") 'mc/mark-previous-like-this) ;; M-9, M-(
 (global-set-key (kbd "M-0") 'mc/mark-next-like-this-symbol) ;; M-0, M-)
 (global-set-key (kbd "M-)") 'mc/mark-next-like-this) ;; M-0, M-)
 (global-set-key (kbd "M-8") 'mc/mark-all-like-this) ;; M-8, M-*
 (global-set-key (kbd "M-*") 'mc/edit-ends-of-lines) ;; M-8, M-*

 (global-unset-key (kbd "M-<down-mouse-1>"))
 (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;;;======================================================================
;;; ni-flymake.el
;;;======================================================================
 (global-set-key (kbd "C-h C-c") 'aflymake-mode-or-syntax-check)
 (global-set-key (kbd "C-h C-f") 'ham-fix-current-buffer)

;;;======================================================================
;;; ni-org.el
;;;======================================================================
 (if (boundp 'org-store-link)
     (progn
       (global-set-key "\C-cl" 'org-store-link)
       (global-set-key "\C-ca" 'org-agenda)
     ))

;;;======================================================================
;;; ni-file-cache.el / projects / window navigation
;;;======================================================================

 ;;;; Open cached file
 (global-set-key (key "C-x C-f") 'ni-file-cache-find-file)
 (global-set-key (key "C-x f") 'ni-file-cache-find-file-at-point)

 ;;;; Fuzzy search
 ;; open file in project
 (global-set-key (kbd "C-x C-p") 'ni-counsel-fzf-at-point)
 ;; open file in a directory
 (global-set-key (kbd "C-x p") 'ni-counsel-fzf-at-point-in-dir)

 ;; go to file tree window
 (global-set-key (kbd "C-h C-d") 'direx:jump-to-project-file-other-window)
 (global-set-key (kbd "C-h d") 'direx:jump-to-project-file)

 ;;;; Swiper / Isearch
 ;; swiper isearch
 (global-set-key (kbd "C-s") 'ni-swiper-isearch)
 ;; emacs' isearch
 (global-set-key (kbd "C-S-s") 'isearch-forward)

;;;======================================================================
;;; ni-user-pierre.el
;;;======================================================================

 ;; Ask for compile command and then compile
 (global-set-key (key "C-h C-n") 'compile)
 ;; Save all then compile with the last compile command used
 (global-set-key (key "C-h C-b") 'save-all-and-compile)

 ;; Goto matching bracket/paren
 (bind-key* "M-a" 'ni-goto-matching-bracket)

 ;; Expand region
 (bind-key* "M-A" 'er/expand-region) ;; M-S-a
 (IsNotTerminal ;; breaks C-space in terminal
   (bind-key* "C-@" 'er/expand-region)) ;; M-S-a

 ;; Previous/Next errors
 (define-key global-map "\M-1" 'aflymake-goto-next-error)
 (define-key global-map "\M-2" 'aflymake-goto-prev-error)
 (define-key global-map "\M-3" 'next-error)
 (define-key global-map "\M-4" 'previous-error)

 ;; Previous/Next flymake errors
 (global-set-key (kbd "M-!") 'next-error) ;; M-! (M-S-1)
 (global-set-key (kbd "M-@") 'previous-error) ;; M-@ (M-S-2)

 ;; Disabled the insert key, remap it to control + insert.
 (define-key global-map [(insert)] nil)
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

 ;; ham-grep starting in current directory, prompt for it
 (global-set-key "\C-h\C-j" 'ham-grep-regexp-current-dir)
 ;; ham-grep in the Work directories
 (global-set-key "\C-h\C-k" 'ham-grep-work-regexp)

 ;; counsel-rg-match in current project
 (global-set-key (kbd "M-.") (lambda () (interactive)
                               (ni-counsel-rg-match
                                 nil pierre-search-file-patterns)))
 ;; counsel-rg-match in current project and all work directories
 (global-set-key (kbd "M->") (lambda () (interactive)
                               (ni-counsel-rg-match
                                 pierre-search-all-dirs
                                 pierre-search-file-patterns)))

 ;; Indent region
 (define-key global-map "\C-h\C-\\" 'indent-region)

 ;; Disable C-MouseWheel to zoom text. Note that you can use
 ;; Super/Cmd/Window-0,-,+ to change the font size in a buffer.
 (global-set-key (kbd "<C-mouse-4>") nil)
 (global-set-key (kbd "<C-mouse-5>") nil)
 (global-set-key (kbd "C-<wheel-down>") nil)
 (global-set-key (kbd "C-<wheel-up>") nil)

 ;; Map M-Esc & C-Esc to escape
 (global-set-key (kbd "M-<escape>") 'keyboard-escape-quit)
 (global-set-key (kbd "C-<escape>") 'keyboard-escape-quit)

 )

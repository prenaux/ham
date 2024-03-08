(provide 'ni-keymap-pierre)

(NotBatchMode
 (require 'bind-key)
 (require 'golden-ratio-scroll-screen)
 (require 'expand-region)
 (require 'zygospore)

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
 (global-set-key (kbd "ESC <up>") 'backward-paragraph)
 (global-set-key (kbd "ESC <down>") 'forward-paragraph)

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
 (global-set-key (kbd "<next>") 'golden-ratio-scroll-screen-up)
 (global-set-key (kbd "<prior>") 'golden-ratio-scroll-screen-down)

 ;; Scroll one line at a time
 (global-set-key (quote [C-M-down]) (quote scroll-up-line))
 (global-set-key (quote [C-M-up]) (quote scroll-down-line))

 ;; Ctrl +/- increase/decrease font size
 (global-set-key (kbd "C-=") 'agl-increase-font-size)
 (global-set-key (kbd "C--") 'agl-decrease-font-size)

 ;; Search and replace in the current buffer
 (global-set-key "\C-h\C-h" 'qrr)

 ;; inc number under cursor
 (define-key global-map (kbd "M-=") 'agl-increment-number-at-point)
 ;; dec number under cursor
 (define-key global-map (kbd "M--") 'agl-decrement-number-at-point)

 ;; Begin/end of buffer
 (define-key global-map "\C-h\C-a" 'beginning-of-buffer)
 (define-key global-map "\C-h\C-e" 'end-of-buffer)

 (bind-key* "C-j" 'pierre-join-line)

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

 ;; Indent region
 (bind-key* "C-h C-\\" 'ni-indent-region-or-buffer)
 (bind-key* "C-M-\\" 'ni-indent-region-or-buffer)

 ;; Disable C-MouseWheel to zoom text. Note that you can use
 ;; Super/Cmd/Window-0,-,+ to change the font size in a buffer.
 (global-set-key (kbd "<C-mouse-4>") nil)
 (global-set-key (kbd "<C-mouse-5>") nil)
 (global-set-key (kbd "C-<wheel-down>") nil)
 (global-set-key (kbd "C-<wheel-up>") nil)

 ;; Map M-Esc & C-Esc to escape
 (global-set-key (kbd "M-<escape>") 'keyboard-escape-quit)
 (global-set-key (kbd "C-<escape>") 'keyboard-escape-quit)

;;;======================================================================
;;; ham keys inspired
;;;=====================================================================

 ;; Unset the leader key
 (global-unset-key (key "M-/"))

 ;; Commands
 (bind-key* "M-g" 'keyboard-escape-quit)
 (bind-key* "M-z" 'undo)
 (bind-key* "C-i" (make-ni-expand))
 (bind-key* "M-s" 'ni-start-from-new-line)
 (bind-key* "M-/ M-s" 'ni-start-from-new-top-line)
 (bind-key* "M-." 'ni-counsel-rg-at-point)

 ;; Macros
 (bind-key* "M-5" 'kmacro-end-and-call-macro)
 (bind-key* "M-6" 'kmacro-start-macro)
 (bind-key* "M-7" 'kmacro-end-macro)

 ;; Nav & Splits
 (bind-key* "C-1" 'zygospore-toggle-delete-other-windows)
 (bind-key* "M-1" 'other-window)
 (bind-key* "M-2" 'split-window-below)
 (bind-key* "M-3" 'split-window-right)
 (bind-key* "M-4" 'delete-window)

 ;; Multi-cursor
 (bind-key* "M-9" 'mc/mark-previous-like-this)
 (bind-key* "M-0" 'mc/mark-next-like-this)
 (bind-key* "M-8" 'mc/mark-all-like-this)
 (bind-key* "M-*" 'mc/edit-ends-of-lines)
 (global-unset-key (kbd "M-<down-mouse-1>"))
 (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

 ;; Region
 (bind-key* "M-t" 'ni-select-current-line-and-forward-line)
 (bind-key* "M-a" 'ni-select-current-line-and-forward-line)
 (bind-key* "M-r" 'er/expand-region)
 (bind-key* "M-/ M-r" 'er/contract-region)

 ;; Goto
 (bind-key* "M-/ M-/" 'ni-goto-matching-bracket)
 (bind-key* "M-/ M-l" 'goto-line)
 (bind-key* "M-/ M-," 'back-button-local-backward)
 (bind-key* "M-/ M-." 'back-button-local-forward)

 ;; Comment/uncomment
 (bind-key* "M-;" 'ni-comment-dwim)
 (bind-key* "M-/ M-c" 'ni-comment-region-or-line-and-go-down)
 (bind-key* "M-/ M-v" 'ni-uncomment-region-or-line-and-go-up)

 ;; Error nav
 (bind-key* "<f1>" 'aflymake-goto-prev-error)
 (bind-key* "<f2>" 'aflymake-goto-next-error)
 (bind-key* "<f3>" 'previous-error)
 (bind-key* "<f4>" 'next-error)

 )

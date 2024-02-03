(provide 'ni-keymap-ham-keys)
(require 'golden-ratio-scroll-screen)
(require 'jump-char)
(require 'zygospore)
(require 'move-text)
(require 'expand-region)
(require 'bind-key) ;; Use bind key for now
(require 'ryo-modal)

;; To be able to navigate around
(require 'smartrep)
(require 'back-button)
(back-button-mode 1)
(diminish 'back-button-mode)

;; expand-region-smart-cursor moves the cursor at the end of the selection
;; when the region expands beyond the initial starting point which allows us
;; to expand it down from there.
(setq expand-region-smart-cursor t)

;; Unbind our prefix keys
(global-unset-key (kbd "M-,"))
(global-unset-key (kbd "M-/"))

;; We use bind-key for now, maybe a minor mode would be cleaner though. We
;; keep this here for reference as a TODO of sort.
(DontExecute
  (defvar ham-keys-minor-mode-map
    (make-sparse-keymap)
    "ham-keys-minor-mode keymap.")

  (define-minor-mode ham-keys-minor-mode
    "A minor mode so that my key settings override annoying major modes."
    :init-value t
    :lighter " ham-keys")
  )

(defun ham-keys-edit (aKey aCommand &optional noryo noglobal)
  "Add the edit prefix to STRING."
  (IsTerminal ;; oh sweet terminal, if only you could send the whole full keys for all xD
    (unless noglobal
      (bind-key* (concat "M-" aKey) aCommand)))
  (unless noglobal
    (bind-key* (concat "C-M-" aKey) aCommand))
  (unless noryo
    (ryo-modal-key aKey aCommand))
  )

(defmacro ham-keys-def-edit (&rest pairs)
  "Define multiple edit keybindings."
  `(progn
     ,@(mapcar (lambda (pair)
                 `(ham-keys-edit ,(car pair) ',(cadr pair)))
               pairs)))

(defun ham-keys-leader (aLeader aKey aCommand &optional noryo noglobal)
  "Add the leader prefix to STRING."
  (unless noglobal
    (bind-key* (concat "M-" aLeader " " aKey) aCommand))
  (IsTerminal ;; oh sweet terminal, if only you could send the whole full keys for all xD
    (unless noglobal
      (bind-key* (concat "M-" aLeader " C-M-" aKey) aCommand))
    (unless noglobal
      (bind-key* (concat "M-" aLeader " M-" aKey) aCommand))
    )
  (unless noglobal
    (bind-key* (concat "C-M-" aLeader " C-M-" aKey) aCommand))
  (unless noryo
    (ryo-modal-key (concat aLeader " " aKey) aCommand))
  )

(defmacro ham-keys-def-leader (aLeader &rest pairs)
  "Define multiple leader keybindings."
  `(progn
     ,@(mapcar (lambda (pair)
                 `(ham-keys-leader ,aLeader ,(car pair) ',(cadr pair)))
               pairs)))

(defun ham-keys-start-from-new-line ()
  (interactive)
  (move-end-of-line nil)
  (newline)
  (indent-for-tab-command))

(defun ham-keys-start-from-new-top-line ()
  (interactive)
  (previous-line)
  (ham-keys-start-from-new-line))

(defun ham-keys-insert-or-change-region ()
  "Kill active region if active"
  (interactive)
  (if mark-active (delete-region (region-beginning) (region-end)))
  (message "Insert mode actived"))

(defun ham-keys-insert-mode-activated ()
  "Notifies that the insert mode has been activated"
  (interactive)
  (message "Insert mode actived"))

(defun ham-keys-delete-word-or-kill-region (arg)
  "Kill active region if active"
  (interactive "p")
  (if mark-active
    (kill-region (region-beginning) (region-end))
    (delete-region ;; we dont want the word in the kill-ring
      (point)
      (progn
        (forward-word arg)
        (point)))
    ))

(defun ham-keys-delete-char-or-kill-region (arg)
  "Kill active region if active"
  (interactive "p")
  (if mark-active
    (kill-region (region-beginning) (region-end))
    (delete-char arg)))

(defun ni-select-current-line-and-forward-line (arg)
  "Select the current line and move the cursor by ARG lines IF no
region is selected.

If a region is already selected when calling this command, only
move the cursor by ARG lines."
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))

(defun ham-keys-fif-at-point ()
  (interactive)
  (ni-counsel-rg-match
    nil pierre-search-file-patterns))

(defun ham-keys-comment-region-or-line-and-go-down (arg)
  "Kill active region if active"
  (interactive "p")
  (if mark-active
    (comment-region (region-beginning) (region-end))
    (progn
      (condition-case nil (comment-region (point-at-bol) (point-at-eol)) (error nil))
      (end-of-line)
      (next-line 1)
      (back-to-indentation)))
  )

(defun ham-keys-uncomment-region-or-line-and-go-up (arg)
  "Kill active region if active"
  (interactive "p")
  (if mark-active
    (uncomment-region (region-beginning) (region-end))
    (progn
      (condition-case nil (uncomment-region (point-at-bol) (point-at-eol)) (error nil))
      (back-to-indentation)
      (next-line -1)))
  )

(defun ham-keys-keyboard-quit ()
  (interactive)
  (mc/keyboard-quit)
  (keyboard-escape-quit))

(ham-keys-def-edit
  ;; Commands
  ("p" counsel-M-x)
  ("g" ham-keys-keyboard-quit)
  )

(ham-keys-def-edit
  ;; Inverted T Movement.
  ("i" previous-line)
  ("j" backward-char)
  ("k" next-line)
  ("l" forward-char)

  ("u" backward-word)
  ("o" forward-word)

  ("h" forward-paragraph)
  ("y" backward-paragraph)
  )

(DontExecute ham-keys-def-edit
  ;; HJKL Movement.
  ("h" backward-char)
  ("j" next-line)
  ("k" previous-line)
  ("l" forward-char)

  ("y" backward-word)
  ("u" forward-paragraph)
  ("i" backward-paragraph)
  ("o" forward-word)
  )

(ham-keys-def-edit
  ;; Non-arrow key movements
  ("6" beginning-of-line)
  ("7" back-to-indentation)
  ("8" move-end-of-line)

  ;; Searching
  ("n" swiper-thing-at-point)

  ;; Multi cursors
  ("m" mc/mark-next-like-this)
  ("S-m" mc/mark-previous-like-this)

  ;; Editing
  ("q" indent-region)
  ("S-q" fill-paragraph)
  ("w" agl-backward-delete-word)
  ("e" agl-delete-word)

  ("a" ni-select-current-line-and-forward-line)
  ("s" ham-keys-start-from-new-line)
  ("S-s" ham-keys-start-from-new-top-line)
  ("d" delete-char)

  ("z" undo)
  ("x" ham-keys-delete-char-or-kill-region)
  ("c" kill-ring-save)
  ("v" yank)
  ("S-v" yank-pop)

  ("b" ham-keys-comment-region-or-line-and-go-down)
  ("S-b" ham-keys-uncomment-region-or-line-and-go-up)

  ;; Visual selection
  ("t" set-mark-command)
  ("r" er/expand-region)
  ("S-r" er/contract-region)

  ;; Macros
  ("9" kmacro-start-macro)
  ("0" kmacro-end-macro)
  (";" kmacro-end-and-call-macro)

  ;; Splits
  ("1" other-window)
  ("2" split-window-below)
  ("3" split-window-right)
  ("4" delete-window)
)

(ham-keys-def-leader "/"
  ;; Goto locations
  ("y" beginning-of-buffer)
  ("h" end-of-buffer)
  ("l" goto-line-preview)
  ("/" ni-goto-matching-bracket)
  ("g" ham-keys-fif-at-point)
  ("v" avy-goto-char)
  ("," back-button-local-backward)
  ("." back-button-local-forward)
  )

(ham-keys-def-leader ","
  ("r" revert-buffer)

  ("o" ni-file-cache-find-file-at-point)
  ("i" find-file) ;; Use to create new files
  ("b" ivy-switch-buffer)
  ("d" direx:jump-to-project-file)

  ("j" ham-grep-regexp-current-dir)
  ("u" ham-grep-work-regexp)
  ("h" qrr)

  ("s" save-buffer)
  ("w" save-some-buffers) ;; Save all buffers
  ("k" kill-buffer)

  ("1" zygospore-toggle-delete-other-windows)

  ("n" universal-argument)

  ("m" mc/mark-all-like-this)

  ("a" mark-whole-buffer)
)

;; Separate global key setups for special cases
(bind-key* (kbd "C-/") (make-ni-expand))
(bind-key* "<f1>" 'aflymake-goto-prev-error)
(bind-key* "<f2>" 'aflymake-goto-next-error)
(bind-key* "<f3>" 'previous-error)
(bind-key* "<f4>" 'next-error)
(bind-key* "<f7>" 'aflymake-mode-or-syntax-check)
(bind-key* "<f8>" 'ham-fix-current-buffer)

;; Modal mode toggles suggestions
;; (global-set-key (kbd "C-h C-v") 'ryo-modal-mode)
;; (global-set-key (kbd "M-SPC") 'ryo-modal-mode)
;; (global-set-key (kbd "<escape>") 'ryo-modal-mode) ;; Doesnt work in Terminal

;; Modal only keys
(ryo-modal-keys
  ;; ("f" ham-keys-insert-or-change-region :exit t)
  ("f" ham-keys-insert-mode-activated :exit t)
  )

(define-global-minor-mode ryo-global-mode ryo-modal-mode
  (lambda ()
    (unless (minibufferp)
      (ryo-modal-mode 1))))

;; (ryo-global-mode 1) ;; If you want modal to be the default

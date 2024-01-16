(provide 'ni-keymap-ham-keys)
(require 'golden-ratio-scroll-screen)
(require 'jump-char)
(require 'goto-chg)
(require 'zygospore)
(require 'move-text)
(require 'expand-region)
(require 'ryo-modal)

;; Make sure our prefix key is unbound
(global-unset-key (key "M-,"))

;; Our prefixes, default is 'M-<edit>' & 'M-, <command>'
(defconst ham-keys-edit-key-prefix "M-")
(defconst ham-keys-edit-key-ryo-prefix "")
(defconst ham-keys-leader-key-prefix "M-, ")
(defconst ham-keys-leader-key-ryo-prefix ", ")

(defun ham-keys-edit (aKey aCommand &optional noryo noglobal)
  "Add the edit prefix to STRING."
  (unless noglobal
    (global-set-key (key (concat ham-keys-edit-key-prefix aKey)) aCommand))
  (unless noryo
    (ryo-modal-key (concat ham-keys-edit-key-ryo-prefix aKey) aCommand))
  )

(defmacro ham-keys-def-edit (&rest pairs)
  "Define multiple edit keybindings."
  `(progn
     ,@(mapcar (lambda (pair)
                 `(ham-keys-edit ,(car pair) ',(cadr pair)))
               pairs)))

(defun ham-keys-leader (aKey aCommand &optional noryo noglobal)
  "Add the leader prefix to STRING."
  (unless noglobal
    (global-set-key (key (concat ham-keys-leader-key-prefix aKey)) aCommand))
  (unless noryo
    (ryo-modal-key (concat ham-keys-leader-key-ryo-prefix aKey) aCommand))
  )

(defmacro ham-keys-def-leader (&rest pairs)
  "Define multiple leader keybindings."
  `(progn
     ,@(mapcar (lambda (pair)
                 `(ham-keys-leader ,(car pair) ',(cadr pair)))
               pairs)))

;; Use the same history ring for isearch and swiper
(defvaralias 'swiper-history 'regexp-search-ring)

;; expand-region-smart-cursor moves the cursor at the end of the selection
;; when the region expands beyond the initial starting point which allows us
;; to expand it down from there.
(setq expand-region-smart-cursor t)

(defun ni-modal-start-from-new-line ()
  (interactive)
  (move-end-of-line nil)
  (newline)
  (indent-for-tab-command))

(defun ni-modal-start-from-new-top-line ()
  (interactive)
  (previous-line)
  (ni-modal-start-from-new-line))

(defun ni-modal-insert-or-change-region ()
  "Kill active region if active"
  (interactive)
  (if mark-active (delete-region (region-beginning) (region-end)))
  (message "Insert mode actived"))

(defun ni-modal-delete-char-or-kill-region (arg)
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

(defun ni-modal-fif-at-point ()
  (interactive)
  (ni-counsel-rg-match
    nil pierre-search-file-patterns))

(defun ni-modal-comment-region-or-line-and-go-down (arg)
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

(defun ni-modal-uncomment-region-or-line-and-go-up (arg)
  "Kill active region if active"
  (interactive "p")
  (if mark-active
    (uncomment-region (region-beginning) (region-end))
    (progn
      (condition-case nil (uncomment-region (point-at-bol) (point-at-eol)) (error nil))
      (back-to-indentation)
      (next-line -1)))
  )

(ham-keys-def-edit
  ;; Commands
  ("p" counsel-M-x)

  ;; Movement
  ("y" backward-word)
  ("o" forward-word)
  ("u" forward-paragraph)
  ("i" backward-paragraph)

  ("l" forward-char)
  ("h" backward-char)
  ("j" next-line)
  ("k" previous-line)

  ("I" mc/mark-previous-like-this)
  ("U" mc/mark-next-like-this)
  ("Y" mc/mark-all-like-this)
  ("O" mc/edit-ends-of-lines)

  ("L" agl-search-word-forward)
  ("H" agl-search-word-backward)
  ("J" move-text-down)
  ("K" move-text-up)

  ("e" move-end-of-line)
  ("a" beginning-of-line)
  ("A" back-to-indentation)

  ;; so that we can hammer M-g M-g quickly
  ("g M-g" ni-goto-matching-bracket)
  ("g g" ni-goto-matching-bracket)
  ("g a" beginning-of-buffer)
  ("g l" goto-line-preview)
  ("g m" pop-to-mark-command)
  ("g e" end-of-buffer)
  ("<" goto-last-change)
  (">" goto-last-change-reverse)

  ("{" golden-ratio-scroll-screen-down)
  ("}" golden-ratio-scroll-screen-up)

  ;; Editing
  ("z" undo)
  ("x" ni-modal-delete-char-or-kill-region)
  ("c" kill-ring-save)
  ("v" yank)
  ("V" yank-pop)
  ("b" ni-modal-start-from-new-line :exit t)
  ("B" ni-modal-start-from-new-top-line :exit t)
  ("q" indent-region)
  ("Q" fill-paragraph)
  (";" ni-modal-comment-region-or-line-and-go-down)
  (":" ni-modal-uncomment-region-or-line-and-go-up)

  ;; Searching
  ("s" ni-swiper-isearch)
  ("*" swiper-thing-at-point)
  ("/" isearch-forward)
  ("?" isearch-backward)
  ("n" isearch-repeat-forward)
  ("N" isearch-repeat-backward)
  ("f" jump-char-forward)
  ("F" jump-char-backward)
  ("d" avy-goto-word-1)
  ("D" avy-goto-char)

  ;; Visual selection
  ("m" set-mark-command)
  ("w" er/expand-region)
  ("W" ni-select-current-line-and-forward-line)
  ("R" string-rectangle)
  ("'" er/mark-inside-quotes)
  ("\"" er/mark-outside-quotes)

  ;; Macros
  ("(" kmacro-start-macro)
  (")" kmacro-end-macro)
  ("t" kmacro-end-and-call-macro)
  )

(ham-keys-def-leader
  ("r" revert-buffer)

  ("f" ni-file-cache-find-file-at-point)
  ("F" find-file)
  ("d" direx:jump-to-project-file)
  ("j" ham-grep-regexp-current-dir)
  ("u" ham-grep-work-regexp)
  ("g" fzf-git-files)
  ("." ni-modal-fif-at-point)

  ("w" save-buffer)
  ("W" save-some-buffers)
  ("k" kill-buffer)
  ("b" ivy-switch-buffer)
  ("B" ibuffer)

  ("1" zygospore-toggle-delete-other-windows)
  ("2" split-window-below)
  ("3" split-window-right)
  ("0" delete-window)
  ("m" other-window)

  ("c" aflymake-mode-or-syntax-check)
  ("v" ham-fix-current-buffer)
)

;; Separate global key setups for special cases
(global-set-key (kbd "M-.") (make-ni-expand))

;; Modal mode toggles
(global-set-key (kbd "C-h C-v") 'ryo-modal-mode)
(global-set-key (kbd "M-SPC") 'ryo-modal-mode)
(global-set-key (kbd "<escape>") 'ryo-modal-mode)

;; Modal only keys
(ryo-modal-keys
  ("r" ni-modal-insert-or-change-region :exit t))

(define-global-minor-mode ryo-global-mode ryo-modal-mode
  (lambda ()
    (unless (minibufferp)
      (ryo-modal-mode 1))))

;; (ryo-global-mode 1) ;; If you want modal to be the default

;; Try to unfuck the arrow keys when running in the terminal
(IsTerminal
  ;; Seems to be the only way to get arrows to work, unsetting in the local
  ;; maps isnt enough. Somehow the actual shortcuts that are bound there still
  ;; work after this... idk whats going on terminal is wierd xD
  (global-unset-key (key "M-I"))
  (global-unset-key (key "M-O"))
  )

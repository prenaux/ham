(provide 'ni-keymap-modal)
(require 'ryo-modal)
(require 'jump-char)
(require 'general)
(require 'goto-chg)
(require 'zygospore)
(require 'move-text)

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

(ryo-modal-keys
  ("r" ni-modal-insert-or-change-region :exit t)

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
  (";" agl-comment-and-go-down)
  (":" agl-uncomment-and-go-up)

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

  ;; Commands
  ("p" counsel-M-x)

  (", r" revert-buffer)

  (", f" ni-file-cache-find-file-at-point)
  (", F" find-file)
  (", d" direx:jump-to-project-file)
  (", j" ham-grep-regexp-current-dir)
  (", u" ham-grep-work-regexp)
  (", g" fzf-git-files)
  (", ." ni-modal-fif-at-point)

  (", w" save-buffer)
  (", W" save-some-buffers)
  (", k" kill-buffer)
  (", b" ivy-switch-buffer)
  (", B" ibuffer)

  (", 1" zygospore-toggle-delete-other-windows)
  (", 2" split-window-below)
  (", 3" split-window-right)
  (", 0" delete-window)
  (", m" other-window)

  (", c" aflymake-mode-or-syntax-check)
  (", v" ham-fix-current-buffer)
  )

(ryo-modal-keys
  (:norepeat t)
  ("0" "C-M-0")
  ("1" "C-M-1")
  ("2" "C-M-2")
  ("3" "C-M-3")
  ("4" "C-M-4")
  ("5" "C-M-5")
  ("6" "C-M-6")
  ("7" "C-M-7")
  ("8" "C-M-8")
  ("9" "C-M-9"))

(add-hook 'find-file-hook 'ryo-modal-mode)

(define-global-minor-mode ryo-global-mode ryo-modal-mode
  (lambda ()
    (unless (minibufferp)
      (ryo-modal-mode 1))))

(global-set-key (kbd "C-h C-v") 'ryo-modal-mode)
(global-set-key (kbd "M-SPC") 'ryo-modal-mode)
(global-set-key (kbd "<escape>") 'ryo-modal-mode)

(ryo-global-mode 1)

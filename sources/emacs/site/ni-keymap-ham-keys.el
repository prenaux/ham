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

(defun ham-keys-edit (aKey aCommand)
  "Add the edit prefix to STRING."
  (if (or
        ;; Dont bind these keys globally
        (string-equal aKey "n")
        (string-equal aKey "p"))
    (progn
      (bind-key (concat "M-" aKey) aCommand)
      (bind-key (concat "C-M-" aKey) aCommand))
    (progn
      (bind-key* (concat "M-" aKey) aCommand)
      (bind-key* (concat "C-M-" aKey) aCommand)))
  (ryo-modal-key aKey aCommand)
  )

(defmacro ham-keys-def-edit (&rest pairs)
  "Define multiple edit keybindings."
  `(progn
     ,@(mapcar (lambda (pair)
                 `(ham-keys-edit ,(car pair) ',(cadr pair)))
               pairs)))

(defun ham-keys-leader (aLeader aKey aCommand)
  "Add the leader prefix to STRING."
  (bind-key* (concat "M-" aLeader " " aKey) aCommand)
  (IsTerminal ;; oh sweet terminal, if only you could send the whole full keys for all xD
    (bind-key* (concat "M-" aLeader " C-M-" aKey) aCommand)
    (bind-key* (concat "M-" aLeader " M-" aKey) aCommand)
    )
  (bind-key* (concat "C-M-" aLeader " C-M-" aKey) aCommand)
  (ryo-modal-key (concat aLeader " " aKey) aCommand)
  )

(defmacro ham-keys-def-leader (aLeader &rest pairs)
  "Define multiple leader keybindings."
  `(progn
     ,@(mapcar (lambda (pair)
                 `(ham-keys-leader ,aLeader ,(car pair) ',(cadr pair)))
               pairs)))

(defun ham-keys-insert-or-change-region ()
  "Kill active region if active"
  (interactive)
  (if mark-active (delete-region (region-beginning) (region-end)))
  (message "Insert mode actived"))

(defun ham-keys-insert-mode-activated ()
  "Notifies that the insert mode has been activated"
  (interactive)
  (message "Insert mode actived"))

(defun ham-keys-keyboard-quit ()
  (interactive)
  (mc/keyboard-quit)
  (keyboard-escape-quit))

(ham-keys-def-edit
  ;; Commands
  ("g" ham-keys-keyboard-quit)
  ("p" repeat)
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

  ;; Editing
  ("q" indent-region)
  ("w" agl-backward-delete-word)
  ("e" agl-delete-word)

  ("a" ni-select-current-line-and-forward-line)
  ("s" ni-start-from-new-line)
  ("S-s" ni-start-from-new-top-line)
  ("d" delete-char)

  ("z" undo)
  ("x" ni-delete-char-or-kill-region)
  ("c" kill-ring-save)
  ("v" yank)
  ("b" yank-pop)

  ;; Visual selection
  ("t" set-mark-command)
  ("r" er/expand-region)

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
  ("g" pierre-rg-match-in-current)
  ("t" pierre-rg-match-in-work)
  ("v" avy-goto-char)
  ("," back-button-local-backward)
  ("." back-button-local-forward)
  ("p" agl-search-word-backward)
  ("n" agl-search-word-forward)
  ("o" ni-counsel-rg-at-point)
  )

(ham-keys-def-leader ","
  ("," counsel-M-x)

  ("u" counsel-fzf)
  ("i" find-file) ;; Use to create new files
  ("o" ni-file-cache-find-file-at-point)

  ("b" ivy-switch-buffer)
  ("d" direx:jump-to-project-file)

  ("e" pierre-join-line)

  ("j" ham-grep-regexp-current-dir)
  ("y" ham-grep-work-regexp)
  ("h" qrr)

  ("s" save-buffer)
  ("w" save-some-buffers) ;; Save all buffers
  ("k" kill-buffer)
  ("t" revert-buffer)

  ("1" zygospore-toggle-delete-other-windows)

  ("r" er/contract-region)

  ("m" mc/mark-all-like-this)
  ("8" mc/edit-ends-of-lines)

  ("a" mark-whole-buffer)

  ("p" agl-run-last-shell-command)

  ("q" fill-paragraph)

  ("c" ni-comment-region-or-line-and-go-down)
  ("v" ni-uncomment-region-or-line-and-go-up)
  )

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

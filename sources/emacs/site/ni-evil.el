(provide 'ni-evil)

(add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs/site/evil"))
(require 'evil)
(evil-mode 1)

(setq evil-search-wrap nil)
(setq evil-search-module 'evil-search)
(evil-select-search-module 'evil-search-module 'evil-search)

(GNUEmacsMin28
  (evil-set-undo-system 'undo-redo))

;;=================================================================
;; evil-owl (registers preview)
;;=================================================================
(require 'evil-owl)
(setq evil-owl-max-string-length 500)
(add-to-list 'display-buffer-alist
  '("*evil-owl*"
     (display-buffer-in-side-window)
     (side . bottom)
     (window-height . 0.3)))
(evil-owl-mode)
(setq evil-owl-idle-delay 0.3)
(diminish 'evil-owl-mode)

;;=================================================================
;; Relative line numbers
;;=================================================================
(IsNotTerminal
  (defvar ni-rel-line-current-line-number 0)

  (setq linum-format 'ni-rel-line-numbers)

  (defun ni-rel-line-numbers (line-number)
    (let ((test2 (- line-number ni-rel-line-current-line-number)))
      (propertize
        (number-to-string (cond ((<= test2 0) (* -1 test2))
                            ((> test2 0) test2)))
        'face 'linum)))

  (defadvice linum-update (around ni-rel-line-update)
    (let ((ni-rel-line-current-line-number (line-number-at-pos)))
      ad-do-it))
  (ad-activate 'linum-update)

  (global-linum-mode t))

;;=================================================================
;; Leader key
;;=================================================================
(require 'general)
(general-create-definer tyrant-def
  :states '(normal insert motion emacs)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "M-SPC")
(tyrant-def "" nil)

(general-def universal-argument-map
  "SPC u" 'universal-argument-more)

(tyrant-def
  "f" 'ni-file-cache-find-file-at-point
  "d" 'direxjump-to-project-file
  "j" 'ham-grep-regexp-current-dir
  "k" 'ham-grep-work-regexp
  "g" 'fzf-git-files
  "s" 'ni-swiper-isearch
  "w" 'save-buffer
  "b" 'ivy-switch-buffer
  "B" 'ibuffer
  "x" 'counsel-M-x
  "." (lambda () (interactive)
        (ni-counsel-rg-match
          nil pierre-search-file-patterns))
  )

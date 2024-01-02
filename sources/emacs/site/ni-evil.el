(provide 'ni-evil)

(add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs/site/evil"))
(require 'evil)
(evil-mode 1)

(setq evil-search-wrap nil)
(define-key evil-visual-state-map (kbd "y") 'kill-ring-save)
(evil-select-search-module 'evil-search-module 'evil-search)

(GNUEmacsMin28
  (evil-set-undo-system 'undo-redo))

;; Relative line numbers
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

(global-linum-mode t)

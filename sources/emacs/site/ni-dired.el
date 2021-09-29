(provide 'ni-dired)
(add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs/site/dired"))

(require 'dired-subtree)
(require 'dired-narrow)
(require 'dired-rainbow)
(require 'dired-collapse)

(defun ni-dired-toggle-or-open ()
  "Toggle subtree or open the file."
  (interactive)
  (if (file-directory-p (dired-get-file-for-visit))
      (progn
    (dired-subtree-toggle)
    (revert-buffer))
    (dired-find-file)))

(setq dired-subtree-line-prefix (lambda (depth) (make-string (* 2 depth) ?\s)))
(setq dired-subtree-use-backgrounds nil)

(autoload 'dired-jump "dired-x" "Jump to dired corresponding current buffer.")
(autoload 'dired-jump-other-window "dired-x" "jump to dired in other window.")

(global-set-key (key "C-x C-j") 'dired-jump-other-window)
(global-set-key (key "C-x j") 'dired-jump)

(define-key dired-mode-map (kbd "<tab>") #'dired-subtree-toggle)
(define-key dired-mode-map (kbd "<backspace>") #'dired-subtree-cycle)
(define-key dired-mode-map (kbd "/") #'dired-narrow)

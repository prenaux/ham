(provide 'ni-dired)
(add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs/site/dired"))

(require 'dired-subtree)
(require 'dired-narrow)
(require 'dired-rainbow)
(require 'dired-collapse)

(defun ni-set-dired-buffer-name ()
  "change buffer name to end with slash"
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
        (rename-buffer (concat "*dired: " name "/*") t))))

(add-hook 'dired-mode-hook 'ni-set-dired-buffer-name)

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

(defun ni-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://xahlee.info/emacs/emacs/dired_sort.html'
Version: 2018-12-23 2022-04-07"
  (interactive)
  (let (xsortBy xarg)
    (setq xsortBy (completing-read "Sort by:" '( "size" "date" "name" )))
    (cond
     ((equal xsortBy "name") (setq xarg "-Al "))
     ((equal xsortBy "date") (setq xarg "-Al -t"))
     ((equal xsortBy "size") (setq xarg "-Al -S"))
     ((equal xsortBy "dir") (setq xarg "-Al --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other xarg )))

(define-key dired-mode-map (kbd "s") 'ni-dired-sort)

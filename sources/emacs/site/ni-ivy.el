(provide 'ni-ivy)

(add-to-list 'load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/ivy"))

(require 'counsel)
(require 'ivy)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
;; intentional space before end of string
(setq ivy-count-format "(%d/%d) ")
(setq ivy-initial-inputs-alist nil)

(global-set-key (kbd "M-x") #'counsel-M-x)

(defmacro ni-ivy-quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  `(progn
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    ,@body))
     (minibuffer-keyboard-quit)))

(defun ni-ivy-quit-and-counsel-find-file (&optional arg)
  "Cancel ivy and ."
  (interactive "P")
  (ni-ivy-quit-and-run
   (counsel-find-file)))

;; C-f always open counsel-find-file when in an ivy minibuffer, this is a
;; useful fallback
(define-key ivy-minibuffer-map (kbd "C-f") 'ni-ivy-quit-and-counsel-find-file)

(provide 'ni-evil)

(add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs/site/evil"))
(require 'evil)
(evil-mode 1)

(defun ni-evil-jk ()
  (interactive)
  (let* ((initial-key ?j)
         (final-key ?k)
         (timeout 0.5)
         (event (read-event nil nil timeout)))
    (if event
        ;; timeout met
        (if (and (characterp event) (= event final-key))
            (evil-normal-state)
          (insert initial-key)
          (push event unread-command-events))
      ;; timeout exceeded
      (insert initial-key))))

(define-key evil-insert-state-map (kbd "j") 'ni-evil-jk)
(define-key evil-motion-state-map (kbd "TAB") nil)
(define-key evil-visual-state-map (kbd "y") 'kill-ring-save)
(setq evil-search-wrap nil)
(setq evil-search-module 'evil-search)

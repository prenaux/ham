(provide 'ni-org)
(require 'ni-base)

;; Base org mode setup
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-log-done t)

(add-hook 'org-mode-hook
          (lambda ()
            (setq truncate-lines nil))
          t)

;; Change back the paragraph definition to be empty lines after a block of text
(defmacro with-default-paragraph-definition (&rest body)
  "Evaluate body forms using the default definition of a paragraph."
  `(let ((paragraph-start (default-value 'paragraph-start))
         (paragraph-separate (default-value 'paragraph-separate)))
     ,@body))

(defalias 'my-org-mark-paragraph 'mark-paragraph)
(defadvice my-org-mark-paragraph
  (around my-org-mark-paragraph-advice activate)
  (with-default-paragraph-definition ad-do-it))

(defalias 'my-org-forward-paragraph 'forward-paragraph)
(defadvice my-org-forward-paragraph
  (around my-org-forward-paragraph-advice activate)
  (with-default-paragraph-definition ad-do-it))

(defalias 'my-org-backward-paragraph 'backward-paragraph)
(defadvice my-org-backward-paragraph
  (around my-org-backward-paragraph-advice activate)
  (with-default-paragraph-definition ad-do-it))

(defun my-org-paragraph-overrides ()
  "Use the default paragraph definitions in org-mode
        when marking or moving by paragraph."
  (local-set-key [remap mark-paragraph] 'my-org-mark-paragraph)
  (local-set-key [remap forward-paragraph] 'my-org-forward-paragraph)
  (local-set-key [remap backward-paragraph] 'my-org-backward-paragraph))

(add-hook 'org-mode-hook 'my-org-paragraph-overrides)

(defun flatten(x)
  (cond ((null x) nil)
        ((listp x) (append (flatten (car x)) (flatten (cdr x))))
        (t (list x))))


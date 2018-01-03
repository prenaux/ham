(provide 'ni-org)
(require 'ni-base)

;; Base org mode setup
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-log-done t)

(add-hook 'org-mode-hook
          (lambda ()
            (setq truncate-lines nil))
          t)

;;--------------------------------------------------------------------------
;; Change back the paragraph definition to be empty lines after a block of
;; text
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

;;--------------------------------------------------------------------------
;; Easier nav
(defun my-org-forward-and-preview ()
    "Go to same level next heading and show preview in dedicated buffer"
    (hide-subtree)
    (org-speed-move-safe (quote outline-next-visible-heading))
    (show-children)
    (org-tree-to-indirect-buffer)
    )
(defun my-org-back-and-preview ()
    "Go to same level previous heading and show preview in dedicated buffer"
    (hide-subtree)
    (org-speed-move-safe (quote outline-previous-visible-heading))
    (show-children)
    (org-tree-to-indirect-buffer)
    )
(defun my-org-up-back-and-preview ()
    "Go to previous level heading and show preview in dedicated buffer"
    (org-speed-move-safe (quote outline-up-heading))
    (org-tree-to-indirect-buffer)
    (hide-subtree)
    )
(defun my-org-up-forward-and-preview ()
    "Go to previous level next heading and show preview in dedicated buffer"
    (org-speed-move-safe (quote outline-up-heading))
    (hide-subtree)
    (org-speed-move-safe (quote outline-next-visible-heading))
    (org-tree-to-indirect-buffer)
    )
(defun my-org-inside-and-preview ()
    "Go to next level heading and show preview in dedicated buffer"
    (org-speed-move-safe (quote outline-next-visible-heading))
    (show-children)
    (org-tree-to-indirect-buffer)
    )

;;--------------------------------------------------------------------------
;; Org mode hook
(defun my-org-mode-overrides ()
  "Use the default paragraph definitions in org-mode
        when marking or moving by paragraph."
  (setq org-use-speed-commands t)
  (add-to-list 'org-speed-commands-user '("l" my-org-inside-and-preview))
  (add-to-list 'org-speed-commands-user '("j" my-org-forward-and-preview))
  (add-to-list 'org-speed-commands-user '("k" my-org-back-and-preview))
  (add-to-list 'org-speed-commands-user '("J" my-org-up-forward-and-preview))
  (local-set-key [remap mark-paragraph] 'my-org-mark-paragraph)
  (local-set-key [remap forward-paragraph] 'my-org-forward-paragraph)
  (local-set-key [remap backward-paragraph] 'my-org-backward-paragraph)
  (local-set-key "\t" 'pabbrev-expand-maybe)
  (local-set-key [tab] 'pabbrev-expand-maybe)
)

(add-hook 'org-mode-hook 'my-org-mode-overrides)

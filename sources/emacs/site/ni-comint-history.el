(provide 'ni-comint-history)
(require 'cl)

(defvar comint-history-file "~/.emacs.d/comint-history")

(defun save-comint-history (str)
  (with-current-buffer (find-file-noselect comint-history-file)
    (goto-char (point-max))
    (if (not (string-match "\\`\\s *\\'" str))
        (progn (insert str)
               (basic-save-buffer)))))

(defun buffer-to-list (name)
  "Takes buffer NAME, returns the contents of tha buffer as a list of strings"
  (with-current-buffer name
    (save-excursion
      (let ((l '())
            (max-line (line-number-at-pos (point-max))))
        (goto-char (point-min))
        (while (not (eq max-line (line-number-at-pos)))
          (add-to-list 'l (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
          (forward-line))
        l))))


(defun remove-duplicates-str (list)
  (delete "" (remove-duplicates list :test 'equal :from-end t)))

(defun ido-complete-comint-history ()
  (interactive)
  (insert
   (ido-completing-read "comint-history: "
                        (remove-duplicates-str
                         (buffer-to-list
                         (find-file-noselect comint-history-file))))))

;; Use IDO fro comint history
;; See: http://paste.lisp.org/display/37129 (modified to remove duplicate)
(defun ido-complete-comint-input-ring ()
  "Fetch a previous element from history using ido-like completion.
This function searches *all* elements in history, not just
previous or next elements like Comint's normal completion.
So you can bind it to both M-r and M-s."
  (interactive)
  (unless (null comint-input-ring)
    (let* ((elt (ido-completing-read "History: " (delete "" (remove-duplicates (cddr (ring-elements comint-input-ring)) :test #'string=)) nil t))
           (pos (comint-previous-matching-input-string-position
                 (regexp-quote elt) 1)))
      (unless (null pos)
        (setq comint-input-ring-index pos)
        (message "History item: %d" (1+ pos))
        (comint-delete-input)
        (insert (ring-ref comint-input-ring pos))))))

(custom-set-variables
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
)

(defun ni-comint-history-hook ()
  (local-set-key (kbd "M-s") 'ido-complete-comint-history)
  (local-set-key (kbd "M-r") 'ido-complete-comint-input-ring))

(add-hook 'comint-input-filter-functions 'save-comint-history)
(add-hook 'shell-mode-hook 'ni-comint-history-hook)
(add-hook 'ham-shell-mode-hook 'ni-comint-history-hook)
(add-hook 'sql-interactive-mode-hook 'ni-comint-history-hook)

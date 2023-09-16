(provide 'ham-fix)

(defvar ham-fix-executable
  (concat (getenv "WORK") "/ham/bin/ham-fix")
  "The ham-fix executable to use.")

(defun ham-fix-current-buffer ()
  "run a ham-fix on the current file and revert the buffer"
  (interactive)
  (save-buffer)
  (if (eq 0 (shell-command
             (format "%s %s"
                     (shell-quote-argument ham-fix-executable)
                     (shell-quote-argument (buffer-file-name)))))
      (progn
        (message "ham-fix SUCCEEDED")
        (revert-buffer t t t)
        (aflymake-mode-or-syntax-check))
    (progn
      (message "ham-fix FAILED"))))

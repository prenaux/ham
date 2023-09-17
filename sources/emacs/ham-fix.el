(provide 'ham-fix)

(defvar ham-fix-executable
  (concat (getenv "WORK") "/ham/bin/ham-fix")
  "The ham-fix executable to use.")

(Windows
 (setq ham-fix-executable (concat (getenv "WORK") "/ham/bin/ham-fix.cmd")))

(defun ham-fix-current-buffer ()
  "run a ham-fix on the current file and revert the buffer"
  (interactive)

  (message (concat "ham-fix-current-buffer: " (buffer-file-name)))
  (let ((fix-cmd (format "%s %s"
                   (shell-quote-argument ham-fix-executable)
                   (shell-quote-argument (buffer-file-name))))
         (aflymake-was-enabled (bound-and-true-p aflymake-mode)))

    (when aflymake-was-enabled
      (aflymake-mode -1)
      (message "ham-fix-current-buffer: disabled aflymake"))

    (message "ham-fix-current-buffer: save-buffer")
    (save-buffer)

    (message (concat "ham-fix-current-buffer: shell-command:" fix-cmd))
    (if (zerop (call-process-shell-command fix-cmd nil nil nil))
      (progn
        (message "ham-fix-current-buffer: succeeded")
        (revert-buffer t t t))
      (progn
        (message "ham-fix-current-buffer: failed")))

    (if aflymake-was-enabled
      (progn
        (aflymake-mode-or-syntax-check)
        (message "ham-fix-current-buffer: done, aflymake-mode re-enabled"))
      (message "ham-fix-current-buffer: done, no aflymake"))))

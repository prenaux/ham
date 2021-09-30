(provide 'ni-direx)
(require 'direx)

(defun ni-set-direx-buffer-name ()
  "change buffer name to end with slash"
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
        (rename-buffer (concat "*direx: " name "/*") t))))

(add-hook 'direx:direx-mode-hook 'ni-set-direx-buffer-name)

(defun direx:jump-to-project-file ()
  (interactive)
  (let ((filename buffer-file-name)
        (dirname default-directory))
    (direx:find-directory (projectile-project-root))
    (cond (filename
           (direx:goto-item-for-tree (direx:make-regular-file filename)))
          (dirname
           (direx:goto-item-for-tree (direx:make-directory dirname))))))

(defun direx:jump-to-project-file-other-window ()
  (interactive)
  (let ((filename buffer-file-name)
        (dirname default-directory))
    (direx:find-directory-other-window (projectile-project-root))
    (cond (filename
           (direx:goto-item-for-tree (direx:make-regular-file filename)))
          (dirname
           (direx:goto-item-for-tree (direx:make-directory dirname))))))

(defun direx:jump-to-directory-at-point! (goto-root-item-p)
  (interactive "P")
  (when (eq major-mode 'direx:direx-mode)
    (let* ((item (direx:item-at-point!))
           (name (direx:file-full-name (direx:item-tree item)))
           (dir (if (file-directory-p name) name
                  (direx:directory-dirname name))))
      (if (not (file-exists-p dir))
          (error "%s not found" dir)
        (direx:find-directory dir)
        (if goto-root-item-p
            (direx:goto-item-for-tree (direx:item-tree direx:root-item))
          (direx:goto-item-for-tree (direx:item-tree item)))))))

(defun direx:jump-to-directory-upward (goto-root-item-p)
  (interactive "P")
  (when (eq major-mode 'direx:direx-mode)
    (let* ((item (direx:item-at-point))
           (cur-dir default-directory)
           (up-dir (direx:directory-dirname
                    (direx:file-full-name (direx:item-tree direx:root-item)))))
      (if (not (file-exists-p up-dir))
          (error "%s not found" up-dir)
        (direx:find-directory up-dir)
        (cond
         (goto-root-item-p
          (direx:goto-item-for-tree (direx:item-tree direx:root-item)))
         (item (direx:goto-item-for-tree (direx:item-tree item)))
         (t (direx:goto-item-for-tree (direx:make-directory cur-dir))))))))

;; Set the current directory as the only directory visible
(define-key direx:direx-mode-map  (kbd "x")           'direx:jump-to-directory-at-point!)
;; Visit the parent directory
(define-key direx:direx-mode-map  (kbd "<backspace>") 'direx:jump-to-directory-upward)

(provide 'ni-templates)
(require 'ni-base)

;;;======================================================================
;;; Template
;;;======================================================================
(agl-begin-time-block "Template")

(defun tpl-cpp-guards ()
  (interactive)
  (let ((GUARD (replace-regexp-in-string
                        "[\\.-]"
                        "_"
                        (upcase (concat "__" (buffer-name) "_" (ni-uuid3) "__"))
                         )))
    (beginning-of-buffer)
    (insert "#pragma once") (newline)
    (insert (concat "#ifndef " GUARD)) (newline)
    (insert (concat "#define " GUARD)) (newline)
    (end-of-buffer)
    (insert (concat "#endif // " GUARD)) (newline)
  ))


(defun tpl-js-flow-type ()
  (interactive)
  (let ((TYPE (read-string "typename: ")))
    (insert (concat "/*: " TYPE " */"))
  )
)

(provide 'ni-sql)
(require 'expand-region)
(require 'sql-indent)
(require 'pgsql-minor-mode)

(Windows
 (setq sql-postgres-program (concat HAM_HOME "/toolsets/postgres/nt-x86/bin/psql.exe")))
(OSX
 (setq sql-postgres-program "/usr/local/bin/psql"))

;; default connection list
(setq sql-connection-alist
      '((local
         (sql-product 'postgres)
         (sql-port 5432)
         (sql-server "localhost")
         (sql-user "postgres")
         (sql-database "postgres")
         (sql-password "123")
        )
       )
)

(defun ni-sql-connect-server (connection)
  "Connect to the input server using sql-servers-list"
  (interactive
   ;; use ido-completing-read or completing-read
   (list (ido-completing-read "Select server: "
                              (mapcar (lambda (item)
                                        (symbol-name (nth 0 item)))
                                      sql-connection-alist)))
  )
  ;; get the sql connection info and product from the sql-connection-alist
  (let* ((connection-info (assoc (intern connection) sql-connection-alist))
         (connection-product (nth 1 (nth 1 (assoc 'sql-product connection-info)))))
    ;; (message (concat "INFO:" connection-info))
    ;; (message (concat "PROD:" connection-product))
    ;; connect to server
    (setq sql-product connection-product)
    (if current-prefix-arg
        (sql-connect connection connection)
      (sql-connect connection)
    )
  )
)

(defun sql-indent-string ()
  "Indents the string under the cursor as SQL."
  (interactive)
  (save-excursion
    (er/mark-inside-quotes)
    (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
           (pos (region-beginning))
           (column (progn (goto-char pos) (current-column)))
           (formatted-text (with-temp-buffer
                             (insert text)
                             (delete-trailing-whitespace)
                             (sql-indent-buffer)
                             (replace-string "\n" (concat "\n" (make-string column (string-to-char " "))) nil (point-min) (point-max))
                             (buffer-string))))
      (delete-region (region-beginning) (region-end))
      (goto-char pos)
      (insert formatted-text))))

(defun ig-get-previous-indentation ()
  "Get the column of the previous indented line"
  (interactive)
  (save-excursion
    (progn
      (move-beginning-of-line nil)
      (skip-chars-backward "\n \t")
      (back-to-indentation))
    (current-column)))

(defun ig-get-current-indentation ()
  "Return column at current indentation"
  (interactive)
  (save-excursion
    (progn
      (back-to-indentation)
      (current-column))))

(defun ig-point-at-current-indentation ()
  "Return point at current indentation"
  (interactive)
  (save-excursion
    (progn
      (move-to-column (ig-get-current-indentation))
      (point))))

(defun ig-point-at-column-on-line (col)
  "Returns the point at `col` on the current line"
  (interactive)
  (save-excursion
    (progn
      (move-to-column col)
      (point))))

(defun ig-move-line-to-column (col)
  "Move the line to col; fill with all spaces if moveing forward"
  (interactive "p")
  (let ((point-at-cur-indent (ig-point-at-current-indentation))
        (col-at-cur-indent (ig-get-current-indentation)))
    (cond (
           (= col 0)
           ;; delete to beginning of line or do nothing
           (if (= col-at-cur-indent 0)
               nil
             (delete-region point-at-cur-indent (ig-point-at-column-on-line 0))))
          (
           (< col col-at-cur-indent)
           ;; delete from our current point BACK to col
           (delete-region (ig-point-at-column-on-line col) point-at-cur-indent))
          (
           (> col col-at-cur-indent)
           ;; delete all text from indent to beginning of line
           (progn
             (delete-region point-at-cur-indent (ig-point-at-column-on-line 0))
             (move-beginning-of-line nil)
             ;; add spaces forward
             (insert-string (make-string col ?\s)))))))

(defun ig-indent-sql ()
  "Indent by `tab-width` at most 1 time greater than the previously indented line otherwise go to the beginning of the line indent forward by `tab-width`"
  (interactive)
  (let ((previous (ig-get-previous-indentation))
        (current (ig-get-current-indentation)))
    (cond ( ;; exactly at previous line's indentation
           (= previous current)
           (ig-move-line-to-column (+ current tab-width)))

          ( ;; current is greater than previous
           (> current previous)
           ;; exactly at one indentation forward from previous lines indent
           (if (= tab-width (- current previous))
               ;; move line to beginning
               (ig-move-line-to-column 0)
             ;; go back to previous indentation level
             (ig-move-line-to-column previous)))

          (t
           (ig-move-line-to-column (+ current tab-width))))))

(defun ni-sql-set-ig-indent ()
  ""
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ig-indent-sql)
)

(defun ni-sql-beginning-of-sp ()
  ""
  (interactive)
  (search-backward-regexp
   "^\\(^SELECT\\s-+ni_utils_drop_function\\)\\|\\(^DROP\\s-+FUNCTION\\s-+IF\\s-+EXISTS\\)")
)

(defun ni-sql-end-of-sp()
  ""
  (interactive)
  (search-forward-regexp "LANGUAGE\\s-+plpgsql;\\s-*$")
)

(defun ni-sql-mark-sp ()
  ""
  (interactive)
  ;; go to the end of the SP
  (ni-sql-end-of-sp)
  ;; enable mark
  (set-mark-command nil)
  ;; go to the beginning of the SP
  (ni-sql-begining-of-sp)
  ;; dont cancel the mark
  (setq deactivate-mark nil)
)

(defun ni-sql-eval-sp ()
  ""
  (interactive)
  (save-excursion
    (ni-sql-mark-sp)
    (sql-send-region (region-beginning) (region-end))
  )
)

(defun ni-sql-run-last-sql-cmd ()
  ""
  (interactive)
  (if (get-buffer (sql-find-sqli-buffer))
      (save-excursion
        (let ((currentBuffer (buffer-name)))
          (pop-to-buffer (sql-find-sqli-buffer))
          (goto-char (point-max))
          (comint-previous-input 1)
          (comint-send-input)
          (pop-to-buffer currentBuffer)
        ))
    (message "No SQL process started."))
)

(defun ni-sql-eval-sp-and-run-last-sql-cmd ()
  ""
  (interactive)
  (save-excursion
    (ni-sql-mark-sp)
    (sql-send-region (region-beginning) (region-end))
    (ni-sql-run-last-sql-cmd))
)

(defvar ni-sql-sp-mode-is-on nil
  "Buffer local variable to store pgsql-mode state.")

(defvar ni-sql-sp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-a")   'ni-sql-beginning-of-sp)
    (define-key map (kbd "C-M-e")   'ni-sql-end-of-sp)
    (define-key map (kbd "C-c C-v") 'ni-sql-mark-sp)
    (define-key map (kbd "C-c C-z") 'ni-sql-eval-sp)
    (define-key map (kbd "C-c C-x") 'ni-sql-eval-sp-and-run-last-sql-cmd)
    map)
  "keymap for `ni-sql-sp-mode'.")

(define-minor-mode ni-sql-sp-mode
  "ni-sql stored procedure minor mode."
  :global nil
  :variable ni-sql-sp-mode-is-on
  :keymap ni-sql-sp-mode-map
  (make-local-variable 'ni-sql-sp-mode-is-on)
)

(define-derived-mode ni-sql-mode sql-mode "ni-sql"
  "Major mode for editing SQL."
)
;; (add-hook 'ni-sql-mode-hook #'pgsql-mode)
(add-hook 'ni-sql-mode-hook #'ni-sql-sp-mode)
(add-hook 'ni-sql-mode-hook 'ni-sql-set-ig-indent)

(add-to-list 'auto-mode-alist '("\\.sql\\'" . ni-sql-mode))
(add-to-list 'auto-mode-alist '("\\.psql\\'" . ni-sql-mode))
(add-to-list 'auto-mode-alist '("\\.mysql\\'" . ni-sql-mode))
(add-to-list 'auto-mode-alist '("\\.pks\\'" . ni-sql-mode)) ;; oracle
(add-to-list 'auto-mode-alist '("\\.pkb\\'" . ni-sql-mode)) ;; oracle

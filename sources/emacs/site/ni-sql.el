(provide 'ni-sql)
(require 'expand-region)
(require 'sql-indent)
(require 'pgsql-minor-mode)

;; default connection list
(setq sql-connection-alist
      '(("local"
         (sql-product 'postgres)
         (sql-port 5432)
         (sql-server "localhost")
         (sql-user "postgres")
         (sql-database "postgres")
         (sql-password "")
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

(define-derived-mode ni-sql-mode sql-mode "ni-sql"
  "Major mode for editing SQL."
)
(add-hook 'ni-sql-mode-hook #'pgsql-mode)
(add-hook 'ni-sql-mode-hook 'ni-sql-set-ig-indent)

(add-to-list 'auto-mode-alist '("\\.sql\\'" . ni-sql-mode))
(add-to-list 'auto-mode-alist '("\\.psql\\'" . ni-sql-mode))
(add-to-list 'auto-mode-alist '("\\.mysql\\'" . ni-sql-mode))

(global-set-key (kbd "C-x C-c") 'ni-sql-connect-server)

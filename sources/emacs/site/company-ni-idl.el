(require 'company)
(require 'company-dabbrev)
(require 's)
(require 'dash)

(ni-add-to-PATH-front (concat (getenv "WORK") "/niSDK/bin"))

;; when t filter to ONLY use the idl-query results, this is always set when using
(setq company-ni-idl-idl-query-only nil)
;; when enabled merge the dabbrev and idl-query results, this means that the list that popups automatically
(setq company-ni-idl-merge-dabbrev  t)
;; the idl-query command to use to query the candidates
(setq company-ni-idl-query-command  "cached_candidates")

(defun ni-idl-build-cache ()
  (interactive)
  (agl-bash-cmd-to-string (concat "ni-idl-query build_cache"))
  nil
)

(defun company-ni-idl-complete ()
  (interactive)
  (company-abort)
  (setq company-ni-idl-idl-query-only t)
  (let ((r (company-complete-common)))
    (setq company-ni-idl-idl-query-only nil)
    r))

(defun company-ni-idl-candidates (arg)
  ;; (message (concat "... ARG:" arg))
  (let ((time-start (float-time))
        (cands
         (delete
          nil ;; delete the last nil from the list
          (let ((raw
                 (agl-bash-cmd-to-string (concat "ni-idl-query " company-ni-idl-query-command " " arg))
                ))
            (mapcar (lambda(line)
                      (if (s-present? line)
                          (progn
                            ;; (message (concat "LINE:" line))
                            (let ((items (mapcar (lambda(el) (s-trim el))
                                                 (split-string line ";"))))
                              (propertize
                               (nth 1 items)
                               'anno (nth 2 items)
                               'meta (nth 3 items)
                               'doc-buffer (nth 4 items)
                              )
                            )
                          )
                        (progn
                          nil
                        )
                      )
                    )
                    (split-string raw "\n")
            )
          ))))
    ;; (message "time: %f" (- (float-time) time-start))
    cands
  )
)

(defun company-ni-idl-grab-symbol ()
  (interactive)
  (message (if (looking-at "\\_>")
               (buffer-substring (point) (save-excursion (skip-syntax-backward "w_")
                                                         (point)))
             (unless (and (char-after) (memq (char-syntax (char-after)) '(?w ?_)))
               ""))))

(defun company--strip-duplicates (list)
  (let ((new-list nil))
    (while list
      (when (and (car list) (not (member (car list) new-list)))
        (setq new-list (cons (car list) new-list)))
      (setq list (cdr list)))
    (nreverse new-list)))

(defun company-ni-idl (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-ni-idl))
    (prefix
     ;; (message "PREFIX:" arg)
     (cond
      (company-ni-idl-idl-query-only
       (company-ni-idl-grab-symbol))
      (t
       (company-grab-word))
     )
    )
    (candidates

     (let ((dabbrev-candidates
            (company--strip-duplicates (company-dabbrev--search (company-dabbrev--make-regexp arg)
                                                                company-dabbrev-time-limit
                                                                `all))))
       (cond
        (company-ni-idl-idl-query-only
         (let ((idl-candidates (company-ni-idl-candidates arg)))
           (if idl-candidates
               idl-candidates
             dabbrev-candidates)))
        (company-ni-idl-merge-dabbrev
         (let ((idl-candidates (company-ni-idl-candidates arg)))
           (if idl-candidates
               (-concat
                idl-candidates
                ;; dabbrev-candidates minus the idl-candidates
                (-remove (lambda (aDabbrevEl)
                           (-any? (lambda (aIdlEl)
                                    (s-equals? aIdlEl aDabbrevEl))
                                  idl-candidates))
                         dabbrev-candidates)
               )
             dabbrev-candidates)
         )
        )
        (t
         dabbrev-candidates)
       )
     )
    )
    (annotation (get-text-property 0 'anno arg))
    (meta (get-text-property 0 'meta arg))
    (post-completion (message (get-text-property 0 'meta arg)))
    (doc-buffer
     (let ((docBuffer (get-text-property 0 'doc-buffer arg)))
       (cond
        ((s-present? docBuffer)
         ;; (message "DOCBUFFER: %s" docBuffer)
         ;; (message "DOC: %s" (agl-bash-cmd-to-string (concat "ni-idl-query doc " docBuffer))
         (let ((doc (agl-bash-cmd-to-string (concat "ni-idl-query doc " docBuffer))))
           (cond
            ((s-present? doc) (company-doc-buffer doc))
            (t nil)))
        )
        (t nil)
       )
     )
    )
    (duplicates nil) ;; duplicates are removed 'by hand'
    (ignore-case nil)
  )
)

(provide 'company-ni-idl)

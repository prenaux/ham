(require 'company)
(require 'company-dabbrev)
(require 's)

(ni-add-to-PATH-front (concat (getenv "WORK") "/niSDK/bin"))

(setq company-ni-idl-do-idl-query nil)
(setq company-ni-idl-query-command "cached_candidates")

(defun ni-idl-build-cache ()
  (interactive)
  (agl-bash-cmd-to-string (concat "ni-idl-query build_cache"))
  nil
)

(defun company-ni-idl-complete ()
  (interactive)
  (company-abort)
  (setq company-ni-idl-do-idl-query t)
  (let ((r (company-complete-common)))
    (setq company-ni-idl-do-idl-query nil)
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

(defun company-ni-idl (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-ni-idl))
    (prefix
     ;; (message "PREFIX:" arg)
     (cond
      (company-ni-idl-do-idl-query
       (company-ni-idl-grab-symbol))
      (t
       (company-grab-word))
     )
    )
    (candidates
     (cond
      (company-ni-idl-do-idl-query
       (company-ni-idl-candidates arg))
      (t
       (progn
         (let ((words (company-dabbrev--search (company-dabbrev--make-regexp arg)
                                               company-dabbrev-time-limit
                                               (pcase company-dabbrev-other-buffers
                                                 (`t (list major-mode))
                                                 (`all `all))))
               (downcase-p (if (eq company-dabbrev-downcase 'case-replace)
                               case-replace
                             company-dabbrev-downcase)))
           words))
      )
     )
    )
    (annotation (get-text-property 0 'anno arg))
    (meta (get-text-property 0 'meta arg))
    (post-completion (message (get-text-property 0 'meta arg)))
    ;; (doc-buffer (company-doc-buffer (get-text-property 0 'doc-buffer arg)))
    (duplicates (not company-ni-idl-do-idl-query))
    (ignore-case nil)
  )
)

(provide 'company-ni-idl)

(provide 'ni-ivy)

(add-to-list 'load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/ivy"))

(require 'counsel)
(require 'ivy)
(require 'wgrep)
(diminish 'ivy-mode)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
;; intentional space before end of string
(setq ivy-count-format "(%d/%d) ")
(setq ivy-initial-inputs-alist nil)

;; 1/3 of the window height for the minibuffer height
(setq ivy-height-alist
      '((t
         lambda (_caller)
         (/ (frame-height) 3))))

(global-set-key (kbd "M-x") #'counsel-M-x)

(defmacro ni-ivy-quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  `(progn
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    ,@body))
     (minibuffer-keyboard-quit)))

(defun ni-ivy-quit-and-counsel-find-file (&optional arg)
  "Cancel ivy and ."
  (interactive "P")
  (ni-ivy-quit-and-run
   (counsel-find-file)))

;; Ido behavior for Ivy. ido allows you to use C-j to view the current
;; directory. If you press RET on a directory, completion continues from that
;; directly. C-j (ivy-immediate-done) also directly opens whatever is in the
;; buffer without using the ivy suggestion so it can be used to create new
;; named buffers.
(define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

;; C-f always open counsel-find-file when in an ivy minibuffer, this is a
;; useful fallback
(define-key ivy-minibuffer-map (kbd "C-f") 'ni-ivy-quit-and-counsel-find-file)

;;;======================================================================
;;; Counsel
;;;======================================================================

;;
;; You can specify certain file types or pattern by prepending the search with -.
;; ex:
;;   rg: -g Make* -- install
;;   rg: --type cpp -- apCanvas
;;
;; To replace the matches found by counsel press M-q in the minibuffer.
;;
(defun ni-counsel-rg-at-point ()
  "Run `counsel-rg' with the text at point as the default search string."
  (interactive)
  (let ((search-string (thing-at-point 'symbol)))
    (counsel-rg search-string (ni-find-search-directory))))

(defun ni-counsel-rg-at-point-in-dir (regexp directory)
  "Run `counsel-rg' with the text at point as the default search string."
  (interactive (list (ni-find-read-regexp "Counsel search for: ")
                     (read-directory-name "Directory: " default-directory)))
    (counsel-rg regexp directory))

(defun ni-counsel-thing-at-point-or-read-string ()
  (let ((symbol (thing-at-point 'symbol)))
    (if (s-blank-str? symbol)
        (read-string "Enter string: ")
      symbol)))

(defun ni-counsel-rg-dumb-jump (&optional ctx-type)
  "Run `counsel-rg' with the text at point as the default search string."
  (interactive)
  (let* ( (minibuffer-prompt-end 0)
          (look-for (ni-counsel-thing-at-point-or-read-string))
          (lang (dumb-jump-get-language buffer-file-name))
          (rg-types (dumb-jump-get-rg-type-by-language lang))
          (search-string (concat "::" ctx-type "::" lang "::" look-for)))
    (counsel-rg
      search-string
      (ni-find-search-directory)
      (concat "-U --pcre2 "
        (mapconcat (lambda (s) (concat "--type " s " ")) rg-types " "))
      "rg-dj: "
      )
    ))

(provide 'ni-file-cache)
(require 'ni-base)
(require 'cl)

;;;======================================================================
;;; File Cache
;;;======================================================================
(NotBatchMode
 (agl-begin-time-block "File Cache")
 (require 'filecache)
 (require 'ido)

 (defvar my-file-cache-name "~/.emacs-filecache")

 ;; --- from Denis Bueno's .emacs - http://obfuscatedcode.wordpress.com/my-dot-emacs/ ---
 ;;
 ;; file cache management --- gives me something like "find resource" in
 ;; eclipse use `file-cache-add-directory-recursively' to set up the cache the
 ;; following functions I got from
 ;; http://www.emacswiki.org/cgi-bin/wiki/FileNameCache and they save the
 ;; cache to disk, since it's big, and doesn't change *that* often.
 (eval-after-load "filecache"
   '(progn
      (defun file-cache-save-my-cache-to-file ()
        "Save the current file-cache to `my-file-cache-name'."
        (interactive)
        (file-cache-save-cache-to-file my-file-cache-name))

      (defun file-cache-only-add-this-directory ()
        (interactive)
        (let ((dir-name (file-name-directory (buffer-file-name))))
          (message (concat "=== File Cache adding directory: " dir-name))
          (file-cache-add-directory-recursively
           (file-name-directory (buffer-file-name)))
          (message "=== File Cache saving...")
          (file-cache-save-my-cache-to-file)
          (message "=== File Cache done !")
       ))

      (defun file-cache-add-this-directory ()
        (interactive)
        (let ((dir-name (file-name-directory (buffer-file-name))))
          (message (concat "=== File Cache adding directory: " dir-name))
          (file-cache-add-directory-recursively
           (file-name-directory (buffer-file-name)))
          (message "=== File Cache saving...")
          (file-cache-save-my-cache-to-file)
          (message "=== File Cache done !")
        ))

      ;; Remove all non-existing files from the list
      (defun file-cache-clean ()
        (interactive)
        (message "=== File Cache cleaning...")
        (setq file-cache-alist (-file-cache-build-clean-list file-cache-alist))
        (message "=== File Cache saving...")
        (file-cache-save-my-cache-to-file)
        (message "=== File Cache cleaning done !")
      )

      (defun -file-cache-build-clean-list (aItems)
        (remove-if
         (lambda (U) (eq U nil))
         (loop for ITEM in aItems
               collect (let ((file (car ITEM))
                             (dirs (cdr ITEM)))
                         (let ((existingDirs
                                (agl-list-flatten (loop for d in dirs
                                                        collect
                                                        (let ((fileName (concat d file)))
                                                          (cond
                                                           ((file-exists-p fileName) d)
                                                           (t nil)))))))
                           (cond
                            (existingDirs (agl-list-flatten (list file existingDirs)))
                            (t nil)))))))

      (defun file-cache-save-cache-to-file (file)
        "Save contents of `file-cache-alist' to FILE.
         For later retrieval using `file-cache-read-cache-from-file'"
        (interactive
         (list (read-file-name "File: " my-file-cache-name)))
        (with-temp-file (expand-file-name file)
          (prin1 file-cache-alist (current-buffer))))

      (defun file-cache-read-cache-from-file (file)
        "Clear `file-cache-alist' and read cache from FILE.
         The file cache can be saved to a file using
         `file-cache-save-cache-to-file'."
        (interactive
         (list (read-file-name "File: " my-file-cache-name)))
        (file-cache-clear-cache)
        (let ((buf (find-file-noselect file)))
          (with-current-buffer buf
            (goto-char (point-min))
            (setq file-cache-alist (read buf))) ;clobber `file-cache-alist'
          (kill-buffer buf)))

      (defun file-cache-add-this-file ()
        (and buffer-file-name
             (file-exists-p buffer-file-name)
             (file-cache-add-file buffer-file-name)))

      ;; Add files to the cache as I edit them.
      (add-hook 'find-file-hook 'file-cache-add-this-file)
      (add-hook 'kill-buffer-hook 'file-cache-add-this-file)

      ;; ignore ._DAV directories in my file cache
      (add-to-list 'file-cache-filter-regexps "/[.]_DAV")
      ;; ignore hg directories in my file cache
      (add-to-list 'file-cache-filter-regexps "/[.]hg")
      ;; ignore git directories in my file cache
      (add-to-list 'file-cache-filter-regexps "/[.]git")
      ;; ignore _ni directories in my file cache
      (add-to-list 'file-cache-filter-regexps "/_ni")
      ;; ignore svn directories in my file cache
      (add-to-list 'file-cache-filter-regexps "/[.]svn")
      (add-to-list 'file-cache-filter-regexps "/_svn") ; on windows
      ;; ignore darcs directories in my file cache
      (add-to-list 'file-cache-filter-regexps "/_darcs")
      ;; ignore maven target directories
      (add-to-list 'file-cache-filter-regexps "/target")
      (add-to-list 'file-cache-filter-regexps "[.]annot$")
      ;; auto generate files
      (add-to-list 'file-cache-filter-regexps "/_gen_.*")
      ;; ignore node_modules
      (add-to-list 'file-cache-filter-regexps "/node_modules")
      (add-to-list 'file-cache-filter-regexps "/build/client")
      (add-to-list 'file-cache-filter-regexps "/build/server")

      ;; Ignore non-source files
      (dolist (pat (list
                    "[.]suo$"
                    "[.]arm$"
                    "[.]zip$"
                    "[.]exe$"
                    "[.]chm$"
                    "[.]jar$"
                    "[.]lib$"
                    "[.]pdb$"
                    "[.]a$"
                    "[.]aqo$"
                    "[.]hi$"
                    "[.]class$"
                    "[.]o$"
                    "[.]obj$"
                    "[.]so$"
                    "[.]dll$"
                    "[.]imagemap$"
                    "[.]cgo$"
                    "[.]a25$"
                    "[.]gpk$"
                    "[.]psd$"
                    "[.]PSD$"
                    "[.]bmp$"
                    "[.]BMP$"
                    "[.]tga$"
                    "[.]TGA$"
                    "[.]png$"
                    "[.]PNG$"
                    "[.]jpg$"
                    "[.]JPG$"
                    "[.]jpeg$"
                    "[.]JPEG$"
                    "[.]dds$"
                    "[.]DDS$"
                    "[.]scene$"
                    "[.]ach$"
                    "[.]swf$"
                    ))
        (add-to-list 'file-cache-filter-regexps pat))

      ;; set up the default cache
      (if (file-exists-p my-file-cache-name)
          (progn
            (message "=== File Cache reading...")
            (file-cache-read-cache-from-file my-file-cache-name)
            (message "=== File Cache reading done !")))))

 (defun ni-file-cache-reload ()
   (interactive)
   (file-cache-read-cache-from-file my-file-cache-name))

 (defun ni-file-cache-find-file (&optional aInitialFile)
   "Open a file from the file cache."
   (interactive)
   (let* (my-ivy-text
          (file
           (completing-read "Cached File: "
                                (mapcar 'car file-cache-alist)
                                nil nil aInitialFile)))
     (let ((record (assoc file file-cache-alist)))
       (find-file
        (expand-file-name
         file
         (if (= (length record) 2)
             (cadr record)
           (completing-read (format "Find %s in dir: " file)
                                (cdr record)
                                nil
                                t)))))))

 ;; Find file at point
 (defun ni-file-cache-find-file-at-point ()
   (interactive)
   (if (or
        (thing-at-point-looking-at "^[ \t]*include[ \t]+\"\\([^ \t\r\n\"]+\\)\"$")
        (thing-at-point-looking-at "^[ \t]*::Import(+\"\\([^ \t\r\n\"]+\\)\")$")
        (thing-at-point-looking-at "^[ \t]*::NewImport(+\\([^ \t\r\n]+\\))$")
        (thing-at-point-looking-at "^[ \t]*(require[ \t]+'\\([^ \t\r\n]+\\))$")
        (thing-at-point-looking-at "^[ \t]*#[ \t]*include[ \t]+[\"<]\\([^\">\r\n]+\\)\\([\">]\\|$\\)$"))
       (progn
         (ni-file-cache-find-file
          (file-name-nondirectory (buffer-substring-no-properties (match-beginning 1)
                                                                  (match-end 1)))))
     (ni-file-cache-find-file (ni-get-default-search-text))))

 ;; Save the cache when before we close emacs
 (add-hook 'kill-emacs-hook 'file-cache-save-my-cache-to-file)
)

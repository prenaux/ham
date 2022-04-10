;;; ham-grep.el --- A front-end for ham-grep
;;
;; Copyright (C) 2022 by Pierre Renaux
;;   Based of https://github.com/bling/pt.el
;;   Copyright (C) 2014 by Bailey
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Install:
;;   (require 'ham-grep)
;;
;; Usage:
;;   M-x ham-grep-regexp-search-dir
;;   M-x ham-grep-regexp-current-dir
;;   M-x ham-grep-work-regexp
;;
(eval-when-compile (require 'cl))
(require 'compile)
(require 'grep)
(require 'thingatpt)
(require 'dash)
(require 's)
(require 'dash)

(defcustom ham-grep-executable
  (concat "\"" HAM_HOME "/bin/ham-grep-for-emacs"  "\"")
  "Name of the ham-grep executable to use."
  :type 'string
  :group 'ham-grep)

(Windows
 (setq ham-grep-executable "ham-grep-for-emacs.cmd"))

;; The remote pt executable assume running on a Linux server with the same
;; user name & using the standard ~/Work/ham structure.
(defcustom ham-grep-remote-executable
  (concat "\"/home/" (getenv "USER") "/Work/ham/bin/ham-grep-for-emacs"  "\"")
  "Name of the executable to use when running through a remote/tramp connection."
  :type 'string
  :group 'ham-grep)

(defcustom ham-grep-arguments
  (list "")
  "Default arguments passed to ham-grep."
  :type '(repeat (string))
  :group 'ham-grep)

(defvar ham-grep-use-search-in-buffer-name t
  "If non-nil, use the search string in the ham-grep buffer's name.")

(defcustom ham-grep-work-subdirs
  (list "rules" "sources" "scripts")
  "Default list of sub directories that ham-grep-work-regexp looks for."
  :type '(repeat (string))
  :group 'ham-grep)

(defvar ham-grep-work-regexp-history-dirs nil
  "History for dirs of ham-grep-work-regexp.")

(define-compilation-mode ham-grep-search-mode "Ham-Grep"
  "Platinum searcher results compilation mode"
  (set (make-local-variable 'truncate-lines) t)
  (set (make-local-variable 'compilation-disable-input) t)
  (let ((symbol 'compilation-ham-grep)
        (pattern '("^\\([A-Za-z]:\\)?\\([^:\n]+?\\):\\([0-9]+\\):[^0-9]" 2 3)))
    (set (make-local-variable 'compilation-error-regexp-alist) (list symbol))
    (set (make-local-variable 'compilation-error-regexp-alist-alist) (list (cons symbol pattern))))
  (set (make-local-variable 'compilation-error-face) grep-hit-face))

(defun ham-grep-get-executable ()
  (if (file-remote-p default-directory)
    (if ham-grep-remote-executable
        ham-grep-remote-executable
      ham-grep-executable)
    ham-grep-executable))

(defun ham-grep-regexp--run (regexp directory &optional args)
  (let ((default-directory directory)
        (ham-grep-full-buffer-name (concat "*ham-grep-" regexp "*")))
    (compilation-start
     (mapconcat 'identity
                (append (list (ham-grep-get-executable))
                        ham-grep-arguments
                        args
                        '("--")
                        (list (shell-quote-argument regexp) ".")) " ")
     'ham-grep-search-mode

     (when ham-grep-use-search-in-buffer-name
       (function (lambda (ignore)
                   ham-grep-full-buffer-name)))

     (regexp-quote regexp))))

;;;###autoload
(defun ham-grep-regexp-search-dir (regexp directory &optional args)
  "Run a ham-grep search with REGEXP rooted at DIRECTORY."
  (interactive (list (ni-find-read-regexp "Ham-Grep search for: ")
                     (read-directory-name "Directory: " (ni-find-search-directory))))
  (ham-grep-regexp--run regexp directory args))

;;;###autoload
(defun ham-grep-regexp-current-dir (regexp directory &optional args)
  "Run a ham-grep search with REGEXP rooted at DIRECTORY."
  (interactive (list (ni-find-read-regexp "Ham-Grep search for: ")
                     (read-directory-name "Directory: " default-directory)))
  (ham-grep-regexp--run regexp directory args))

(defun ham-grep-work-get-dirs (dir)
  (-map
   (lambda (d) (concat "\"" d "\""))
   (-filter
    (lambda (d) (file-directory-p d))
    (-map
     (lambda (d)
       (cond
        ((string-prefix-p "/" dir) (concat dir "/" d))
        (t (concat (getenv "WORK") "/" dir "/" d))))
     ham-grep-work-subdirs)
    )))

;;;###autoload
(defun ham-grep-work-regexp (regexp dirs &optional args)
  "Run a ham-grep-work search with REGEXP rooted at the specified WORK directories."
  (interactive (list (ni-find-read-regexp "Ham-Grep search for: ")
                     (read-from-minibuffer "Dirs: " (-first-item ham-grep-work-regexp-history-dirs)
                                           nil nil 'ham-grep-work-regexp-history-dirs)))
  (let ((dir-args (-flatten
                   (-map (lambda (x) (ham-grep-work-get-dirs x))
                         (s-split " " dirs))))
        (ham-grep-full-buffer-name (concat "*ham-grep-work-" regexp "*")))
    (compilation-start
     (mapconcat 'identity
                (append (list (ham-grep-get-executable))
                        ham-grep-arguments
                        args
                        '("--")
                        (list (shell-quote-argument regexp))
                        dir-args) " ")
     'ham-grep-search-mode
     (when ham-grep-use-search-in-buffer-name
       (function (lambda (ignore)
                   ham-grep-full-buffer-name)))
     (regexp-quote regexp))))

(provide 'ham-grep)
;;; ham-grep.el ends here

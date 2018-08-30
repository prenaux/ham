;;; pt.el --- A front-end for pt, The Platinum Searcher.
;;
;; Copyright (C) 2014 by Bailey Ling
;; Author: Bailey Ling
;; URL: https://github.com/bling/pt.el
;; Filename: pt.el
;; Description: A front-end for pt, the Platinum Searcher
;; Created: 2014-04-27
;; Version: 0.0.3
;; Keywords: pt ack ag grep search
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
;;
;; Autoloads will be set up automatically if you use package.el.
;;
;; Usage:
;;
;; M-x pt-regexp
;; M-x pt-regexp-file-pattern
;; M-x projectile-pt
;;
;;; Code:

(eval-when-compile (require 'cl))
(require 'compile)
(require 'grep)
(require 'thingatpt)
(require 'dash)
(require 's)

(defcustom pt-executable
  "pt"
  "Name of the pt executable to use."
  :type 'string
  :group 'pt)

(defcustom pt-arguments
  (list "--smart-case" "-e")
  "Default arguments passed to pt."
  :type '(repeat (string))
  :group 'pt)

(defvar pt-use-search-in-buffer-name t
  "If non-nil, use the search string in the pt buffer's name.")

(define-compilation-mode pt-search-mode "Pt"
  "Platinum searcher results compilation mode"
  (set (make-local-variable 'truncate-lines) t)
  (set (make-local-variable 'compilation-disable-input) t)
  (let ((symbol 'compilation-pt)
        (pattern '("^\\([A-Za-z]:\\)?\\([^:\n]+?\\):\\([0-9]+\\):[^0-9]" 2 3)))
    (set (make-local-variable 'compilation-error-regexp-alist) (list symbol))
    (set (make-local-variable 'compilation-error-regexp-alist-alist) (list (cons symbol pattern))))
  (set (make-local-variable 'compilation-error-face) grep-hit-face))

;;;###autoload
(defun pt-regexp (regexp directory &optional args)
  "Run a pt search with REGEXP rooted at DIRECTORY."
  (interactive (list (read-from-minibuffer "Pt search for: " (thing-at-point 'symbol))
                     (read-directory-name "Directory: ")))
  (let ((default-directory directory)
        (pt-full-buffer-name (concat "*pt-" regexp "*")))
    (compilation-start
     (mapconcat 'identity
                (append (list pt-executable)
                        pt-arguments
                        args
                        '("--nogroup" "--nocolor" "--")
                        (list (shell-quote-argument regexp) ".")) " ")
     'pt-search-mode

     (when pt-use-search-in-buffer-name
       (function (lambda (ignore)
                   pt-full-buffer-name)))

     (regexp-quote regexp)

     )))

(defun pt-work-get-dirs (dir)
  (cond
   ((string-prefix-p "/" dir)
    (list
     (concat "\"" dir "/sources" "\"")
     (concat "\"" dir "/scripts" "\""))
   )
   (t
    (list
     (concat "\"" (getenv "WORK") "/" dir "/sources" "\"")
     (concat "\"" (getenv "WORK") "/" dir "/scripts" "\""))
   )
  )
)

(defvar pt-work-regexp-history-search nil
  "History for searches of pt-work-regexp.")

(defvar pt-work-regexp-history-dirs nil
  "History for dirs of pt-work-regexp.")

(defun pt-work-regexp (regexp dirs &optional args)
  "Run a pt-work search with REGEXP rooted at the specified WORK directories."
  (interactive (list (read-from-minibuffer "Pt search for: "
                                           (-first-item (-non-nil
                                                         (list (thing-at-point 'symbol)
                                                               (-first-item pt-work-regexp-history-search)))
                                           )
                                           nil nil 'pt-work-regexp-history-search
                     )
                     (read-from-minibuffer "Dirs: " (-first-item pt-work-regexp-history-dirs)
                                           nil nil 'pt-work-regexp-history-dirs
                     )
               )
  )
  (let (
        (dir-args (-flatten
                   (-map (lambda (x) (pt-work-get-dirs x))
                         (s-split " " dirs))))
        (pt-full-buffer-name (concat "*pt-work-" regexp "*"))
       )
    (compilation-start
     (mapconcat 'identity
                (append (list pt-executable)
                        pt-arguments
                        args
                        '("--nogroup" "--nocolor" "--")
                        (list (shell-quote-argument regexp))
                        dir-args) " ")
     'pt-search-mode

     (when pt-use-search-in-buffer-name
       (function (lambda (ignore)
                   pt-full-buffer-name)))

     (regexp-quote regexp)

    )))

;;;###autoload
(defun pt-regexp-file-pattern (regexp directory pattern)
  "Run a pt search with REGEXP rooted at DIRECTORY with FILE-FILTER."
  (interactive (list (read-from-minibuffer "Pt search for: " (thing-at-point 'symbol))
                     (read-directory-name "Directory: ")
                     (read-from-minibuffer "File pattern: ")))
  (pt-regexp regexp
             directory
             (list (concat "--file-search-regexp=" (shell-quote-argument pattern)))))

;;;###autoload
(defun projectile-pt (regexp)
  "Run a pt search with REGEXP rooted at the current projectile project root."
  (interactive (list (read-from-minibuffer "Pt search for: " (thing-at-point 'symbol))))
  (if (fboundp 'projectile-project-root)
      (pt-regexp regexp
                 (projectile-project-root)
                 (mapcar (lambda (val) (concat "--ignore=" val))
                         (append projectile-globally-ignored-files
                                 projectile-globally-ignored-directories)))
    (error "Projectile is not available")))

(provide 'pt)
;;; pt.el ends here

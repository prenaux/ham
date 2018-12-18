;;; aflymake-easy.el --- Helpers for easily building flymake checkers

;; Copyright (C) 2012 Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/aflymake-easy
;; Version: DEV
;; Keywords: convenience, internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides the `aflymake-easy-load' helper function for
;; setting up flymake checkers. Just call that function with the
;; appropriate arguments in a major mode hook function. See
;; `aflymake-ruby' for an example:
;; https://github.com/purcell/aflymake-ruby

;;; Code:

(require 'aflymake)

(defvar aflymake-easy--active nil
  "Indicates when aflymake-easy-load has successfully run in this buffer.")
(defvar aflymake-easy--command-fn nil
  "The user-specified function for building the flymake command.")
(defvar aflymake-easy--location nil
  "Where to create the temp file when checking, one of 'tempdir, 'inplace or
'temp-with-folder.")
(defvar aflymake-easy--extension nil
  "The canonical file name extension to use for the current file.")

(mapc 'make-variable-buffer-local
      '(aflymake-easy--active
        aflymake-easy--command-fn
        aflymake-easy--location
        aflymake-easy--extension))

(defun aflymake-easy--tempfile-in-temp-dir (file-name prefix)
  "Create a temporary file for storing the contents of FILE-NAME in the system tempdir.
Argument PREFIX temp file prefix, supplied by flymake."
  (make-temp-file (or prefix "aflymake-easy")
                  nil
                  (concat "." aflymake-easy--extension)))

(defun aflymake-easy--aflymake-init ()
  "A catch-all flymake init function for use in `aflymake-allowed-file-name-masks'."
  (let* ((tempfile
          (aflymake-init-create-temp-buffer-copy
           (cond
            ((eq 'tempdir aflymake-easy--location)
             'aflymake-easy--tempfile-in-temp-dir)
            ((eq 'inplace aflymake-easy--location)
             'aflymake-create-temp-inplace)
            ((eq 'temp-with-folder aflymake-easy--location)
             'aflymake-create-temp-with-folder-structure)
            (t
             (error "unknown location for aflymake-easy: %s" aflymake-easy--location)))))
         (command (funcall aflymake-easy--command-fn tempfile)))
    (list (car command) (cdr command))))

(defun aflymake-easy-exclude-buffer-p ()
  "Whether to skip flymake in the current buffer."
  (and (fboundp 'tramp-tramp-file-p)
       (buffer-file-name)
       (tramp-tramp-file-p (buffer-file-name))))

(defun aflymake-easy-load (command-fn &optional err-line-patterns location extension warning-re info-re)
  "Enable flymake in the containing buffer using a specific narrow configuration.
Argument COMMAND-FN function called to build the
   command line to run (receives filename, returns list).
Argument ERR-LINE-PATTERNS patterns for identifying errors (see `aflymake-err-line-patterns').
Argument EXTENSION a canonical extension for this type of source file, e.g. \"rb\".
Argument LOCATION where to create the temporary copy: one of 'tempdir (default), 'inplace or 'temp-with-folder
Argument WARNING-RE a pattern which identifies error messages as warnings.
Argument INFO-RE a pattern which identifies messages as infos (supported only
by the flymake fork at https://github.com/illusori/emacs-flymake)."
  (let ((executable (car (funcall command-fn "dummy"))))
    (if (executable-find executable) ;; TODO: defer this checking
        (unless (aflymake-easy-exclude-buffer-p)
          (setq aflymake-easy--command-fn command-fn
                aflymake-easy--location (or location 'tempdir)
                aflymake-easy--extension extension
                aflymake-easy--active t)
          (set (make-local-variable 'aflymake-allowed-file-name-masks)
               '(("." aflymake-easy--aflymake-init)))
          (when err-line-patterns
            (set (make-local-variable 'aflymake-err-line-patterns) err-line-patterns))
          (dolist (var '(aflymake-warning-re aflymake-warn-line-regexp))
            (set (make-local-variable var) (or warning-re "^[wW]arn")))
          (when (boundp 'aflymake-info-line-regexp)
            (set (make-local-variable 'aflymake-info-line-regexp)
                 (or info-re "^[iI]nfo")))
          (aflymake-mode t))
      (message "Not enabling flymake: '%s' program not found" executable))))

;; Internal overrides for flymake

(defun aflymake-easy--find-all-matches (str)
  "Return every match for `aflymake-err-line-patterns' in STR.

This is a judicious override for `aflymake-split-output', enabled
by the advice below, which allows for matching multi-line
patterns."
  (let (matches
        (last-match-end-pos 0))
    (dolist (pattern aflymake-err-line-patterns)
      (let ((regex (car pattern))
            (pos 0))
        (while (string-match regex str pos)
          (push (match-string 0 str) matches)
          (setq pos (match-end 0)))
        (setq last-match-end-pos (max pos last-match-end-pos))))
    (let ((residual (substring str last-match-end-pos)))
      (list matches
            (unless (string= "" residual) residual)))))

(defadvice aflymake-split-output (around aflymake-easy--split-output (output) activate protect)
  "Override `aflymake-split-output' to support mult-line error messages."
  (setq ad-return-value (if aflymake-easy--active
                            (aflymake-easy--find-all-matches output)
                          ad-do-it)))


(defadvice aflymake-post-syntax-check (before aflymake-easy--force-check-was-interrupted activate)
  (when aflymake-easy--active
    (setq aflymake-check-was-interrupted t)))


(provide 'aflymake-easy)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; aflymake-easy.el ends here

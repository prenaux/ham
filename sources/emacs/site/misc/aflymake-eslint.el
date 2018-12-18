;;; aflymake-eslint.el --- A flymake handler for javascript files
;;; linted with [eslint](eslint.org)
;;
;;; Author: Travis Jefferson
;;; URL: https://github.com/tjefferson08/aflymake-eslint
;;; Version: DEV
;;; Package-Requires: ((aflymake-easy "0.1"))
;;;
;;; Commentary:
;;  Usage:
;;   (require 'aflymake-eslint)
;;   (add-hook 'js-mode-hook 'aflymake-eslint-load)
;;
;; Uses aflymake-easy, from https://github.com/purcell/aflymake-easy
;;; Code:

(require 'aflymake-easy)

(defconst aflymake-eslint-err-line-patterns
  '(("^[ ]*\\([0-9]+\\):\\([0-9]+\\)[ ]*\\(.*\\)$" nil 1 2 3))) ;; default eslint reporter format

(defvar aflymake-eslint-executable "eslint"
  "The eslint executable to use for syntax checking.")

;; TODO add some options for eslint CLI
(defun aflymake-eslint-command (filename)
  "Construct a command that flymake can use to run eslint on a file."
  (list aflymake-eslint-executable filename))

;;;###autoload
(defun aflymake-eslint-load ()
  "Configure flymake mode to check the current buffer's Javascript syntax."
  (interactive)
  (aflymake-easy-load 'aflymake-eslint-command
                     aflymake-eslint-err-line-patterns

                     ;; this did not work with the 'tempdir, apparently eslint
                     ;; got lost and couldn't find its .eslintrc files
                     'inplace
                     "js"))

(provide 'aflymake-eslint)
;;; aflymake-eslint.el ends here

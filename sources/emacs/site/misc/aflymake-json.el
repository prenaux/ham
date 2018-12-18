;;; aflymake-json.el --- A flymake handler for json using jsonlint
;;
;;; Author: Steve Purcell <steve@sanityinc.com>
;;; Homepage: https://github.com/purcell/aflymake-json
;;; Version: DEV
;;; Package-Requires: ((aflymake-easy "0.1"))
;;
;;; Commentary:
;;
;; This package requires the "jsonlint" program, which can be installed using npm:
;;
;;    npm install jsonlint -g
;;
;; Usage:
;;
;;   (require 'aflymake-json)
;;
;; Then, if you're using `json-mode':
;;
;;   (add-hook 'json-mode 'aflymake-json-load)
;;
;; or, if you use `js-mode' for json:
;;
;;   (add-hook 'js-mode-hook 'aflymake-json-maybe-load)
;;
;; otherwise:
;;
;;   (add-hook 'find-file-hook 'aflymake-json-maybe-load)
;;
;; Uses aflymake-easy, from https://github.com/purcell/aflymake-easy

;;; Code:

(require 'aflymake-easy)

(defconst aflymake-json-err-line-patterns
  '(("^\\(.+\\)\: line \\([0-9]+\\), col \\([0-9]+\\), \\(.+\\)$" nil 2 3 4)))

(defvar aflymake-json-executable "jsonlint"
  "The jsonlint executable to use for syntax checking.")

(defun aflymake-json-command (filename)
  "Construct a command that flymake can use to check json source."
  (list aflymake-json-executable "-c" "-q" filename))

;;;###autoload
(defun aflymake-json-load ()
  "Configure flymake mode to check the current buffer's javascript syntax."
  (interactive)
  (aflymake-easy-load 'aflymake-json-command
                     aflymake-json-err-line-patterns
                     'tempdir
                     "json"))

;;;###autoload
(defun aflymake-json-maybe-load ()
  "Call `aflymake-json-load' if this file appears to be json."
  (interactive)
  (if (and buffer-file-name
           (string= "json" (file-name-extension buffer-file-name)))
      (aflymake-json-load)))


(provide 'aflymake-json)
;;; aflymake-json.el ends here

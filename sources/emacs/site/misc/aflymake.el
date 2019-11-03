;;; flymake.el --- a universal on-the-fly syntax checker

;; Copyright (C) 2003-2012 Free Software Foundation, Inc.

;; Author:  Pavel Kobyakov <pk_at_work@yahoo.com>
;; Maintainer: Sam Graham <libflymake-emacs BLAHBLAH illusori.co.uk>
;; Version: 0.4.16
;; Keywords: c languages tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Flymake is a minor Emacs mode performing on-the-fly syntax
;; checks using the external syntax check tool (for C/C++ this
;; is usually the compiler)

;;; Bugs/todo:

;; - Only uses "Makefile", not "makefile" or "GNUmakefile"
;;   (from http://bugs.debian.org/337339).

;;; Code:

(require 'cc-defs)
(eval-when-compile (require 'cl))
(eval-when-compile (require 'tramp))
(if (featurep 'xemacs) (require 'overlay))

(defvar aflymake-is-running nil
  "If t, aflymake syntax check process is running for the current buffer.")
(make-variable-buffer-local 'aflymake-is-running)

(defvar aflymake-buffers 0
  "Refcount of number of buffers in AFlymake mode, when zero the last-change timer is disabled.")

(defvar aflymake-timer nil
  "Timer for starting syntax check.")

(defvar aflymake-last-change-time nil
  "Time of last buffer change.")
(make-variable-buffer-local 'aflymake-last-change-time)

(defvar aflymake-check-start-time nil
  "Time at which syntax check was started.")
(make-variable-buffer-local 'aflymake-check-start-time)

(defvar aflymake-check-was-interrupted nil
  "Non-nil if syntax check was killed by `aflymake-compile'.")
(make-variable-buffer-local 'aflymake-check-was-interrupted)

(defvar aflymake-check-should-restart nil
  "Non-nil if syntax check should restart after terminating.")
(make-variable-buffer-local 'aflymake-check-should-restart)

(defvar aflymake-err-info nil
  "Sorted list of line numbers and lists of err info in the form (file, err-text).")
(make-variable-buffer-local 'aflymake-err-info)

(defvar aflymake-new-err-info nil
  "Same as `aflymake-err-info', effective when a syntax check is in progress.")
(make-variable-buffer-local 'aflymake-new-err-info)

(defcustom aflymake-start-syntax-check-on-find-file t
  "Start syntax check on find file."
  :group 'aflymake
  :type 'boolean)

(defcustom aflymake-run-in-place t
  "If nil, aflymake will run on copies in `temporary-file-directory' rather
than the same directory as the original file.

If the file makes use of relative include paths it's quite possible that
setting this to nil will cause compilation errors. On the other hand, leaving
it set to t will trigger any automated file creation detection that is
watching your project directory. YMMV.

When editing a remote file via Tramp, this flag also has the side-effect of
determining whether the syntax check is run in the same place as the original
file (and thus on the remote machine), or in the same place as
`temporary-file-directory' (usually the local machine)."
  :group 'aflymake
  :type 'boolean)

;;;###autoload
(define-minor-mode aflymake-mode
  "Toggle on-the-fly syntax checking.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  :group 'aflymake :lighter aflymake-mode-line
  (cond

    ;; Turning the mode ON.
    (aflymake-mode
      (cond
        ((not buffer-file-name)
         (message "AFlymake unable to run without a buffer file name"))
        ((not (aflymake-can-syntax-check-file buffer-file-name))
          (aflymake-log 2 "aflymake cannot check syntax in buffer %s" (buffer-name)))
        (t
          (setq aflymake-buffers (1+ aflymake-buffers))
          (add-hook 'after-change-functions 'aflymake-after-change-function nil t)
          (add-hook 'after-save-hook 'aflymake-after-save-hook nil t)
          (add-hook 'kill-buffer-hook 'aflymake-kill-buffer-hook nil t)
          ;;+(add-hook 'find-file-hook 'aflymake-find-file-hook)

          (aflymake-report-status "" "")

          (when (and (> aflymake-buffers 0)
                     (not aflymake-timer))
            (setq aflymake-timer (run-at-time nil 1 'aflymake-on-timer-event)))

          (when (and aflymake-start-syntax-check-on-find-file
                     ;; Since we write temp files in current dir, there's no point
                     ;; trying if the directory is read-only (bug#8954).
                     (or (not aflymake-run-in-place)
                         (file-writable-p (file-name-directory buffer-file-name)))
                     (file-readable-p (file-name-directory buffer-file-name)))
            (with-demoted-errors
              (aflymake-start-syntax-check))))))

    ;; Turning the mode OFF.
    (t
      (remove-hook 'after-change-functions 'aflymake-after-change-function t)
      (remove-hook 'after-save-hook 'aflymake-after-save-hook t)
      (remove-hook 'kill-buffer-hook 'aflymake-kill-buffer-hook t)
      ;;+(remove-hook 'find-file-hook (function aflymake-find-file-hook) t)

      (aflymake-delete-own-overlays)

      (setq aflymake-buffers (1- aflymake-buffers))

      (when (and (<= aflymake-buffers 0)
                 aflymake-timer)
        (cancel-timer aflymake-timer)
        (setq aflymake-timer nil))

      (setq aflymake-is-running nil))))

;;;; [[ cross-emacs compatibility routines
(defsubst aflymake-makehash (&optional test)
  (if (fboundp 'make-hash-table)
      (if test (make-hash-table :test test) (make-hash-table))
    (with-no-warnings
      (makehash test))))

(defalias 'aflymake-float-time
  (if (fboundp 'float-time)
      'float-time
    (if (featurep 'xemacs)
        (lambda ()
          (multiple-value-bind (s0 s1 s2) (values-list (current-time))
            (+ (* (float (ash 1 16)) s0) (float s1) (* 0.0000001 s2)))))))

(defalias 'aflymake-replace-regexp-in-string
  (if (eval-when-compile (fboundp 'replace-regexp-in-string))
      'replace-regexp-in-string
    (lambda (regexp rep str)
      (replace-in-string str regexp rep))))

(defalias 'aflymake-split-string
  (if (condition-case nil (equal (split-string " bc " " " t) '("bc"))
        (error nil))
      (lambda (str pattern) (split-string str pattern t))
    (lambda (str pattern)
      "Split STR into a list of substrings bounded by PATTERN.
Zero-length substrings at the beginning and end of the list are omitted."
      (let ((split (split-string str pattern)))
        (while (equal (car split) "") (setq split (cdr split)))
        (setq split (nreverse split))
        (while (equal (car split) "") (setq split (cdr split)))
        (nreverse split)))))

(defalias 'aflymake-get-temp-dir
  (if (fboundp 'temp-directory)
      'temp-directory
    (lambda () temporary-file-directory)))

(defun aflymake-posn-at-point-as-event (&optional position window dx dy)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of WINDOW, as a mouse-1 click
event (identical to the event that would be triggered by clicking
mouse button 1 at the top left corner of the glyph).

POSITION and WINDOW default to the position of point in the
selected window.

DX and DY specify optional offsets from the top left of the glyph."
  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  (unless dx (setq dx 0))
  (unless dy (setq dy 0))

  (let* ((pos (posn-at-point position window))
         (x-y (posn-x-y pos))
         (edges (window-inside-pixel-edges window))
         (win-x-y (window-pixel-edges window)))
    ;; adjust for window edges
    (setcar (nthcdr 2 pos)
            (cons (+ (car x-y) (car  edges) (- (car win-x-y))  dx)
                  (+ (cdr x-y) (cadr edges) (- (cadr win-x-y)) dy)))
    (list 'mouse-1 pos)))

(defun aflymake-popup-menu (menu-data)
  "Pop up the aflymake menu at point, using the data MENU-DATA.
POS is a list of the form ((X Y) WINDOW), where X and Y are
pixels positions from the top left corner of WINDOW's frame.
MENU-DATA is a list of error and warning messages returned by
`aflymake-make-err-menu-data'."
  (if (featurep 'xemacs)
      (let* ((pos         (aflymake-get-point-pixel-pos))
             (x-pos       (nth 0 pos))
             (y-pos       (nth 1 pos))
             (fake-event-props  '(button 1 x 1 y 1)))
        (setq fake-event-props (plist-put fake-event-props 'x x-pos))
        (setq fake-event-props (plist-put fake-event-props 'y y-pos))
        (popup-menu (aflymake-make-xemacs-menu menu-data)
                    (make-event 'button-press fake-event-props)))
    (x-popup-menu (if (eval-when-compile (fboundp 'posn-at-point))
                      (aflymake-posn-at-point-as-event)
                    (list (aflymake-get-point-pixel-pos) (selected-window)))
                  (aflymake-make-emacs-menu menu-data))))

(defun aflymake-make-emacs-menu (menu-data)
  "Return a menu specifier using MENU-DATA.
MENU-DATA is a list of error and warning messages returned by
`aflymake-make-err-menu-data'.
See `x-popup-menu' for the menu specifier format."
  (let* ((menu-title     (nth 0 menu-data))
         (menu-items     (nth 1 menu-data))
         (menu-commands  (mapcar (lambda (foo)
                                   (cons (nth 0 foo) (nth 1 foo)))
                                 menu-items)))
    (list menu-title (cons "" menu-commands))))

(if (featurep 'xemacs) (progn

(defun aflymake-nop ())

(defun aflymake-make-xemacs-menu (menu-data)
  "Return a menu specifier using MENU-DATA."
  (let* ((menu-title     (nth 0 menu-data))
         (menu-items     (nth 1 menu-data))
         (menu-commands  nil))
    (setq menu-commands (mapcar (lambda (foo)
                                  (vector (nth 0 foo) (or (nth 1 foo) '(aflymake-nop)) t))
                                menu-items))
    (cons menu-title menu-commands)))

)) ;; xemacs

(unless (eval-when-compile (fboundp 'posn-at-point))

(defun aflymake-current-row ()
  "Return current row number in current frame."
  (if (fboundp 'window-edges)
      (+ (car (cdr (window-edges))) (count-lines (window-start) (point)))
    (count-lines (window-start) (point))))

(defun aflymake-selected-frame ()
  (if (fboundp 'window-edges)
      (selected-frame)
    (selected-window)))

(defun aflymake-get-point-pixel-pos ()
  "Return point position in pixels: (x, y)."
  (let ((mouse-pos  (mouse-position))
        (pixel-pos  nil)
        (ret        nil))
    (if (car (cdr mouse-pos))
        (progn
          (set-mouse-position (aflymake-selected-frame) (current-column) (aflymake-current-row))
          (setq pixel-pos (mouse-pixel-position))
          (set-mouse-position (car mouse-pos) (car (cdr mouse-pos)) (cdr (cdr mouse-pos)))
          (setq ret (list (car (cdr pixel-pos)) (cdr (cdr pixel-pos)))))
      (progn
        (setq ret '(0 0))))
    (aflymake-log 3 "mouse pos is %s" ret)
    ret))

) ;; End of (unless (fboundp 'posn-at-point)

;;;; ]]

(defcustom aflymake-log-level -1
  "Logging level, only messages with level lower or equal will be logged.
-1 = NONE, 0 = ERROR, 1 = WARNING, 2 = INFO, 3 = DEBUG

   See `aflymake-log-file-name' if you want to control where the log is created."
  :group 'aflymake
  :type 'integer)

;; Yes, this is an awful default.
(defcustom aflymake-log-file-name "~/aflymake.log"
  "Where to put the aflymake log if logging is enabled.

   See `aflymake-log-level' if you want to control what is logged."
  :group 'aflymake
  :type 'string)

(defun aflymake-enquote-log-string (string)
  "Prepends > on each line of STRING."
  (concat "\n  > " (aflymake-replace-regexp-in-string "\n" "\n  > " string t t)))

(defun aflymake-log (level text &rest args)
  "Log a message at level LEVEL.
If LEVEL is higher than `aflymake-log-level', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format')."
  (if (<= level aflymake-log-level)
    (let* ((msg (apply 'format text args))
           ;; Surely there's a better way to do this?
           (time (current-time))
           (timestamp (concat (format-time-string "%Y-%m-%d %T" time)
                              "."
                              (format "%06d" (third time)))))
      (message "%s" msg)
      (make-directory (file-name-directory aflymake-log-file-name) 1)
      (write-region (concat "[" timestamp "] " msg "\n") nil aflymake-log-file-name t 566))))

(defun aflymake-ins-after (list pos val)
  "Insert VAL into LIST after position POS."
  (let ((tmp (copy-sequence list)))        ; (???)
    (setcdr (nthcdr pos tmp) (cons val (nthcdr (1+ pos) tmp)))
    tmp))

(defun aflymake-set-at (list pos val)
  "Set VAL at position POS in LIST."
  (let ((tmp (copy-sequence list)))        ; (???)
    (setcar (nthcdr pos tmp) val)
    tmp))

(defvar aflymake-processes nil
  "List of currently active aflymake processes.")

(defvar aflymake-output-residual nil)

(make-variable-buffer-local 'aflymake-output-residual)

(defgroup aflymake nil
  "A universal on-the-fly syntax checker."
  :version "23.1"
  :group 'tools)

(defcustom aflymake-allowed-file-name-masks
  '(("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" aflymake-simple-make-init)
    ("\\.xml\\'" aflymake-xml-init)
    ("\\.html?\\'" aflymake-xml-init)
    ("\\.cs\\'" aflymake-simple-make-init)
    ("\\.p[ml]\\'" aflymake-perl-init)
    ("\\.php[345]?\\'" aflymake-php-init)
    ("\\.js\\'" aflymake-javascript-init)
    ("\\.jsx\\'" aflymake-javascript-init)
    ("\\.css\\'" aflymake-css-init)
    ("\\.h\\'" aflymake-master-make-header-init aflymake-master-cleanup)
    ("\\.java\\'" aflymake-simple-make-java-init aflymake-simple-java-cleanup)
    ("[0-9]+\\.tex\\'" aflymake-master-tex-init aflymake-master-cleanup)
    ("\\.tex\\'" aflymake-simple-tex-init)
    ("\\.idl\\'" aflymake-simple-make-init)
    ("\\.spec\\'" aflymake-specfile-init)
    ("\\.po\\'" aflymake-pofile-init)
    ;; ("\\.cpp\\'" 1)
    ;; ("\\.java\\'" 3)
    ;; ("\\.h\\'" 2 ("\\.cpp\\'" "\\.c\\'")
    ;; ("[ \t]*#[ \t]*include[ \t]*\"\\([\w0-9/\\_\.]*[/\\]*\\)\\(%s\\)\"" 1 2))
    ;; ("\\.idl\\'" 1)
    ;; ("\\.odl\\'" 1)
    ;; ("[0-9]+\\.tex\\'" 2 ("\\.tex\\'")
    ;; ("[ \t]*\\input[ \t]*{\\(.*\\)\\(%s\\)}" 1 2 ))
    ;; ("\\.tex\\'" 1)
    )
  "Files syntax checking is allowed for."
  :group 'aflymake
  :type '(repeat (string symbol symbol symbol)))

(defun aflymake-get-file-name-mode-and-masks (file-name)
  "Return the corresponding entry from `aflymake-allowed-file-name-masks'."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (let ((fnm aflymake-allowed-file-name-masks)
        (mode-and-masks  nil))
    (while (and (not mode-and-masks) fnm)
      (if (string-match (car (car fnm)) file-name)
          (setq mode-and-masks (cdr (car fnm))))
      (setq fnm (cdr fnm)))
    (aflymake-log 3 "file %s, init=%s" file-name (car mode-and-masks))
    mode-and-masks))

(defun aflymake-can-syntax-check-file (file-name)
  "Determine whether we can syntax check FILE-NAME.
Return nil if we cannot, non-nil if we can."
  (if (aflymake-get-init-function file-name) t nil))

(defun aflymake-get-init-function (file-name)
  "Return init function to be used for the file."
  (let* ((init-f  (nth 0 (aflymake-get-file-name-mode-and-masks file-name))))
    ;;(aflymake-log 0 "calling %s" init-f)
    ;;(funcall init-f (current-buffer))
    init-f))

(defun aflymake-get-cleanup-function (file-name)
  "Return cleanup function to be used for the file."
  (or (nth 1 (aflymake-get-file-name-mode-and-masks file-name))
      'aflymake-simple-cleanup))

(defun aflymake-get-real-file-name-function (file-name)
  (or (nth 2 (aflymake-get-file-name-mode-and-masks file-name))
      'aflymake-get-real-file-name))

(defvar aflymake-find-buildfile-cache (aflymake-makehash 'equal))

(defun aflymake-get-buildfile-from-cache (dir-name)
  (gethash dir-name aflymake-find-buildfile-cache))

(defun aflymake-add-buildfile-to-cache (dir-name buildfile)
  (puthash dir-name buildfile aflymake-find-buildfile-cache))

(defun aflymake-clear-buildfile-cache ()
  (clrhash aflymake-find-buildfile-cache))

(defun aflymake-find-buildfile (buildfile-name source-dir-name)
  "Find buildfile starting from current directory.
Buildfile includes Makefile, build.xml etc.
Return its file name if found, or nil if not found."
  (or (aflymake-get-buildfile-from-cache source-dir-name)
      (let* ((file (locate-dominating-file source-dir-name buildfile-name)))
        (if file
            (let* ((file (file-truename file)))
              (aflymake-log 3 "found buildfile at %s" file)
              (aflymake-add-buildfile-to-cache source-dir-name file)
              file)
          (progn
            (aflymake-log 3 "buildfile for %s not found" source-dir-name)
            nil)))))

(defun aflymake-fix-file-name (name)
  "Replace all occurrences of '\' with '/'."
  (when name
    (setq name (expand-file-name name))
    (setq name (abbreviate-file-name name))
    (setq name (directory-file-name name))
    name))

(defun aflymake-same-files (file-name-one file-name-two)
  "Check if FILE-NAME-ONE and FILE-NAME-TWO point to same file.
Return t if so, nil if not."
  (equal (aflymake-fix-file-name file-name-one)
         (aflymake-fix-file-name file-name-two)))

(defcustom aflymake-master-file-dirs '("." "./src" "./UnitTest")
  "Dirs where to look for master files."
  :group 'aflymake
  :type '(repeat (string)))

(defcustom aflymake-master-file-count-limit 32
  "Max number of master files to check."
  :group 'aflymake
  :type 'integer)

;; This is bound dynamically to pass a parameter to a sort predicate below
(defvar aflymake-included-file-name)

(defun aflymake-find-possible-master-files (file-name master-file-dirs masks)
  "Find (by name and location) all possible master files.
Master files include .cpp and .c for .h.  Files are searched for
starting from the .h directory and max max-level parent dirs.
File contents are not checked."
  (let* ((dirs master-file-dirs)
         (files  nil)
         (done   nil))

    (while (and (not done) dirs)
      (let* ((dir (expand-file-name (car dirs) (file-name-directory file-name)))
             (masks masks))
        (while (and (file-exists-p dir) (not done) masks)
          (let* ((mask        (car masks))
                 (dir-files   (directory-files dir t mask)))

            (aflymake-log 3 "dir %s, %d file(s) for mask %s"
                         dir (length dir-files) mask)
            (while (and (not done) dir-files)
              (when (not (file-directory-p (car dir-files)))
                (setq files (cons (car dir-files) files))
                (when (>= (length files) aflymake-master-file-count-limit)
                  (aflymake-log 3 "master file count limit (%d) reached" aflymake-master-file-count-limit)
                  (setq done t)))
              (setq dir-files (cdr dir-files))))
          (setq masks (cdr masks))))
      (setq dirs (cdr dirs)))
    (when files
      (let ((aflymake-included-file-name (file-name-nondirectory file-name)))
        (setq files (sort files 'aflymake-master-file-compare))))
    (aflymake-log 3 "found %d possible master file(s)" (length files))
    files))

(defun aflymake-master-file-compare (file-one file-two)
  "Compare two files specified by FILE-ONE and FILE-TWO.
This function is used in sort to move most possible file names
to the beginning of the list (File.h -> File.cpp moved to top)."
  (and (equal (file-name-sans-extension aflymake-included-file-name)
              (file-name-sans-extension (file-name-nondirectory file-one)))
       (not (equal file-one file-two))))

(defcustom aflymake-check-file-limit 8192
  "Max number of chars to look at when checking possible master file.
Nil means search the entire file."
  :group 'aflymake
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Characters")))

(defun aflymake-check-patch-master-file-buffer
       (master-file-temp-buffer
        master-file-name patched-master-file-name
        source-file-name patched-source-file-name
        include-dirs regexp)
  "Check if MASTER-FILE-NAME is a master file for SOURCE-FILE-NAME.
If yes, patch a copy of MASTER-FILE-NAME to include PATCHED-SOURCE-FILE-NAME
instead of SOURCE-FILE-NAME.

For example, foo.cpp is a master file if it includes foo.h.

Whether a buffer for MATER-FILE-NAME exists, use it as a source
instead of reading master file from disk."
  (let* ((source-file-nondir    (file-name-nondirectory source-file-name))
         (source-file-extension (file-name-extension source-file-nondir))
         (source-file-nonext    (file-name-sans-extension source-file-nondir))
         (found                 nil)
         (inc-name              nil)
         (search-limit          aflymake-check-file-limit))
    (setq regexp
          (format regexp        ; "[ \t]*#[ \t]*include[ \t]*\"\\(.*%s\\)\""
                  ;; Hack for tex files, where \include often excludes .tex.
                  ;; Maybe this is safe generally.
                  (if (and (> (length source-file-extension) 1)
                           (string-equal source-file-extension "tex"))
                      (format "%s\\(?:\\.%s\\)?"
                              (regexp-quote source-file-nonext)
                              (regexp-quote source-file-extension))
                    (regexp-quote source-file-nondir))))
    (unwind-protect
        (with-current-buffer master-file-temp-buffer
          (when (or (not search-limit)
                    (> search-limit (point-max)))
            (setq search-limit (point-max)))
          (aflymake-log 3 "checking %s against regexp %s"
                       master-file-name regexp)
          (goto-char (point-min))
          (while (and (< (point) search-limit)
                      (re-search-forward regexp search-limit t))
            (let ((match-beg   (match-beginning 1))
                  (match-end   (match-end 1)))

              (aflymake-log 3 "found possible match for %s" source-file-nondir)
              (setq inc-name (match-string 1))
              (and (> (length source-file-extension) 1)
                   (string-equal source-file-extension "tex")
                   (not (string-match (format "\\.%s\\'" source-file-extension)
                                      inc-name))
                   (setq inc-name (concat inc-name "." source-file-extension)))
              (when (eq t (compare-strings
                           source-file-nondir nil nil
                           inc-name (- (length inc-name)
                                       (length source-file-nondir)) nil))
                (aflymake-log 3 "inc-name=%s" inc-name)
                (when (aflymake-check-include source-file-name inc-name
                                             include-dirs)
                  (setq found t)
                  ;;  replace-match is not used here as it fails in
                  ;; XEmacs with 'last match not a buffer' error as
                  ;; check-includes calls replace-in-string
                  (aflymake-replace-region
                   match-beg match-end
                   (file-name-nondirectory patched-source-file-name))))
              (forward-line 1)))
          (when found
            (aflymake-save-buffer-in-file patched-master-file-name)))
      ;;+(aflymake-log 3 "killing buffer %s"
      ;;                (buffer-name master-file-temp-buffer))
      (kill-buffer master-file-temp-buffer))
    ;;+(aflymake-log 3 "check-patch master file %s: %s" master-file-name found)
    (when found
      (aflymake-log 2 "found master file %s" master-file-name))
    found))

(defun aflymake-replace-region (beg end rep)
  "Replace text in BUFFER in region (BEG END) with REP."
  (save-excursion
    (goto-char end)
    ;; Insert before deleting, so as to better preserve markers's positions.
    (insert rep)
    (delete-region beg end)))

(defun aflymake-read-file-to-temp-buffer (file-name)
  "Insert contents of FILE-NAME into newly created temp buffer."
  (let* ((temp-buffer (get-buffer-create (generate-new-buffer-name (concat "aflymake:" (file-name-nondirectory file-name))))))
    (with-current-buffer temp-buffer
      (insert-file-contents file-name))
    temp-buffer))

(defun aflymake-copy-buffer-to-temp-buffer (buffer)
  "Copy contents of BUFFER into newly created temp buffer."
  (with-current-buffer
      (get-buffer-create (generate-new-buffer-name
                          (concat "aflymake:" (buffer-name buffer))))
    (insert-buffer-substring buffer)
    (current-buffer)))

(defun aflymake-check-include (source-file-name inc-name include-dirs)
  "Check if SOURCE-FILE-NAME can be found in include path.
Return t if it can be found via include path using INC-NAME."
  (if (file-name-absolute-p inc-name)
      (aflymake-same-files source-file-name inc-name)
    (while (and include-dirs
                (not (aflymake-same-files
                      source-file-name
                      (concat (file-name-directory source-file-name)
                              "/" (car include-dirs)
                              "/" inc-name))))
      (setq include-dirs (cdr include-dirs)))
    include-dirs))

(defun aflymake-find-buffer-for-file (file-name)
  "Check if there exists a buffer visiting FILE-NAME.
Return t if so, nil if not."
  (let ((buffer-name (get-file-buffer file-name)))
    (if buffer-name
        (get-buffer buffer-name))))

(defun aflymake-create-master-file (source-file-name patched-source-file-name get-incl-dirs-f create-temp-f masks include-regexp)
  "Save SOURCE-FILE-NAME with a different name.
Find master file, patch and save it."
  (let* ((possible-master-files     (aflymake-find-possible-master-files source-file-name aflymake-master-file-dirs masks))
         (master-file-count         (length possible-master-files))
         (idx                       0)
         (temp-buffer               nil)
         (master-file-name          nil)
         (patched-master-file-name  nil)
         (found                     nil))

    (while (and (not found) (< idx master-file-count))
      (setq master-file-name (nth idx possible-master-files))
      (setq patched-master-file-name (funcall create-temp-f master-file-name "aflymake_master"))
      (if (aflymake-find-buffer-for-file master-file-name)
          (setq temp-buffer (aflymake-copy-buffer-to-temp-buffer (aflymake-find-buffer-for-file master-file-name)))
        (setq temp-buffer (aflymake-read-file-to-temp-buffer master-file-name)))
      (setq found
            (aflymake-check-patch-master-file-buffer
             temp-buffer
             master-file-name
             patched-master-file-name
             source-file-name
             patched-source-file-name
             (funcall get-incl-dirs-f (file-name-directory master-file-name))
             include-regexp))
      (setq idx (1+ idx)))
    (if found
        (list master-file-name patched-master-file-name)
      (progn
        (aflymake-log 3 "none of %d master file(s) checked includes %s" master-file-count
                     (file-name-nondirectory source-file-name))
        nil))))

(defun aflymake-save-buffer-in-file (file-name)
  (make-directory (file-name-directory file-name) 1)
  (write-region nil nil file-name nil 566)
  (aflymake-log 3 "saved buffer %s in file %s" (buffer-name) file-name))

(defun aflymake-save-string-to-file (file-name data)
  "Save string DATA to file FILE-NAME."
  (write-region data nil file-name nil 566))

(defun aflymake-read-file-to-string (file-name)
  "Read contents of file FILE-NAME and return as a string."
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-substring (point-min) (point-max))))

(defun aflymake-process-filter (process output)
  "Parse OUTPUT and highlight error lines.
It's aflymake process filter."
  (let ((source-buffer (process-buffer process)))

    (aflymake-log 3 "received %d byte(s) of output from process %d"
                 (length output) (process-id process))
    (when (buffer-live-p source-buffer)
      (with-current-buffer source-buffer
        (aflymake-parse-output-and-residual output)))))

(defun aflymake-process-sentinel (process _event)
  "Sentinel for syntax check buffers."
  (when (memq (process-status process) '(signal exit))
    (let* ((exit-status       (process-exit-status process))
           (command           (process-command process))
           (source-buffer     (process-buffer process))
           (tramp-verbose     -1)
           (cleanup-f         (aflymake-get-cleanup-function (buffer-file-name source-buffer))))

      (aflymake-log 2 "process %d exited with code %d"
                   (process-id process) exit-status)
      (condition-case err
          (progn
            (aflymake-log 3 "cleaning up using %s" cleanup-f)
            (when (buffer-live-p source-buffer)
              (with-current-buffer source-buffer
                (funcall cleanup-f)))

            (delete-process process)
            (setq aflymake-processes (delq process aflymake-processes))

            (when (buffer-live-p source-buffer)
              (with-current-buffer source-buffer

                (aflymake-parse-residual)
                (aflymake-post-syntax-check exit-status command)
                (setq aflymake-is-running nil)
                (when aflymake-check-should-restart
                  (aflymake-log 2 "restarting syntax check")
                  (aflymake-start-syntax-check)))))
        (error
         (let ((err-str (format "Error in process sentinel for buffer %s: %s"
                                source-buffer (error-message-string err))))
           (aflymake-log 0 err-str)
           (with-current-buffer source-buffer
             (setq aflymake-is-running nil))))))
    (aflymake-run-next-queued-syntax-check)))

(defcustom aflymake-after-syntax-check-hook '()
  "Hook run each time AFlymake completes a syntax check and updates its list of errors."
  :type 'hook
  :group 'aflymake)

(defun aflymake-post-syntax-check (exit-status command)
  (setq aflymake-err-info aflymake-new-err-info)
  (setq aflymake-new-err-info nil)
  (setq aflymake-err-info
        (aflymake-fix-line-numbers
         aflymake-err-info 1 (aflymake-count-lines)))
  (aflymake-delete-own-overlays)
  (aflymake-highlight-err-lines aflymake-err-info)
  (let (err-count warn-count info-count)
    (setq err-count (aflymake-get-err-count aflymake-err-info "e"))
    (setq warn-count  (aflymake-get-err-count aflymake-err-info "w"))
    (setq info-count  (aflymake-get-err-count aflymake-err-info "i"))
    (aflymake-log 2 "%s: %d error(s), %d warning(s), %d info in %.2f second(s)"
                 (buffer-name) err-count warn-count info-count
                 (- (aflymake-float-time) aflymake-check-start-time))
    (setq aflymake-check-start-time nil)

    (if (and (equal 0 err-count) (equal 0 warn-count) (equal 0 info-count))
        (if (equal 0 exit-status)
            (aflymake-report-status "" "")        ; PASSED
          (if (not aflymake-check-was-interrupted)
              (aflymake-report-fatal-status "CFGERR"
                                           (format "Configuration error has occurred while running %s" command))
            (aflymake-report-status nil ""))) ; "STOPPED"
      (aflymake-report-status (format "%d/%d/%d" err-count warn-count info-count) "")))
  (run-hooks 'aflymake-after-syntax-check-hook))

(defun aflymake-parse-output-and-residual (output)
  "Split OUTPUT into lines, merge in residual if necessary."
  (aflymake-log 3 "received process output: %s" (aflymake-enquote-log-string output))
  (let* ((buffer-residual     aflymake-output-residual)
         (total-output        (if buffer-residual (concat buffer-residual output) output))
         (lines-and-residual  (aflymake-split-output total-output))
         (lines               (nth 0 lines-and-residual))
         (new-residual        (nth 1 lines-and-residual)))
    (setq aflymake-output-residual new-residual)
    (setq aflymake-new-err-info
          (aflymake-parse-err-lines
           aflymake-new-err-info lines))))

(defun aflymake-parse-residual ()
  "Parse residual if it's non empty."
  (when aflymake-output-residual
    (setq aflymake-new-err-info
          (aflymake-parse-err-lines
           aflymake-new-err-info
           (list aflymake-output-residual)))
    (setq aflymake-output-residual nil)))

(defun aflymake-er-make-er (line-no line-err-info-list)
  (list line-no line-err-info-list))

(defun aflymake-er-get-line (err-info)
  (nth 0 err-info))

(defun aflymake-er-get-line-err-info-list (err-info)
  (nth 1 err-info))

(defstruct (aflymake-ler
            (:constructor nil)
            (:constructor aflymake-ler-make-ler (file line type text &optional full-file)))
  file line type text full-file)

(defun aflymake-ler-set-file (line-err-info file)
  (aflymake-ler-make-ler file
                        (aflymake-ler-line line-err-info)
                        (aflymake-ler-type line-err-info)
                        (aflymake-ler-text line-err-info)
                        (aflymake-ler-full-file line-err-info)))

(defun aflymake-ler-set-full-file (line-err-info full-file)
  (aflymake-ler-make-ler (aflymake-ler-file line-err-info)
                        (aflymake-ler-line line-err-info)
                        (aflymake-ler-type line-err-info)
                        (aflymake-ler-text line-err-info)
                        full-file))

(defun aflymake-ler-set-line (line-err-info line)
  (aflymake-ler-make-ler (aflymake-ler-file line-err-info)
                        line
                        (aflymake-ler-type line-err-info)
                        (aflymake-ler-text line-err-info)
                        (aflymake-ler-full-file line-err-info)))

(defun aflymake-get-line-err-count (line-err-info-list type)
  "Return number of errors of specified TYPE.
Value of TYPE is either \"e\", \"w\" or \"i\"."
  (let* ((idx        0)
         (count      (length line-err-info-list))
         (err-count  0))

    (while (< idx count)
      (when (equal type (aflymake-ler-type (nth idx line-err-info-list)))
        (setq err-count (1+ err-count)))
      (setq idx (1+ idx)))
    err-count))

(defun aflymake-get-err-count (err-info-list type)
  "Return number of errors of specified TYPE for ERR-INFO-LIST."
  (let* ((idx        0)
         (count      (length err-info-list))
         (err-count  0))
    (while (< idx count)
      (setq err-count (+ err-count (aflymake-get-line-err-count (nth 1 (nth idx err-info-list)) type)))
      (setq idx (1+ idx)))
    err-count))

(defun aflymake-fix-line-numbers (err-info-list min-line max-line)
  "Replace line numbers with fixed value.
If line-numbers is less than MIN-LINE, set line numbers to MIN-LINE.
If line numbers is greater than MAX-LINE, set line numbers to MAX-LINE.
The reason for this fix is because some compilers might report
line number outside the file being compiled."
  (let* ((count     (length err-info-list))
         (err-info  nil)
         (line      0))
    (while (> count 0)
      (setq err-info (nth (1- count) err-info-list))
      (setq line (aflymake-er-get-line err-info))
      (when (or (< line min-line) (> line max-line))
        (setq line (if (< line min-line) min-line max-line))
        (setq err-info-list (aflymake-set-at err-info-list (1- count)
                                            (aflymake-er-make-er line
                                                                (aflymake-er-get-line-err-info-list err-info)))))
      (setq count (1- count))))
  err-info-list)

(defun aflymake-highlight-err-lines (err-info-list)
  "Highlight error lines in BUFFER using info from ERR-INFO-LIST."
  (save-excursion
    (dolist (err err-info-list)
      (aflymake-highlight-line (car err) (nth 1 err)))))

(defun aflymake-overlay-p (ov)
  "Determine whether overlay OV was created by aflymake."
  (and (overlayp ov) (overlay-get ov 'aflymake-overlay)))

(defun aflymake-make-overlay (beg end tooltip-text face mouse-face)
  "Allocate a aflymake overlay in range BEG and END."
  (when (not (aflymake-region-has-aflymake-overlays beg end))
    (let ((ov (make-overlay beg end nil t t)))
      (overlay-put ov 'face           face)
      (overlay-put ov 'mouse-face     mouse-face)
      (overlay-put ov 'help-echo      tooltip-text)
      (overlay-put ov 'aflymake-overlay  t)
      (overlay-put ov 'priority 100)
      ;;+(aflymake-log 3 "created overlay %s" ov)
      ov)
    (aflymake-log 3 "created an overlay at (%d-%d) with face %s" beg end face)))

(defun aflymake-delete-own-overlays ()
  "Delete all aflymake overlays in BUFFER."
  (dolist (ol (overlays-in (point-min) (point-max)))
    (when (aflymake-overlay-p ol)
      (delete-overlay ol)
      ;;+(aflymake-log 3 "deleted overlay %s" ol)
      )))

(defun aflymake-region-has-aflymake-overlays (beg end)
  "Check if region specified by BEG and END has overlay.
Return t if it has at least one aflymake overlay, nil if no overlay."
  (let ((ov                  (overlays-in beg end))
        (has-aflymake-overlays  nil))
    (while (consp ov)
      (when (aflymake-overlay-p (car ov))
        (setq has-aflymake-overlays t))
      (setq ov (cdr ov)))
    has-aflymake-overlays))

(defface aflymake-errline
  '((((class color) (background dark)) (:background "Firebrick4"))
    (((class color) (background light)) (:background "LightPink"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'aflymake)

(defface aflymake-warnline
  '((((class color) (background dark)) (:background "DarkBlue"))
    (((class color) (background light)) (:background "LightBlue2"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'aflymake)

(defface aflymake-infoline
  '((((class color) (background dark)) (:background "DarkGreen"))
    (((class color) (background light)) (:background "LightGreen"))
    (t (:bold t)))
  "Face used for marking info lines."
  :group 'aflymake)

(defcustom aflymake-number-of-errors-to-display 1
  "Number of aflymake errors to display in the tooltip if there are more than one.

If set to nil, all errors for the line will be displayed."
  :group 'aflymake
  :type '(choice integer (const nil)))

(defun aflymake-highlight-line (line-no line-err-info-list)
  "Highlight line LINE-NO in current buffer.
Perhaps use text from LINE-ERR-INFO-LIST to enhance highlighting."
  (goto-char (point-min))
  (forward-line (1- line-no))
  (when (and aflymake-number-of-errors-to-display
             (> (length line-err-info-list) aflymake-number-of-errors-to-display))
    (setq line-err-info-list (butlast line-err-info-list (- (length line-err-info-list) aflymake-number-of-errors-to-display))))
  (let* ((line-beg (point-at-bol))
         (line-end (point-at-eol))
         (beg      line-beg)
         (end      line-end)
         (tooltip-text (mapconcat 'aflymake-ler-text line-err-info-list "\n"))
         (face     nil))

    (goto-char line-beg)
    (while (looking-at "[ \t]")
      (forward-char))

    (setq beg (point))

    (goto-char line-end)
    (while (and (looking-at "[ \t\r\n]") (> (point) 1))
      (backward-char))

    (setq end (1+ (point)))

    (when (<= end beg)
      (setq beg line-beg)
      (setq end line-end))

    (when (= end beg)
      (goto-char end)
      (forward-line)
      (setq end (point)))

    (if (> (aflymake-get-line-err-count line-err-info-list "e") 0)
      (setq face 'aflymake-errline)
      (if (> (aflymake-get-line-err-count line-err-info-list "w") 0)
        (setq face 'aflymake-warnline)
        (setq face 'aflymake-infoline)))

    (aflymake-make-overlay beg end tooltip-text face nil)))

(defun aflymake-parse-err-lines (err-info-list lines)
  "Parse err LINES, store info in ERR-INFO-LIST."
  (let* ((count              (length lines))
         (idx                0)
         (line-err-info      nil)
         (real-file-name     nil)
         (source-file-name   buffer-file-name)
         (get-real-file-name-f (aflymake-get-real-file-name-function source-file-name)))

    (while (< idx count)
      (setq line-err-info (aflymake-parse-line (nth idx lines)))
      (when line-err-info
        (setq real-file-name (funcall get-real-file-name-f
                                      (aflymake-ler-file line-err-info)))
        (setq line-err-info (aflymake-ler-set-full-file line-err-info real-file-name))

        (when (aflymake-same-files real-file-name source-file-name)
          (setq line-err-info (aflymake-ler-set-file line-err-info nil))
          (setq err-info-list (aflymake-add-err-info err-info-list line-err-info))))
      (aflymake-log 3 "parsed '%s', %s line-err-info" (nth idx lines) (if line-err-info "got" "no"))
      (setq idx (1+ idx)))
    err-info-list))

(defun aflymake-split-output (output)
  "Split OUTPUT into lines.
Return last one as residual if it does not end with newline char.
Returns ((LINES) RESIDUAL)."
  (when (and output (> (length output) 0))
    (let* ((lines (aflymake-split-string output "[\n\r]+"))
           (complete (equal "\n" (char-to-string (aref output (1- (length output))))))
           (residual nil))
      (when (not complete)
        (setq residual (car (last lines)))
        (setq lines (butlast lines)))
      (list lines residual))))

(defun aflymake-reformat-err-line-patterns-from-compile-el (original-list)
  "Grab error line patterns from ORIGINAL-LIST in compile.el format.
Convert it to aflymake internal format."
  (let* ((converted-list '()))
    (dolist (item original-list)
      (setq item (cdr item))
      (let ((regexp (nth 0 item))
            (file (nth 1 item))
            (line (nth 2 item))
            (col (nth 3 item)))
        (if (consp file)        (setq file (car file)))
        (if (consp line)        (setq line (car line)))
        (if (consp col)        (setq col (car col)))

        (when (not (functionp line))
          (setq converted-list (cons (list regexp file line col) converted-list)))))
    converted-list))

(require 'compile)

(defvar aflymake-err-line-patterns ; regexp file-idx line-idx col-idx (optional) text-idx(optional), match-end to end of string is error text
  (append
   '(
     ;; MS Visual C++ 6.0
     ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \: \\(\\(error\\|warning\\|fatal error\\) \\(C[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
      1 3 nil 4)
     ;; jikes
     ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:[0-9]+\:[0-9]+\:[0-9]+\: \\(\\(Error\\|Warning\\|Caution\\|Semantic Error\\):[ \t\n]*\\(.+\\)\\)"
      1 3 nil 4)
     ;; MS midl
     ("midl[ ]*:[ ]*\\(command line error .*\\)"
      nil nil nil 1)
     ;; MS C#
     ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\),[0-9]+)\: \\(\\(error\\|warning\\|fatal error\\) \\(CS[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
      1 3 nil 4)
     ;; perl
     ("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)
     ;; PHP
     ("\\(?:Parse\\|Fatal\\) error: \\(.*\\) in \\(.*\\) on line \\([0-9]+\\)" 2 3 nil 1)
     ;; JSHint/CSSLint
     ("\\(.+\\): line \\([0-9]+\\), col \\([0-9]+\\), \\(.+\\)" 1 2 3 4)
     ;; LaTeX warnings (fileless) ("\\(LaTeX \\(Warning\\|Error\\): .*\\) on input line \\([0-9]+\\)" 20 3 nil 1)
     ;; gcc after 4.5 (includes column number)
     (" *\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:\\([0-9]+\\)\:[ \t\n]*\\(.+\\)"
      1 3 4 5)
     ;; ant/javac, also matches gcc prior to 4.5
     (" *\\(\\[javac\\] *\\)?\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:[ \t\n]*\\(.+\\)"
      2 4 nil 5))
   ;; compilation-error-regexp-alist)
   (aflymake-reformat-err-line-patterns-from-compile-el compilation-error-regexp-alist-alist))
  "Patterns for matching error/warning lines.  Each pattern has the form
\(REGEXP FILE-IDX LINE-IDX COL-IDX ERR-TEXT-IDX).
Use `aflymake-reformat-err-line-patterns-from-compile-el' to add patterns
from compile.el")

;;(defcustom aflymake-err-line-patterns
;;  '(
;;    ; MS Visual C++ 6.0
;;    ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \: \\(\\(error\\|warning\\|fatal error\\) \\(C[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
;;       1 3 4)
;;   ; jikes
;;   ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:[0-9]+\:[0-9]+\:[0-9]+\: \\(\\(Error\\|Warning\\|Caution\\):[ \t\n]*\\(.+\\)\\)"
;;       1 3 4))
;;    "patterns for matching error/warning lines, (regexp file-idx line-idx err-text-idx)"
;;   :group 'aflymake
;;   :type '(repeat (string number number number))
;;)

(defcustom aflymake-warn-line-regexp "^[wW]arning"
  "Regexp pattern for detecting if an error line is of class \"warning\" rather than \"error\"."
  :group 'aflymake
  :type 'string)

(defcustom aflymake-info-line-regexp "^[iI]nfo"
  "Regexp pattern for detecting if an error line is of class \"info\" rather than \"error\"."
  :group 'aflymake
  :type 'string)

(defun aflymake-parse-line (line)
  "Parse LINE to see if it is an error or warning.
Return its components if so, nil otherwise."
  (let ((raw-file-name nil)
        (line-no 0)
        (err-type "e")
        (err-text nil)
        (patterns aflymake-err-line-patterns)
        (matched nil))
    (while (and patterns (not matched))
      (when (string-match (car (car patterns)) line)
        (let* ((file-idx (nth 1 (car patterns)))
               (line-idx (nth 2 (car patterns))))

          (setq raw-file-name (if file-idx (match-string file-idx line) nil))
          (setq line-no       (if line-idx (string-to-number (match-string line-idx line)) 0))
          (setq err-text      (if (> (length (car patterns)) 4)
                                  (match-string (nth 4 (car patterns)) line)
                                (aflymake-patch-err-text (substring line (match-end 0)))))
          (or err-text (setq err-text "<no error text>"))
          (if (and err-text (string-match aflymake-warn-line-regexp err-text))
            (setq err-type "w"))
          (if (and err-text (string-match aflymake-info-line-regexp err-text))
            (setq err-type "i"))
          (aflymake-log 3 "parse line: type=%s file-idx=%s line-idx=%s file=%s line=%s text=%s"
            err-type file-idx line-idx raw-file-name line-no err-text)
          (setq matched t)))
      (setq patterns (cdr patterns)))
    (if matched
      (aflymake-ler-make-ler raw-file-name line-no err-type err-text)
      ())))

(defun aflymake-find-err-info (err-info-list line-no)
  "Find (line-err-info-list pos) for specified LINE-NO."
  (if err-info-list
      (let* ((line-err-info-list  nil)
             (pos       0)
             (count     (length err-info-list)))

        (while (and (< pos count) (< (car (nth pos err-info-list)) line-no))
          (setq pos (1+ pos)))
        (when (and (< pos count) (equal (car (nth pos err-info-list)) line-no))
          (setq line-err-info-list (aflymake-er-get-line-err-info-list (nth pos err-info-list))))
        (list line-err-info-list pos))
    '(nil 0)))

(defun aflymake-line-err-info-is-less-or-equal (line-one line-two)
  (or (string< (aflymake-ler-type line-one) (aflymake-ler-type line-two))
      (and (string= (aflymake-ler-type line-one) (aflymake-ler-type line-two))
           (not (aflymake-ler-file line-one)) (aflymake-ler-file line-two))
      (and (string= (aflymake-ler-type line-one) (aflymake-ler-type line-two))
           (or (and      (aflymake-ler-file line-one)       (aflymake-ler-file line-two))
               (and (not (aflymake-ler-file line-one)) (not (aflymake-ler-file line-two)))))))

(defun aflymake-add-line-err-info (line-err-info-list line-err-info)
  "Update LINE-ERR-INFO-LIST with the error LINE-ERR-INFO.
For the format of LINE-ERR-INFO, see `aflymake-ler-make-ler'.
The new element is inserted in the proper position, according to
the predicate `aflymake-line-err-info-is-less-or-equal'.
The updated value of LINE-ERR-INFO-LIST is returned."
  (if (not line-err-info-list)
      (list line-err-info)
    (let* ((count  (length line-err-info-list))
           (idx    0))
      (while (and (< idx count) (aflymake-line-err-info-is-less-or-equal (nth idx line-err-info-list) line-err-info))
        (setq idx (1+ idx)))
      (cond ((equal 0     idx)    (setq line-err-info-list (cons line-err-info line-err-info-list)))
            (t                    (setq line-err-info-list (aflymake-ins-after line-err-info-list (1- idx) line-err-info))))
      line-err-info-list)))

(defun aflymake-add-err-info (err-info-list line-err-info)
  "Update ERR-INFO-LIST with the error LINE-ERR-INFO, preserving sort order.
Returns the updated value of ERR-INFO-LIST.
For the format of ERR-INFO-LIST, see `aflymake-err-info'.
For the format of LINE-ERR-INFO, see `aflymake-ler-make-ler'."
  (let* ((line-no             (if (aflymake-ler-file line-err-info) 1 (aflymake-ler-line line-err-info)))
         (info-and-pos        (aflymake-find-err-info err-info-list line-no))
         (exists              (car info-and-pos))
         (pos                 (nth 1 info-and-pos))
         (line-err-info-list  nil)
         (err-info            nil))

    (if exists
        (setq line-err-info-list (aflymake-er-get-line-err-info-list (car (nthcdr pos err-info-list)))))
    (setq line-err-info-list (aflymake-add-line-err-info line-err-info-list line-err-info))

    (setq err-info (aflymake-er-make-er line-no line-err-info-list))
    (cond (exists             (setq err-info-list (aflymake-set-at err-info-list pos err-info)))
          ((equal 0 pos)      (setq err-info-list (cons err-info err-info-list)))
          (t                  (setq err-info-list (aflymake-ins-after err-info-list (1- pos) err-info))))
    err-info-list))

(defun aflymake-get-project-include-dirs-imp (basedir)
  "Include dirs for the project current file belongs to."
  (if (aflymake-get-project-include-dirs-from-cache basedir)
      (progn
        (aflymake-get-project-include-dirs-from-cache basedir))
    ;;else
    (let* ((command-line  (concat "make -C "
                                  (shell-quote-argument basedir)
                                  " DUMPVARS=INCLUDE_DIRS dumpvars"))
           (output        (shell-command-to-string command-line))
           (lines         (aflymake-split-string output "\n"))
           (count         (length lines))
           (idx           0)
           (inc-dirs      nil))
      (while (and (< idx count) (not (string-match "^INCLUDE_DIRS=.*" (nth idx lines))))
        (setq idx (1+ idx)))
      (when (< idx count)
        (let* ((inc-lines  (aflymake-split-string (nth idx lines) " *-I"))
               (inc-count  (length inc-lines)))
          (while (> inc-count 0)
            (when (not (string-match "^INCLUDE_DIRS=.*" (nth (1- inc-count) inc-lines)))
              (push (aflymake-replace-regexp-in-string "\"" "" (nth (1- inc-count) inc-lines)) inc-dirs))
            (setq inc-count (1- inc-count)))))
      (aflymake-add-project-include-dirs-to-cache basedir inc-dirs)
      inc-dirs)))

(defcustom aflymake-get-project-include-dirs-function 'aflymake-get-project-include-dirs-imp
  "Function used to get project include dirs, one parameter: basedir name."
  :group 'aflymake
  :type 'function)

(defun aflymake-get-project-include-dirs (basedir)
  (funcall aflymake-get-project-include-dirs-function basedir))

(defun aflymake-get-system-include-dirs ()
  "System include dirs - from the 'INCLUDE' env setting."
  (let* ((includes (getenv "INCLUDE")))
    (if includes (aflymake-split-string includes path-separator) nil)))

(defvar aflymake-project-include-dirs-cache (aflymake-makehash 'equal))

(defun aflymake-get-project-include-dirs-from-cache (base-dir)
  (gethash base-dir aflymake-project-include-dirs-cache))

(defun aflymake-add-project-include-dirs-to-cache (base-dir include-dirs)
  (puthash base-dir include-dirs aflymake-project-include-dirs-cache))

(defun aflymake-clear-project-include-dirs-cache ()
  (clrhash aflymake-project-include-dirs-cache))

(defun aflymake-get-include-dirs (base-dir)
  "Get dirs to use when resolving local file names."
  (let* ((include-dirs (append '(".") (aflymake-get-project-include-dirs base-dir) (aflymake-get-system-include-dirs))))
    include-dirs))

;; (defun aflymake-restore-formatting ()
;;   "Remove any formatting made by aflymake."
;;   )

;; (defun aflymake-get-program-dir (buffer)
;;   "Get dir to start program in."
;;   (unless (bufferp buffer)
;;     (error "Invalid buffer"))
;;   (with-current-buffer buffer
;;     default-directory))

(defun aflymake-safe-delete-file (file-name)
  (when (and file-name (file-exists-p file-name))
    (delete-file file-name)
    (aflymake-log 1 "deleted file %s" file-name)))

(defun aflymake-safe-delete-directory (dir-name)
  (condition-case nil
    (progn
      (delete-directory dir-name)
      (aflymake-log 1 "deleted dir %s" dir-name))
    (error
     (aflymake-log 1 "Failed to delete dir %s, error ignored" dir-name))))

(defcustom aflymake-compilation-prevents-syntax-check t
  "If non-nil, don't start syntax check if compilation is running."
  :group 'aflymake
  :type 'boolean)

(defcustom aflymake-max-parallel-syntax-checks 4
  "If non-nil, the maximum number of syntax checks to run in parallel before queuing."
  :group 'aflymake
  :type 'integer)

(defvar aflymake-syntax-check-queue ()
  "Queue of pending buffers to run aflymake on if aflymake-max-parallel-syntax-checks is exceeded.")

(defun aflymake-ready-for-next-syntax-check ()
  "Returns t if aflymake is running less than aflymake-max-parallel-syntax-checks checks, nil otherwise."
  (or (not aflymake-max-parallel-syntax-checks)
      (< (length aflymake-processes) aflymake-max-parallel-syntax-checks)))

(defun aflymake-queue-syntax-check (buffer)
  "Queue a syntax check on BUFFER to run later once number of parallel runs is low enough."
  (aflymake-log 3 "aflymake syntax check queued for buffer: %s" buffer)
  ;; For responsiveness to current activity we run as a LIFO stack rather than FIFO pipe
  (setq aflymake-syntax-check-queue (delete buffer aflymake-syntax-check-queue))
  (push buffer aflymake-syntax-check-queue)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (aflymake-report-status nil ":Queued")))
  (aflymake-log 3 "aflymake syntax check queue is now: %s" aflymake-syntax-check-queue))

(defun aflymake-remove-queued-syntax-check (buffer)
  "Remove a syntax check for BUFFER from the queue."
  (aflymake-log 3 "aflymake syntax check removed from queue for buffer: %s" buffer)
  (setq aflymake-syntax-check-queue (delete buffer aflymake-syntax-check-queue))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (aflymake-report-status nil nil)))
  (aflymake-log 3 "aflymake syntax check queue is now: %s" aflymake-syntax-check-queue))

(defun aflymake-pop-next-live-buffer-in-queue ()
  "Pop the next still-existing buffer from the queue of pending syntax checks."
  (while (and aflymake-syntax-check-queue
           (not (buffer-live-p (car aflymake-syntax-check-queue))))
    (pop aflymake-syntax-check-queue))
  (let ((buffer (pop aflymake-syntax-check-queue)))
    (aflymake-log 3 "aflymake syntax check popped for buffer: %s" buffer)
    (aflymake-log 3 "aflymake syntax check queue is now: %s" aflymake-syntax-check-queue)
    buffer))

(defun aflymake-run-next-queued-syntax-check ()
  "Run the next queued syntax check to run later once number of parallel runs is low enough."
  (interactive)
  (when (aflymake-ready-for-next-syntax-check)
    (let ((buffer (aflymake-pop-next-live-buffer-in-queue)))
      (when buffer
        (with-current-buffer buffer
          (aflymake-start-syntax-check))))))

(defun aflymake-start-syntax-check ()
  "Start syntax checking for current buffer. Once the syntax checking is
complete the `aflymake-after-syntax-check-hook' hook will be run."
  (interactive)
  (aflymake-log 3 "aflymake is running: %s" aflymake-is-running)
  (when (and (not aflymake-is-running)
             (aflymake-can-syntax-check-file buffer-file-name))
    (if (aflymake-ready-for-next-syntax-check)
      (when (or (not aflymake-compilation-prevents-syntax-check)
                (not (aflymake-compilation-is-running))) ;+ (aflymake-rep-ort-status buffer "COMP")
        (aflymake-clear-buildfile-cache)
        (aflymake-clear-project-include-dirs-cache)

        (setq aflymake-check-was-interrupted nil)
        (setq aflymake-check-should-restart nil)

        (let* ((source-file-name  buffer-file-name)
               (init-f (aflymake-get-init-function source-file-name))
               (cleanup-f (aflymake-get-cleanup-function source-file-name))
               (tramp-verbose -1)
               (cmd-and-args (funcall init-f))
               (cmd          (nth 0 cmd-and-args))
               (args         (nth 1 cmd-and-args))
               (dir          (nth 2 cmd-and-args)))
          (if (not cmd-and-args)
            (progn
              (aflymake-log 0 "init function %s for %s failed, cleaning up" init-f source-file-name)
              (funcall cleanup-f))
            (progn
              (setq aflymake-last-change-time nil)
              (aflymake-start-syntax-check-process cmd args dir)))))
      (aflymake-queue-syntax-check (current-buffer)))))

(defun aflymake-syntax-check-directory (dir)
  "Try to determine least-broken directory to use as the working directory
to run the syntax check command from.

If DIR is supplied from the init function for the file type then that will
be used.

Otherwise if `aflymake-run-in-place' is nil and `default-directory' appears
to be a tramp file, we use `temporary-file-directory' as a least-worst
compromise to ensure the tempoary aflymake copy of the buffer is on the same
machine as where the syntax check command is being run.

Otherwise we fall through to using `default-directory'."
  (or dir (if (and (not aflymake-run-in-place)
                   (featurep 'tramp)
                   tramp-mode
                   (tramp-tramp-file-p default-directory))
              temporary-file-directory
              default-directory)))

(defun aflymake-start-syntax-check-process (cmd args dir)
  "Start syntax check process."
    (condition-case err
      ;; Attempt to preserve the buffer against output leaking before the
      ;; process-filter is set up, this can occur when the process is
      ;; started over a remote tramp connection.
      ;; I'm fairly certain this is a bug in `tramp-handle-start-file-process'
      ;; but I'm damned if I can figure out what, so this is a workaround.
      (c-save-buffer-state ()
        (save-excursion
          (save-restriction
            (narrow-to-region (point-max) (point-max))
            ;; For some reason this insert is needed to prevent
            ;; tramp-mode from inserting any MOTD on the remote
            ;; machine outside the narrowed region.
            ;; With this insert it puts the MOTD in the narrowed
            ;; region and we can delete it safely.
            ;; Beats me, and I'm not entirely comfortable with it.
            (insert "\n")
            (let* ((tramp-verbose -1)
                   (process
                      (let ((default-directory (aflymake-syntax-check-directory dir)))
                        (aflymake-log 3 "starting process on dir %s" default-directory)
                        (apply 'start-file-process "aflymake-proc" (current-buffer) cmd args))))
              (set-process-query-on-exit-flag process nil)
              (set-process-sentinel process 'aflymake-process-sentinel)
              (set-process-filter process 'aflymake-process-filter)
              (push process aflymake-processes)
              ;; Clean up any output that has leaked. Fixes issues with Tramp.
              ;;(aflymake-log 3 "buffer-string %s" (aflymake-enquote-log-string (buffer-string)))
              (delete-region (point-min) (point-max))

              (setq aflymake-is-running t)
              (setq aflymake-last-change-time nil)
              (setq aflymake-check-start-time (aflymake-float-time))

              (aflymake-report-status nil "*")
              (aflymake-log 2 "started process %d, command=%s"
                (process-id process) (process-command process))
              process))))
      (error
       (let* ((err-str (format "Failed to launch syntax check process '%s' with args %s: %s"
                               cmd args (error-message-string err)))
              (source-file-name buffer-file-name)
              (cleanup-f        (aflymake-get-cleanup-function source-file-name))
              (tramp-verbose -1))
         (aflymake-log 0 err-str)
         (funcall cleanup-f)
         (aflymake-report-fatal-status "PROCERR" err-str)))))

(defun aflymake-kill-process (proc)
  "Kill process PROC."
  (kill-process proc)
  (let* ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq aflymake-check-was-interrupted t))))
  (aflymake-log 1 "killed process %d" (process-id proc)))

(defun aflymake-stop-syntax-check (&optional buffer)
  "Kill any queued or running syntax check for BUFFER.
Defaults to `current-buffer' if not supplied.

NOTE: Stopping a syntax check will not complete until the spawned process has
been terminated, this happens asynchronously and it is highly likely that
immediately after calling `aflymake-stop-syntax-check' the process will still
be running. Among other things, this will prevent starting a new syntax check
in the buffer until the process terminates. If you want to queue up a new
syntax check, you should look at `aflymake-restart-syntax-check' which handles
this async delay correctly for you."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (aflymake-remove-queued-syntax-check buffer)
    (dolist (proc aflymake-processes)
      (if (equal (process-buffer proc) buffer) (aflymake-kill-process proc)))))

(defun aflymake-stop-all-syntax-checks ()
  "Kill all syntax check processes."
  (interactive)
  (while aflymake-syntax-check-queue
    (aflymake-remove-queued-syntax-check (car aflymake-syntax-check-queue)))
  (while aflymake-processes
    (aflymake-kill-process (pop aflymake-processes))))

(defun aflymake-restart-syntax-check (&optional buffer)
  "Kill any queued or running syntax check for BUFFER and start a new one.
Defaults to `current-buffer' if not supplied."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (aflymake-remove-queued-syntax-check buffer)
    (dolist (proc aflymake-processes)
      (when (equal (process-buffer proc) buffer)
        (with-current-buffer buffer
          (setq aflymake-check-should-restart t))
        (aflymake-kill-process proc)))))

(defun aflymake-compilation-is-running ()
  (and (boundp 'compilation-in-progress)
       compilation-in-progress))

(defun aflymake-compile ()
  "Kill all aflymake syntax checks, start compilation."
  (interactive)
  (aflymake-stop-all-syntax-checks)
  (call-interactively 'compile))

(defcustom aflymake-no-changes-timeout 0.5
  "Time to wait after last change before starting compilation."
  :group 'aflymake
  :type 'number)

(defun aflymake-on-timer-event ()
  "Start a syntax check for current buffer if necessary."
  (when (and aflymake-mode
             (not aflymake-is-running)
             aflymake-last-change-time
             (> (- (aflymake-float-time) aflymake-last-change-time)
                aflymake-no-changes-timeout))

    (setq aflymake-last-change-time nil)
    (aflymake-log 3 "starting syntax check as more than %f second(s) passed since last change"
      aflymake-no-changes-timeout)
    (aflymake-start-syntax-check)))

(defun aflymake-current-line-no ()
  "Return number of current line in current buffer."
  (count-lines (point-min) (if (eobp) (point) (1+ (point)))))

(defun aflymake-count-lines ()
  "Return number of lines in buffer BUFFER."
  (count-lines (point-min) (point-max)))

(defun aflymake-display-err-menu-for-current-line ()
  "Display a menu with errors/warnings for current line if it has errors and/or warnings."
  (interactive)
  (let* ((line-no             (aflymake-current-line-no))
         (line-err-info-list  (nth 0 (aflymake-find-err-info aflymake-err-info line-no)))
         (menu-data           (aflymake-make-err-menu-data line-no line-err-info-list))
         (choice              nil))
    (if menu-data
        (progn
          (setq choice (aflymake-popup-menu menu-data))
          (aflymake-log 3 "choice=%s" choice)
          (when choice
            (eval choice)))
      (aflymake-log 1 "no errors for line %d" line-no))))

(defun aflymake-make-err-menu-data (line-no line-err-info-list)
  "Make a (menu-title (item-title item-action)*) list with errors/warnings from LINE-ERR-INFO-LIST."
  (let* ((menu-items  nil))
    (when line-err-info-list
      (let* ((count           (length line-err-info-list))
             (menu-item-text  nil))
        (while (> count 0)
          (setq menu-item-text (aflymake-ler-text (nth (1- count) line-err-info-list)))
          (let* ((file       (aflymake-ler-file (nth (1- count) line-err-info-list)))
                 (full-file  (aflymake-ler-full-file (nth (1- count) line-err-info-list)))
                 (line       (aflymake-ler-line (nth (1- count) line-err-info-list))))
            (if file
                (setq menu-item-text (concat menu-item-text " - " file "(" (format "%d" line) ")")))
            (setq menu-items (cons (list menu-item-text
                                         (if file (list 'aflymake-goto-file-and-line full-file line) nil))
                                   menu-items)))
          (setq count (1- count)))
        (aflymake-log 3 "created menu-items with %d item(s)" (length menu-items))))
    (if menu-items
        (let* ((menu-title  (format "Line %d: %d error(s), %d warning(s), %d info" line-no
                                    (aflymake-get-line-err-count line-err-info-list "e")
                                    (aflymake-get-line-err-count line-err-info-list "w")
                                    (aflymake-get-line-err-count line-err-info-list "i"))))
          (list menu-title menu-items))
      nil)))

(defun aflymake-goto-file-and-line (file line)
  "Try to get buffer for FILE and goto line LINE in it."
  (if (not (file-exists-p file))
      (aflymake-log 1 "File %s does not exist" file)
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

;; aflymake minor mode declarations
(defvar aflymake-mode-line nil)

(make-variable-buffer-local 'aflymake-mode-line)

(defvar aflymake-mode-line-e-w nil)

(make-variable-buffer-local 'aflymake-mode-line-e-w)

(defvar aflymake-mode-line-status nil)

(make-variable-buffer-local 'aflymake-mode-line-status)

(defun aflymake-report-status (e-w &optional status)
  "Show status in mode line."
  (when e-w
    (setq aflymake-mode-line-e-w e-w))
  (when status
    (setq aflymake-mode-line-status status))
  (let* ((mode-line " AFlymake"))
    (when (> (length aflymake-mode-line-e-w) 0)
      (setq mode-line (concat mode-line ":" aflymake-mode-line-e-w)))
    (setq mode-line (concat mode-line aflymake-mode-line-status))
    (setq aflymake-mode-line mode-line)
    (force-mode-line-update)))

(defun aflymake-display-warning (warning)
  "Display a warning to user."
  (message-box warning))

(defcustom aflymake-gui-warnings-enabled t
  "Enables/disables GUI warnings."
  :group 'aflymake
  :type 'boolean)

(defun aflymake-report-fatal-status (status warning)
  "Display a warning and switch aflymake mode off."
  (when aflymake-gui-warnings-enabled
    (aflymake-display-warning (format "AFlymake: %s. AFlymake will be switched OFF" warning)))
  (aflymake-mode 0)
  (aflymake-log 0 "switched OFF AFlymake mode for buffer %s due to fatal status %s, warning %s"
               (buffer-name) status warning))

;;;###autoload
(defun aflymake-mode-on ()
  "Turn aflymake mode on."
  (aflymake-mode 1)
  (aflymake-log 1 "aflymake mode turned ON for buffer %s" (buffer-name)))

;;;###autoload
(defun aflymake-mode-off ()
  "Turn aflymake mode off."
  (aflymake-mode 0)
  (aflymake-log 1 "aflymake mode turned OFF for buffer %s" (buffer-name)))

(defcustom aflymake-start-syntax-check-on-newline t
  "Start syntax check if newline char was added/removed from the buffer."
  :group 'aflymake
  :type 'boolean)

(defun aflymake-after-change-function (start stop _len)
  "Start syntax check for current buffer if it isn't already running."
  ;;+(aflymake-log 0 "setting change time to %s" (aflymake-float-time))
  (let((new-text (buffer-substring start stop)))
    (when (and aflymake-start-syntax-check-on-newline (equal new-text "\n"))
      (aflymake-log 3 "starting syntax check as new-line has been seen")
      (aflymake-start-syntax-check))
    (setq aflymake-last-change-time (aflymake-float-time))))

(defun aflymake-after-save-hook ()
  (if (local-variable-p 'aflymake-mode (current-buffer))        ; (???) other way to determine whether aflymake is active in buffer being saved?
      (progn
        (aflymake-log 3 "starting syntax check as buffer was saved")
        (aflymake-start-syntax-check)))) ; no more mode 3. cannot start check if mode 3 (to temp copies) is active - (???)

(defun aflymake-kill-buffer-hook ()
  (when aflymake-timer
    (cancel-timer aflymake-timer)
    (setq aflymake-timer nil)))

;;;###autoload
(defun aflymake-find-file-hook ()
  ;;+(when aflymake-start-syntax-check-on-find-file
  ;;+    (aflymake-log 3 "starting syntax check on file open")
  ;;+    (aflymake-start-syntax-check)
  ;;+)
  (when (and (not (local-variable-p 'aflymake-mode (current-buffer)))
             (aflymake-can-syntax-check-file buffer-file-name))
    (aflymake-mode)
    (aflymake-log 3 "automatically turned ON aflymake mode")))

(defun aflymake-get-first-err-line-no (err-info-list)
  "Return first line with error."
  (when err-info-list
    (aflymake-er-get-line (car err-info-list))))

(defun aflymake-get-last-err-line-no (err-info-list)
  "Return last line with error."
  (when err-info-list
    (aflymake-er-get-line (nth (1- (length err-info-list)) err-info-list))))

(defun aflymake-get-next-err-line-no (err-info-list line-no)
  "Return next line with error."
  (when err-info-list
    (let* ((count  (length err-info-list))
           (idx    0))
      (while (and (< idx count) (>= line-no (aflymake-er-get-line (nth idx err-info-list))))
        (setq idx (1+ idx)))
      (if (< idx count)
          (aflymake-er-get-line (nth idx err-info-list))))))

(defun aflymake-get-prev-err-line-no (err-info-list line-no)
  "Return previous line with error."
  (when err-info-list
    (let* ((count (length err-info-list)))
      (while (and (> count 0) (<= line-no (aflymake-er-get-line (nth (1- count) err-info-list))))
        (setq count (1- count)))
      (if (> count 0)
          (aflymake-er-get-line (nth (1- count) err-info-list))))))

(defun aflymake-skip-whitespace ()
  "Move forward until non-whitespace is reached."
  (while (looking-at "[ \t]")
    (forward-char)))

(defcustom aflymake-goto-error-hook '()
  "Hook run each time `aflymake-goto-next-error' and `aflymake-goto-prev-error'
are called."
  :type 'hook
  :group 'aflymake)

(defcustom aflymake-goto-error-skip-whitespace t
  "Whether to skip leading whitespace on the line when using
`aflymake-goto-next-error' and `aflymake-goto-prev-error'."
  :type 'boolean
  :group 'aflymake)

(defun aflymake-goto-line (line-no)
  "Go to line LINE-NO.
If `aflymake-goto-error-skip-whitespace' is t also move past leading whitespace
on the line.
The hook `aflymake-goto-error-hook' is run after moving to the new position."
  (goto-char (point-min))
  (forward-line (1- line-no))
  (if aflymake-goto-error-skip-whitespace
    (aflymake-skip-whitespace))
  (run-hooks 'aflymake-goto-error-hook))

(defun aflymake-goto-next-error ()
  "Go to next error in error ring.
The hook `aflymake-goto-error-hook' is run after moving to the new position."
  (interactive)
  (let ((line-no (aflymake-get-next-err-line-no aflymake-err-info (aflymake-current-line-no))))
    (when (not line-no)
      (setq line-no (aflymake-get-first-err-line-no aflymake-err-info))
      (aflymake-log 1 "passed end of file"))
    (if line-no
        (aflymake-goto-line line-no)
      (aflymake-log 1 "no errors in current buffer"))))

(defun aflymake-goto-prev-error ()
  "Go to previous error in errror ring.
The hook `aflymake-goto-error-hook' is run after moving to the new position."
  (interactive)
  (let ((line-no (aflymake-get-prev-err-line-no aflymake-err-info (aflymake-current-line-no))))
    (when (not line-no)
      (setq line-no (aflymake-get-last-err-line-no aflymake-err-info))
      (aflymake-log 1 "passed beginning of file"))
    (if line-no
        (aflymake-goto-line line-no)
      (aflymake-log 1 "no errors in current buffer"))))

(defun aflymake-patch-err-text (string)
  (if (string-match "^[\n\t :0-9]*\\(.*\\)$" string)
      (match-string 1 string)
    string))

;;;; general init-cleanup and helper routines
;; TODO: rename these to something sane and deprecate the current names.
(defun aflymake-create-temp-copy (file-name prefix)
  "Make filename for a temporary copy of FILE-NAME.

If `aflymake-run-in-place' is true it will use `aflymake-create-temp-inplace',
otherwise it will use `aflymake-create-temp-intemp'.

Note that this function, despite its name, does not actually create a
copy of the file: it only choses and returns a filename for the temp
copy."
  (if aflymake-run-in-place
    (aflymake-create-temp-inplace file-name prefix)
    (aflymake-create-temp-intemp file-name prefix)))

(defun aflymake-create-temp-inplace (file-name prefix)
    "Return filename in the same directory as FILE-NAME for a
temporary copy of the buffer editing FILE-NAME.

Despite the name of the argument, PREFIX will be appended to the
filename as a suffix to ensure we don't overwrite the original.
This usually defaults to \"aflymake\".

Note that this function, despite its name, does not actually create a
copy of the file: it only choses and returns a filename for the temp
copy."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "aflymake"))
  (let* ((temp-name (file-truename (concat (file-name-sans-extension file-name)
                                           "_" prefix
                                           (and (file-name-extension file-name)
                                                (concat "." (file-name-extension file-name)))))))
    (aflymake-log 3 "create-temp-inplace: file=%s temp=%s" file-name temp-name)
    temp-name))

;; This was lifted from various blogs, I'm not sure who the original
;; author was - whoever it was: thank you!
;; I got it from http://blog.urth.org/2011/06/aflymake-versus-the-catalyst-restarter.html
;; but Dave Rolsky indicates he got it from elsewhere.
(defun aflymake-create-temp-intemp (file-name prefix)
  "Return filename in temporary directory for a temporary
copy of the buffer editing FILE-NAME. This is a replacement for
`aflymake-create-temp-inplace'. The difference is that it gives
a file name in `temporary-file-directory' instead of the same
directory as FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not work if the file you are checking depends
relative paths to other files \(for the type of checks aflymake
makes).

Note that this function, despite its name, does not actually create a
copy of the file: it only choses and returns a filename for the temp
copy."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
    (setq prefix "aflymake"))
  (let* ((name (concat
                 (file-name-nondirectory (file-name-sans-extension file-name))
                 "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (file-truename (make-temp-file name nil ext))))
    (aflymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
      temp-name))

(defun aflymake-create-temp-with-folder-structure (file-name prefix)
  (unless (stringp file-name)
    (error "Invalid file-name"))

  (let* ((dir       (file-name-directory file-name))
         ;; Not sure what this slash-pos is all about, but I guess it's just
         ;; trying to remove the leading / of absolute file names.
         (slash-pos (string-match "/" dir))
         (temp-dir  (expand-file-name (substring dir (1+ slash-pos))
                                      (aflymake-get-temp-dir))))

    (file-truename (expand-file-name (file-name-nondirectory file-name)
                                     temp-dir))))

(defun aflymake-delete-temp-directory (dir-name)
  "Attempt to delete temp dir created by `aflymake-create-temp-with-folder-structure', do not fail on error."
  (let* ((dir-name (file-truename dir-name))
         (temp-dir (file-truename (aflymake-get-temp-dir)))
         (suffix   (substring dir-name (length temp-dir))))

    (while (> (length suffix) 0)
      (setq suffix (directory-file-name suffix))
      ;;(aflymake-log 0 "suffix=%s" suffix)
      ;;(aflymake-log 0 "delete=%s" (file-truename (expand-file-name suffix temp-dir)))
      (aflymake-safe-delete-directory
        (file-truename (expand-file-name suffix temp-dir)))
      (setq suffix (file-name-directory suffix)))))

(defvar aflymake-temp-source-file-name nil)
(make-variable-buffer-local 'aflymake-temp-source-file-name)

(defvar aflymake-master-file-name nil)
(make-variable-buffer-local 'aflymake-master-file-name)

(defvar aflymake-temp-master-file-name nil)
(make-variable-buffer-local 'aflymake-temp-master-file-name)

(defvar aflymake-base-dir nil)
(make-variable-buffer-local 'aflymake-base-dir)

(defun aflymake-init-create-temp-buffer-copy (create-temp-f)
  "Make a temporary copy of the current buffer, save its name in buffer data and return the name."
  (let*  ((source-file-name       buffer-file-name)
          (temp-source-file-name  (funcall create-temp-f source-file-name "aflymake")))

    (aflymake-save-buffer-in-file temp-source-file-name)
    (setq aflymake-temp-source-file-name temp-source-file-name)
    temp-source-file-name))

(defun aflymake-simple-cleanup ()
  "Do cleanup after `aflymake-init-create-temp-buffer-copy'.
Delete temp file."
  (aflymake-safe-delete-file aflymake-temp-source-file-name)
  (setq aflymake-last-change-time nil))

(defun aflymake-get-real-file-name (file-name-from-err-msg)
  "Translate file name from error message to \"real\" file name.
Return full-name.  Names are real, not patched."
  (let* ((real-name                nil)
         (source-file-name        buffer-file-name)
         (master-file-name        aflymake-master-file-name)
         (temp-source-file-name        aflymake-temp-source-file-name)
         (temp-master-file-name        aflymake-temp-master-file-name)
         (base-dirs
          (list aflymake-base-dir
                (file-name-directory source-file-name)
                (if master-file-name (file-name-directory master-file-name))))
         (files (list (list source-file-name       source-file-name)
                      (list temp-source-file-name  source-file-name)
                      (list master-file-name       master-file-name)
                      (list temp-master-file-name  master-file-name))))

    (when (equal 0 (length file-name-from-err-msg))
      (setq file-name-from-err-msg source-file-name))

    (setq real-name (aflymake-get-full-patched-file-name file-name-from-err-msg base-dirs files))
    ;; if real-name is nil, than file name from err msg is none of the files we've patched
    (if (not real-name)
        (setq real-name (aflymake-get-full-nonpatched-file-name file-name-from-err-msg base-dirs)))
    (if (not real-name)
        (setq real-name file-name-from-err-msg))
    (setq real-name (aflymake-fix-file-name real-name))
    (aflymake-log 3 "get-real-file-name: file-name=%s real-name=%s" file-name-from-err-msg real-name)
    real-name))

(defun aflymake-get-full-patched-file-name (file-name-from-err-msg base-dirs files)
  (let* ((base-dirs-count  (length base-dirs))
         (file-count       (length files))
         (real-name        nil))

    (while (and (not real-name) (> base-dirs-count 0))
      (setq file-count (length files))
      (while (and (not real-name) (> file-count 0))
        (let* ((this-dir        (nth (1- base-dirs-count) base-dirs))
               (this-file       (nth 0 (nth (1- file-count) files)))
               (this-real-name  (nth 1 (nth (1- file-count) files))))
          ;;+(aflymake-log 0 "this-dir=%s this-file=%s this-real=%s msg-file=%s" this-dir this-file this-real-name file-name-from-err-msg)
          (when (and this-dir this-file (aflymake-same-files
                                         (expand-file-name file-name-from-err-msg this-dir)
                                         this-file))
            (setq real-name this-real-name)))
        (setq file-count (1- file-count)))
      (setq base-dirs-count (1- base-dirs-count)))
    real-name))

(defun aflymake-get-full-nonpatched-file-name (file-name-from-err-msg base-dirs)
  (let* ((real-name  nil))
    (if (file-name-absolute-p file-name-from-err-msg)
        (setq real-name file-name-from-err-msg)
      (let* ((base-dirs-count  (length base-dirs)))
        (while (and (not real-name) (> base-dirs-count 0))
          (let* ((full-name (expand-file-name file-name-from-err-msg
                                              (nth (1- base-dirs-count) base-dirs))))
            (if (file-exists-p full-name)
                (setq real-name full-name))
            (setq base-dirs-count (1- base-dirs-count))))))
    real-name))

(defun aflymake-init-find-buildfile-dir (source-file-name buildfile-name)
  "Find buildfile, store its dir in buffer data and return its dir, if found."
  (let* ((buildfile-dir
          (aflymake-find-buildfile buildfile-name
                                  (file-name-directory source-file-name))))
    (if buildfile-dir
        (setq aflymake-base-dir buildfile-dir)
      (aflymake-log 1 "no buildfile (%s) for %s" buildfile-name source-file-name)
      (aflymake-report-fatal-status
       "NOMK" (format "No buildfile (%s) found for %s"
                      buildfile-name source-file-name)))))

(defun aflymake-init-create-temp-source-and-master-buffer-copy (get-incl-dirs-f create-temp-f master-file-masks include-regexp)
  "Find master file (or buffer), create its copy along with a copy of the source file."
  (let* ((source-file-name       buffer-file-name)
         (temp-source-file-name  (aflymake-init-create-temp-buffer-copy create-temp-f))
         (master-and-temp-master (aflymake-create-master-file
                                  source-file-name temp-source-file-name
                                  get-incl-dirs-f create-temp-f
                                  master-file-masks include-regexp)))

    (if (not master-and-temp-master)
        (progn
          (aflymake-log 1 "cannot find master file for %s" source-file-name)
          (aflymake-report-status "!" "")        ; NOMASTER
          nil)
      (setq aflymake-master-file-name (nth 0 master-and-temp-master))
      (setq aflymake-temp-master-file-name (nth 1 master-and-temp-master)))))

(defun aflymake-master-cleanup ()
  (aflymake-simple-cleanup)
  (aflymake-safe-delete-file aflymake-temp-master-file-name))

;;;; make-specific init-cleanup routines
(defun aflymake-get-syntax-check-program-args (source-file-name base-dir use-relative-base-dir use-relative-source get-cmd-line-f)
  "Create a command line for syntax check using GET-CMD-LINE-F."
  (funcall get-cmd-line-f
           (if use-relative-source
               (file-relative-name source-file-name base-dir)
             source-file-name)
           (if use-relative-base-dir
               (file-relative-name base-dir
                                   (file-name-directory source-file-name))
             base-dir)))

(defun aflymake-get-make-cmdline (source base-dir)
  (list "make"
        (list "-s"
              "-C"
              base-dir
              (concat "CHK_SOURCES=" (shell-quote-argument source))
              "SYNTAX_CHECK_MODE=1"
              "check-syntax")))

(defun aflymake-get-ant-cmdline (source base-dir)
  (list "ant"
        (list "-buildfile"
              (concat base-dir "/" "build.xml")
              (concat "-DCHK_SOURCES=" (shell-quote-argument source))
              "check-syntax")))

(defun aflymake-simple-make-init-impl (create-temp-f use-relative-base-dir use-relative-source build-file-name get-cmdline-f)
  "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
  (let* ((args nil)
         (source-file-name   buffer-file-name)
         (buildfile-dir      (aflymake-init-find-buildfile-dir source-file-name build-file-name)))
    (if buildfile-dir
        (let* ((temp-source-file-name  (aflymake-init-create-temp-buffer-copy create-temp-f)))
          (setq args (aflymake-get-syntax-check-program-args temp-source-file-name buildfile-dir
                                                            use-relative-base-dir use-relative-source
                                                            get-cmdline-f))))
    args))

(defun aflymake-simple-make-init ()
  (aflymake-simple-make-init-impl 'aflymake-create-temp-copy
    aflymake-run-in-place t "Makefile" 'aflymake-get-make-cmdline))

(defun aflymake-master-make-init (get-incl-dirs-f master-file-masks include-regexp)
  "Create make command line for a source file checked via master file compilation."
  (let* ((make-args nil)
         (temp-master-file-name (aflymake-init-create-temp-source-and-master-buffer-copy
                                 get-incl-dirs-f 'aflymake-create-temp-copy
                                 master-file-masks include-regexp)))
    (when temp-master-file-name
      (let* ((buildfile-dir (aflymake-init-find-buildfile-dir temp-master-file-name "Makefile")))
        (if  buildfile-dir
            (setq make-args (aflymake-get-syntax-check-program-args
                             temp-master-file-name buildfile-dir nil nil 'aflymake-get-make-cmdline)))))
    make-args))

(defun aflymake-find-make-buildfile (source-dir)
  (aflymake-find-buildfile "Makefile" source-dir))

;;;; .h/make specific
(defun aflymake-master-make-header-init ()
  (aflymake-master-make-init
   'aflymake-get-include-dirs
   '("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'")
   "[ \t]*#[ \t]*include[ \t]*\"\\([[:word:]0-9/\\_.]*%s\\)\""))

;;;; .java/make specific
(defun aflymake-simple-make-java-init ()
  (aflymake-simple-make-init-impl 'aflymake-create-temp-with-folder-structure nil nil "Makefile" 'aflymake-get-make-cmdline))

(defun aflymake-simple-ant-java-init ()
  (aflymake-simple-make-init-impl 'aflymake-create-temp-with-folder-structure nil nil "build.xml" 'aflymake-get-ant-cmdline))

(defun aflymake-simple-java-cleanup ()
  "Cleanup after `aflymake-simple-make-java-init' -- delete temp file and dirs."
  (aflymake-safe-delete-file aflymake-temp-source-file-name)
  (when aflymake-temp-source-file-name
    (aflymake-delete-temp-directory
     (file-name-directory aflymake-temp-source-file-name))))

;;;; perl-specific init-cleanup routines
(defun aflymake-perlbrew-path-sync ()
  "Sync $PATH in the environment of the current Emacs process with modifications
made by perlbrew in other shell processes.

Detection of perlbrew relies on $PERLBREW_ROOT being set in the environment, if
you're using a GUI-launched Emacs such as Emacs.app then you may need to take
steps to set this manually."
  (when (getenv "PERLBREW_ROOT")
    ;; This is pretty ugly, we need to run a bash intepreter, source
    ;; the perlbrew path hacking, then echo the modified path.
    ;; The antics with PATH=... is to try to make sure we don't trash
    ;; $PATH in the event that the command errors for some reason - don't
    ;; want our path set to the error message.
    (let ((raw-modified-path (shell-command-to-string (concat "bash -c '. " (getenv "PERLBREW_ROOT") "/etc/bashrc;echo \"PATH=$PATH\"'"))))
      (string-match "PATH=\\(.+\\)$" raw-modified-path)
      (let ((modified-path (match-string 1 raw-modified-path)))
        (if modified-path
          (progn
            (aflymake-log 2 "Updating $PATH to match perlbrew $PATH: \"%s\"" modified-path)
            (setenv "PATH" modified-path))
          (aflymake-log 1 "Unable to parse perlbrew $PATH output \"%s\"" raw-modified-path))))))

(defcustom aflymake-perl-lib-dir nil
  "Path to override AFlymake's attempt to find the Perl include dir
for a project with `aflymake-find-perl-lib-dir`."
  :group 'aflymake
  :type 'string)

(defun aflymake-find-perl-lib-dir (source-dir)
  "Look for a directory to push onto the Perl include directories with
the -I option.

Looks up the directory tree from the source file for a directory containing
either a \"Makefile.PL\" or a \"Build.PL\" file, or a directory named \"lib\",
then uses the \"lib\" subdirectory of that project directory.

Alternatively you may override this behaviour by customizing the
`aflymake-perl-lib-dir` variable to give the exact directory name you
wish to have supplied to Perl -I."
  (or
    (if aflymake-perl-lib-dir (expand-file-name aflymake-perl-lib-dir))
    (let ((project-root-dir
            (or (aflymake-find-buildfile "Makefile.PL" source-dir)
                (aflymake-find-buildfile "Build.PL" source-dir)
                (aflymake-find-buildfile "lib" source-dir))))
      (when project-root-dir
        (expand-file-name (concat project-root-dir "lib"))))))

(defun aflymake-perl-init ()
  (let* ((temp-file   (aflymake-init-create-temp-buffer-copy
                       'aflymake-create-temp-copy))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name)))
         (include-dir (aflymake-find-perl-lib-dir buffer-file-name)))
    (aflymake-perlbrew-path-sync)
    (list "perl" (append
      (list "-wc")
      (if include-dir (list "-I" include-dir))
      (list local-file)))))

;;;; php-specific init-cleanup routines
(defun aflymake-php-init ()
  (let* ((temp-file   (aflymake-init-create-temp-buffer-copy
                       'aflymake-create-temp-copy))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "php" (list "-f" local-file "-l"))))

;;;; javascript-specific init-cleanup routines
(defun aflymake-javascript-init ()
  (let* ((temp-file   (aflymake-init-create-temp-buffer-copy
                       'aflymake-create-temp-copy))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "jshint" (list local-file))))

;;;; css-specific init-cleanup routines
(defun aflymake-css-init ()
  (let* ((temp-file   (aflymake-init-create-temp-buffer-copy
                       'aflymake-create-temp-copy))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "csslint" (list "--format=compact" local-file))))

;; rpm spec files with rpmlint
(defun aflymake-specfile-init ()
    (let* ((temp-file (aflymake-init-create-temp-buffer-copy
                       'aflymake-create-temp-copy))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "rpmlint" (list local-file))))

;; check po files with msgfmt -c
(defun aflymake-pofile-init ()
    (let* ((temp-file (aflymake-init-create-temp-buffer-copy
                       'aflymake-create-temp-copy))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "msgfmt" (list "-c" local-file))))


;;;; tex-specific init-cleanup routines
(defun aflymake-get-tex-args (file-name)
  ;;(list "latex" (list "-c-style-errors" file-name))
  (list "texify" (list "--pdf" "--tex-option=-c-style-errors" file-name)))

(defun aflymake-simple-tex-init ()
  (aflymake-get-tex-args (aflymake-init-create-temp-buffer-copy 'aflymake-create-temp-copy)))

(defun aflymake-master-tex-init ()
  (let* ((temp-master-file-name (aflymake-init-create-temp-source-and-master-buffer-copy
                                 'aflymake-get-include-dirs-dot 'aflymake-create-temp-copy
                                 '("\\.tex\\'")
                                 "[ \t]*\\in\\(?:put\\|clude\\)[ \t]*{\\(.*%s\\)}")))
    (when temp-master-file-name
      (aflymake-get-tex-args temp-master-file-name))))

(defun aflymake-get-include-dirs-dot (_base-dir)
  '("."))

;;;; xml-specific init-cleanup routines
(defun aflymake-xml-init ()
  (list "xmlstarlet" (list "val" "-e" (aflymake-init-create-temp-buffer-copy 'aflymake-create-temp-copy))))

(provide 'aflymake)

;;; aflymake.el ends here

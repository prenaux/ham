;; *****************************************************************************
;;
;;  ham-mode.el
;;  Font-lock support for Ham files
;;
;;  Modified by Pierre Renaux to support hamfiles
;;
;;  ****************************************************************************
;;  Copyright (C) 2003, 2004, Rob Walker <rob@tenfoot.org.uk>
;;    http://www.tenfoot.org.uk/emacs/
;;  12 May 2004 - 0.3 - Fix keyword quoting, XEmacs support
;;  22 Mar 2003 - 0.2 - Autoload
;;  04 Mar 2003 - 0.1 - Added imenu support and basic indentation
;;
;;  Copyright (C) 2000, Eric Scouten
;;  Started Sat, 05 Aug 2000
;;
;; *****************************************************************************
;;
;;  This is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2, or (at your option)
;;  any later version.
;;
;;  ham-mode.el is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with GNU Emacs; see the file COPYING.  If not, write to the
;;  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;  Boston, MA 02111-1307, USA.
;;
;; *****************************************************************************
;;
;;  To add font-lock support for Ham files, simply add the line
;;  (require 'ham-mode) to your .emacs file. Make sure generic-mode.el
;;  is visible in your load-path as well.
;;
;; *****************************************************************************


;; Generic-mode is a meta-mode which can be used to define small modes
;; which provide basic comment and font-lock support. Ham-mode depends on
;; this mode.

;; generic.el for GNU emacs, generic-mode.el for XEmacs
(if (string-match "XEmacs\\|Lucid" emacs-version)
    (require 'generic-mode)
  (require 'generic))

(defun ham-mode-quote-keywords (keywords)
  "Returns a list of expressions that match each element in KEYWORDS.
For generic-mode, each element is quoted. For generic, each element is unchanged."
  (if (featurep 'generic-mode)
      (mapcar 'regexp-quote keywords)
    keywords))

;;;###autoload
(define-generic-mode 'ham-mode

  ; Ham comments always start with '#'
  (list ?# "#" )

  ; Ham keywords (defined later)
  nil

  ; Extra stuff to colorize
  (list

    ; Ham keywords
	(generic-make-keywords-list
	 (list "actions" "bind" "case" "default" "else" "existing" "for" "if"
		   "ignore" "in" "include" "local" "on" "piecemeal" "quietly" "rule" "switch"
		   "together" "updated" "return")
	 'font-lock-keyword-face)

	; Ham built-in variables
	(generic-make-keywords-list
	 (list
	  "HAMDATE" "HAMSHELL" "HAMUNAME" "HAMVERSION" "MAC" "NT" "OS" "OS2"
	  "OSPLAT" "OSVER" "UNIX" "VMS")
	 'font-lock-constant-face)

	; Ham built-in targets
	(generic-make-keywords-list
	 (list
	  "ALWAYS" "DEPENDS" "ECHO" "INCLUDES" "LEAVES" "LOCATE" "NOCARE"
	  "NOTFILE" "NOUPDATE" "SEARCH" "TEMPORARY")
	 'font-lock-builtin-face)

	; Ham built-in targets (warnings)
	(generic-make-keywords-list
	 (list
	  "EXIT")
	 'font-lock-warning-face)

	; Hambase rules
	(generic-make-keywords-list
	 (ham-mode-quote-keywords
          (list
           "Archive" "As" "Bulk" "Cc" "CcMv" "C++" "Chgrp" "Chmod" "Chown" "Clean" "CreLib"
           "Depends" "File" "Fortran" "GenFile" "GenFile1" "HardLink"
           "HdrRule" "Install" "InstallBin" "InstallFile" "InstallInto" "InstallLib" "InstallMan"
           "InstallShell" "Lex" "Library" "LibraryFromObjects" "Link" "LinkLibraries"
           "Main" "MainFromObjects" "MakeLocate" "MkDir" "MkDir1" "Object" "ObjectC++Flags"
           "ObjectCcFlags" "ObjectHdrs" "Objects" "Ranlib" "RmTemps" "Setuid" "SubDir"
           "SubDirC++Flags" "SubDirCcFlags" "SubDirHdrs" "SubInclude" "Shell" "Undefines"
           "UserObject" "Yacc" "Yacc1" "BULK" "FILE" "HDRRULE" "INSTALL" "INSTALLBIN" "INSTALLLIB"
           "INSTALLMAN" "LIBRARY" "LIBS" "LINK" "MAIN" "SETUID" "SHELL" "UNDEFINES"
           "addDirName" "makeCommon" "makeDirName" "makeGrist" "makeGristedName" "makeRelPath"
           "makeString" "makeSubDir" "makeSuffixed" "unmakeDir"))
	 'font-lock-function-name-face)

	; Hambase built-in targets
	(generic-make-keywords-list
	 (list
	  "all" "clean" "dirs" "exe" "files" "first" "install" "lib" "obj" "shell" "uninstall")
	 'font-lock-builtin-face)

	; Hambase built-in variables
	(generic-make-keywords-list
	 (ham-mode-quote-keywords
          (list
           "ALL_LOCATE_TARGET" "AR" "ARFLAGS" "AS" "ASFLAGS" "AWK" "BCCROOT" "BINDIR" "CC" "CCFLAGS"
           "C++" "C++FLAGS" "CHMOD" "CP" "CRELIB" "CW" "CWGUSI" "CWMAC" "CWMSL" "DOT" "DOTDOT"
           "EXEMODE" "FILEMODE" "FORTRAN" "FORTRANFLAGS" "GROUP" "HDRGRIST" "HDRPATTERN" "HDRRULE"
           "HDRS" "HDRSCAN" "HDRSEARCH" "INSTALL" "HAMFILE" "HAMRULES" "LEX" "LIBDIR" "LINK"
           "LINKFLAGS" "LINKLIBS" "LOCATE_SOURCE" "LOCATE_TARGET" "LN" "MACINC" "MANDIR" "MKDIR"
           "MODE" "MSLIB" "MSLINK" "MSIMPLIB" "MSRC" "MSVC" "MSVCNT" "MV" "NEEDLIBS" "NOARSCAN"
           "OSFULL" "OPTIM" "OWNER" "RANLIB" "RCP" "RELOCATE" "RM" "RSH" "RUNVMS" "SEARCH_SOURCE"
           "SED" "SHELLHEADER" "SHELLMODE" "SLASH" "SLASHINC" "SOURCE_GRIST" "STDHDRS" "STDLIBPATH"
           "SUBDIR" "SUBDIRASFLAGS" "SUBDIRC++FLAGS" "SUBDIRCCFLAGS" "SUBDIRHDRS" "SUBDIR_TOKENS"
           "SUFEXE" "SUFLIB" "SUFOBJ" "UNDEFFLAG" "UNDEFS" "WATCOM" "YACC" "YACCFLAGS" "YACCFILES"))
	 'font-lock-function-name-face)

     ; Ham variable references $(foo)
	'("$(\\([^ :\\[()\t\r\n]+\\)[)\\[:]" 1 font-lock-variable-name-face))

  ; Apply this mode to all files called Hamfile, Hamrules or Hambase
  (list "\\(_build.ham\\|_rules.ham\\|Hamfile\\|Hamrules\\|Hambase\\)\\'")

  ; Attach setup function so we can modify syntax table.
  (list 'ham-mode-setup-function)

  ; Brief description
  "Generic mode for Ham rules files")

(defun ham-mode-setup-function ()
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?. "w")
  (modify-syntax-entry ?/ "w")
  (modify-syntax-entry ?+ "w")
  (modify-syntax-entry ?# "<")
  (modify-syntax-entry ?\n ">")
  (setq imenu-generic-expression
        '(("Rules" "^rule\\s-+\\([A-Za-z0-9_]+\\)" 1)
          ("Actions" "^actions\\s-+\\([A-Za-z0-9_]+\\)" 1)))
  (imenu-add-to-menubar "Ham")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ham-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (run-hooks 'ham-mode-hook)
  )

(defvar ham-mode-hook nil)

(defvar ham-indent-size 2
  "Amount to indent by in ham-mode")

(defvar ham-case-align-to-colon nil
  "Whether to align case statements to the colons")

(defun ham-indent-line (&optional whole-exp)
  "Indent current line"
  (interactive)
  (let ((indent (ham-indent-level))
	(pos (- (point-max) (point))) beg)
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (zerop (- indent (current-column)))
	nil
      (delete-region beg (point))
      (indent-to indent))
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))
    ))

(defun ham-goto-block-start ()
  "Goto the start of the block containing point (or beginning of buffer if not
   in a block"
  (let ((l 1))
    (while (and (not (bobp)) (> l 0))
      (skip-chars-backward "^{}")
      (unless (bobp)
        (backward-char)
        (setq l (cond
                 ((eq (char-after) ?{) (1- l))
                 ((eq (char-after) ?}) (1+ l))
                 )))
      )
    (bobp))
  )

(defun ham-indent-level ()
  (save-excursion
    (let ((p (point))
          ind
          (is-block-start nil)
          (is-block-end nil)
          (is-case nil)
          (is-switch nil)
          switch-ind)
      ;; see what's on this line
      (beginning-of-line)
      (setq is-block-end (looking-at "^[^{\n]*}\\s-*$"))
      (setq is-block-start (looking-at ".*{\\s-*$"))
      (setq is-case (looking-at "\\s-*case.*:"))

      ;; goto start of current block (0 if at top level)
      (if (ham-goto-block-start)
          (setq ind 0)
        (setq ind (+ (current-indentation) ham-indent-size)))

      ;; increase indent in switch statements (not cases)
      (setq is-switch (re-search-backward "^\\s-*switch" (- (point) 100) t))
      (when (and is-switch (not (or is-block-end is-case)))
        (goto-char p)
        (setq ind (if (and ham-case-align-to-colon
                           (re-search-backward "^\\s-*case.*?\\(:\\)"))
                      (+ (- (match-beginning 1) (match-beginning 0))
                         ham-indent-size)
                    (+ ind ham-indent-size)))
        )

      ;; indentation of this line is ham-indent-size more than that of the
      ;; previous block
      (cond (is-block-start  ind)
            (is-block-end    (- ind ham-indent-size))
            (is-case         ind)
            (t               ind)
            )
      )))

(provide 'ham-mode)

;; ham-mode.el ends here

(provide 'ni-muse)
(require 'ni-base)
(require 'ham-setup)

;;;======================================================================
;;; Muse
;;;======================================================================
(agl-begin-time-block "Muse")

(add-to-list 'load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/muse/lisp"))
(add-to-list 'load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/muse/contrib"))
(add-to-list 'load-path (concat ENV_DEVENV_EMACS_SCRIPTS "/muse/experimental"))

(require 'muse)
(require 'muse-mode)
(require 'muse-colors)
;; (require 'muse-project)
(require 'muse-book)
(require 'muse-html)
;; (require 'muse-latex)
;; (require 'muse-texinfo)
;; (require 'muse-journal)
;; (require 'muse-poem)
;; (require 'muse-message)
;; (require 'muse-http)
;; (require 'muse-blosxom)  ; load blosxom module
;; (require 'muse-docbook)  ; load DocBook publishing style
;; (require 'muse-latex2png) ; publish <latex> tags
(require 'muse-wiki)     ; load wiki support
;; (require 'muse-xml)      ; load XML support

(add-hook 'muse-mode-hook 'turn-on-orgtbl)

(defconst MUSE_HTML_HEADER (or (getenv "MUSE_HTML_HEADER") (concat ENV_DEVENV_EMACS_SCRIPTS "/muse-tpl/web-header.html")))
;; (message (concat "MUSE_HTML_HEADER:" MUSE_HTML_HEADER))
(defconst MUSE_HTML_FOOTER (or (getenv "MUSE_HTML_FOOTER") (concat ENV_DEVENV_EMACS_SCRIPTS "/muse-tpl/web-footer.html")))
;; (message (concat "MUSE_HTML_FOOTER:" MUSE_HTML_FOOTER))
(defconst MUSE_TEX_HEADER (or (getenv "MUSE_TEX_HEADER") (concat ENV_DEVENV_EMACS_SCRIPTS "/muse-tpl/header.tex")))
(defconst MUSE_TEX_FOOTER (or (getenv "MUSE_TEX_FOOTER") (concat ENV_DEVENV_EMACS_SCRIPTS "/muse-tpl/footer.tex")))

(if (not (getenv "HAM_HOME"))
    (setenv "HAM_HOME" (concat (ham-getenv "WORK") "/ham")))

(custom-set-variables
;;  '(muse-blosxom-base-directory (concat ENV_WORK "Blog/"))
 '(muse-colors-autogen-headings (quote outline))
 '(muse-colors-inline-image-method (quote muse-colors-use-publishing-directory))
;;  '(muse-completing-read-function (quote ido-completing-read))
 '(muse-html-charset-default "utf-8")
 '(muse-html-encoding-default (quote utf-8))
 '(muse-html-header MUSE_HTML_HEADER)
 '(muse-html-footer MUSE_HTML_FOOTER)
 '(muse-html-meta-content-encoding (quote utf-8))
 '(muse-latex-header MUSE_TEX_HEADER)
 '(muse-latex-footer MUSE_TEX_FOOTER)
;;  '(muse-mode-hook (quote (flyspell-mode footnote-mode)))
;;  '(muse-publish-comments-p t)
 '(muse-publish-date-format "%b. %e, %Y")
 '(muse-publish-desc-transforms (quote (muse-wiki-publish-pretty-title muse-wiki-publish-pretty-interwiki muse-publish-strip-URL)))
 '(muse-wiki-publish-small-title-words (quote ("the" "and" "at" "on" "of" "for" "in" "an" "a" "page")))
 '(muse-xhtml-header MUSE_HTML_HEADER)
 '(muse-xhtml-footer MUSE_HTML_FOOTER))

;; (custom-set-faces
;;  '(muse-bad-link ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)))))

;; We dont want to use wiki words in our documents
(setq muse-wiki-use-wikiword nil)
;; We don't want the contents page by default
(setq muse-publish-generate-contents nil)

(defun agl-ignore ())

;; Register the curstom tags
(add-to-list 'muse-publish-markup-tags '("pb" t t t ignore))

;; Register the agl-pdf types
(defcustom muse-agl-pdf-markup-tags
  '(
	("pb" t t   t muse-agl-pdf-pb-tag)
	)
  "A list of tag specifications, for specially marking up Latex-PDF."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Nestable" :value nil)
                       function))
  :group 'muse-latex)

(muse-derive-style "agl-pdf" "pdf" :tags 'muse-agl-pdf-markup-tags)

(muse-derive-style "agl-tex" "latex" :tags 'muse-agl-pdf-markup-tags)

(defun muse-agl-pdf-pb-tag (beg end attrs)
  (save-excursion
	(goto-char beg)
    (muse-insert-markup "\\newpage ")
    (goto-char end)))

(defcustom muse-agl-heading-regexp
  "\\(?:\\([0-9]+\\)\\(?:: \\)?\\)?\\(.+?\\)?"
  "A regexp that matches a journal heading.
Paren group 1 is the ISO date, group 2 is the optional category,
and group 3 is the optional heading for the entry."
  :type 'regexp
  :group 'muse-agl)

(defcustom muse-agl-date-format "%a, %e %b %Y"
  "Date format to use for journal entries."
  :type 'string
  :group 'muse-agl)

(defcustom muse-agl-html-heading-regexp
  (concat "^<h2[^>\n]*>" muse-agl-heading-regexp "</h2>$")
  "A regexp that matches a journal heading from an HTML document.
Paren group 1 is the ISO date, group 2 is the optional category,
and group 3 is the optional heading for the entry."
  :type 'regexp
  :group 'muse-agl)

(defcustom muse-agl-html-entry-template
  "<div class=\"entry\">
  <a name=\"%anchor%\" class=\"anchor\"></a>
  <div class=\"entry-body\">
    <div class=\"entry-head\">
      <div class=\"entry-date\">
        <span class=\"date\">%date%</span>
      </div>
      <div class=\"entry-title\">
        <h2>%title%</h2>
      </div>
    </div>
    <div class=\"entry-text\">
      <div class=\"entry-qotd\">
        <p>%qotd%</p>
      </div>
%text%
    </div>
  </div>
</div>\n\n"
  "Template used to publish individual journal entries as HTML.
This may be text or a filename."
  :type 'string
  :group 'muse-agl)

(defun muse-agl-anchorize-title (title)
  "This strips tags from TITLE, truncates TITLE at begin parenthesis,
and escapes any remaining non-alphanumeric characters."
  (save-match-data
    (if (string-match "(" title)
        (setq title (substring title 0 (match-beginning 0))))
    (if (string-match "<[^>]+>" title)
        (setq title (replace-match "" nil nil title)))
    (let (pos code len ch)
      (while (setq pos (string-match (concat "[^" muse-regexp-alnum "_]")
                                     title pos))
          (setq ch (aref title pos)
                code (format "%%%02X" (cond ((fboundp 'char-to-ucs)
                                             (char-to-ucs ch))
                                            ((fboundp 'char-to-int)
                                             (char-to-int ch))
                                            (t ch)))
                len (length code)
                title (concat (substring title 0 pos)
                              code
                              (when (< pos (length title))
                                (substring title (1+ pos) nil)))
                pos (+ len pos)))
        title)))

(defun muse-agl-html-munge-buffer ()
  (goto-char (point-min))
  (let ((heading-regexp muse-agl-html-heading-regexp)
        (inhibit-read-only t))
    (while (re-search-forward heading-regexp nil t)
      (let* ((date (match-string 1))
             (orig-date date)
             (title (match-string 2))
             (clean-title title)
             datestamp qotd text)
        (delete-region (match-beginning 0) (match-end 0))
        (if clean-title
            (save-match-data
              (while (string-match "\\(^<[^>]+>\\|<[^>]+>$\\)" clean-title)
                (setq clean-title (replace-match "" nil nil clean-title)))))
        (save-match-data
          (when (and date
                     (string-match
                      (concat "\\`\\([1-9][0-9][0-9][0-9]\\)[./]?"
                              "\\([0-1][0-9]\\)[./]?\\([0-3][0-9]\\)") date))
            (setq datestamp
                  (encode-time
                   0 0 0
                   (string-to-number (match-string 3 date))
                   (string-to-number (match-string 2 date))
                   (string-to-number (match-string 1 date))
                   nil)
                  date (concat (format-time-string
                                muse-agl-date-format datestamp)
                               (substring date (match-end 0))))))
        (save-restriction
          (narrow-to-region
           (point) (if (re-search-forward
                        (concat "\\(^<hr>$\\|"
                                heading-regexp "\\)") nil t)
                       (match-beginning 0)
                     (point-max)))
          (goto-char (point-max))
          (while (and (not (bobp))
                      (eq ?\  (char-syntax (char-before))))
            (delete-char -1))
          (goto-char (point-min))
          (while (and (not (eobp))
                      (eq ?\  (char-syntax (char-after))))
            (delete-char 1))
          (save-excursion
            (when (search-forward "<qotd>" nil t)
              (let ((tag-beg (match-beginning 0))
                    (beg (match-end 0))
                    end)
                (re-search-forward "</qotd>\n*")
                (setq end (point-marker))
                (save-restriction
                  (narrow-to-region beg (match-beginning 0))
                  (muse-publish-escape-specials (point-min) (point-max)
                                                nil 'document)
                  (setq qotd (buffer-substring-no-properties
                              (point-min) (point-max))))
                (delete-region tag-beg end)
                (set-marker end nil))))
          (setq text (buffer-string))
          (delete-region (point-min) (point-max))
          (let ((entry muse-agl-html-entry-template))
            (muse-insert-file-or-string entry)
            (muse-publish-mark-read-only (point-min) (point-max))
            (goto-char (point-min))
            (while (search-forward "%date%" nil t)
              (remove-text-properties (match-beginning 0) (match-end 0)
                                      '(read-only nil rear-nonsticky nil))
              (replace-match (or date "") nil t))
            (goto-char (point-min))
            (while (search-forward "%title%" nil t)
              (remove-text-properties (match-beginning 0) (match-end 0)
                                      '(read-only nil rear-nonsticky nil))
              (replace-match (or title "&nbsp;") nil t))
            (goto-char (point-min))
            (while (search-forward "%anchor%" nil t)
              (replace-match (muse-agl-anchorize-title
                              (or clean-title orig-date))
                             nil t))
            (goto-char (point-min))
            (while (search-forward "%qotd%" nil t)
              (save-restriction
                (narrow-to-region (match-beginning 0) (match-end 0))
                (delete-region (point-min) (point-max))
                (when qotd (muse-insert-markup qotd))))
            (goto-char (point-min))
            (while (search-forward "%text%" nil t)
              (remove-text-properties (match-beginning 0) (match-end 0)
                                      '(read-only nil rear-nonsticky nil))
              (replace-match text nil t))
            (when (null qotd)
              (goto-char (point-min))
              (when (search-forward "<div class=\"entry-qotd\">" nil t)
                (let ((beg (match-beginning 0)))
                  (re-search-forward "</div>\n*" nil t)
                  (delete-region beg (point))))))))))
  ;; indicate that we are to continue the :before-end processing
  nil)

(defun muse-agl-html-insert-contents (depth)
  "Scan the current document and generate a table of contents at point.
DEPTH indicates how many levels of headings to include.  The default is 2."
  (let ((max-depth (or depth 2))
        (index 1)
        base contents l end)
    (save-excursion
      (goto-char (point-min))
      (search-forward "Page published by Emacs Muse begins here" nil t)
      (catch 'done
        (while (re-search-forward "<h\\([0-9]+\\)>\\(.+?\\)</h\\1>$" nil t)
;;           (unless (and (get-text-property (point) 'read-only)
;;                        (not (get-text-property (match-beginning 0)
;;                                                'muse-contents)))
            (remove-text-properties (match-beginning 0) (match-end 0)
                                    '(muse-contents nil))
            (setq l (1- (string-to-number (match-string 1))))
            (if (null base)
                (setq base l)
              (if (< l base)
                  (throw 'done t)))
            (when (<= l max-depth)
              ;; escape specials now before copying the text, so that we
              ;; can deal sanely with both emphasis in titles and
              ;; special characters
              ;; (goto-char (match-end 2))
              ;; (muse-insert-markup "<a href='#top' class='to_top'>^</a>")
              (goto-char (match-end 2))
              (setq end (point-marker))
              (muse-publish-escape-specials (match-beginning 2) end
                                            nil 'document)
              (muse-publish-mark-read-only (match-beginning 2) end)
              (setq contents (cons (cons l (buffer-substring-no-properties
                                            (match-beginning 2) end))
                                   contents))
              (set-marker end nil)
              (goto-char (match-end 2))

              (muse-insert-markup (format "<a class='muse-anchor' name=\"%s\" id=\"%s\"><a href='#top' class='to_top'>^</a></a>"
                                          (concat "sec" (int-to-string index))
                                          (concat "sec" (int-to-string index))))

              (setq index (1+ index))))
;;           )
        ))
    (setq index 1 contents (nreverse contents))
    (let ((depth 1) (sub-open 0) (p (point)))
      (muse-insert-markup "<div id=\"contents\">\n<ul>\n")
      (while contents
        (muse-insert-markup "<li>\n"
                            "<a href=\"#sec" (int-to-string index) "\">"
                            (muse-html-strip-links (cdar contents))
                            "</a>\n"
                            "</li>\n")
        (setq index (1+ index)
              depth (caar contents)
              contents (cdr contents))
        (when contents
          (cond
           ((< (caar contents) depth)
            (let ((idx (caar contents)))
              (while (< idx depth)
                (muse-insert-markup "</ul>\n")
                (setq sub-open (1- sub-open)
                      idx (1+ idx)))))
           ((> (caar contents) depth) ; can't jump more than one ahead
            (muse-insert-markup "<ul>\n")
            (setq sub-open (1+ sub-open))))))
      (while (> sub-open 0)
        (muse-insert-markup "</ul>\n")
        (setq sub-open (1- sub-open)))
      (muse-insert-markup "</ul>\n</div>\n")
      (muse-publish-mark-read-only p (point)))))

(defun muse-agl-html-denote-headings ()
  "Place a text property on any headings in the current buffer.
This allows the headings to be picked up later on if publishing a
table of contents."
  (save-excursion
    (goto-char (point-min))
    (search-forward "Page published by Emacs Muse begins here" nil t)
    (while (re-search-forward "<h\\([0-9]+\\)>\\(.+?\\)</h\\1>$" nil t)
      (unless (get-text-property (point) 'read-only)
        (add-text-properties (match-beginning 0) (match-end 0)
                             '(muse-contents t))))))

(defun muse-agl-html-munge-final-buffer ()
  (if muse-publish-generate-contents
      (progn
        (goto-char (car muse-publish-generate-contents))
        (muse-agl-html-insert-contents (cdr muse-publish-generate-contents))
        (setq muse-publish-generate-contents nil))
    (muse-agl-html-denote-headings))
t
)

(muse-derive-style "agl-html-0" "html"
                   :before-end 'muse-agl-html-munge-final-buffer
                   )

(muse-derive-style "agl-html" "agl-html-0" :before-end 'muse-agl-html-munge-buffer)

(defun agl-muse-publish-to-pdf ()
  ""
  (interactive)
  (muse-publish-this-file
   (muse-style "agl-pdf")
   (file-name-directory buffer-file-name)))
(defun agl-muse-publish-to-html ()
  ""
  (interactive)
  (muse-publish-this-file
   (muse-style "agl-html")
   (file-name-directory buffer-file-name)))

(global-set-key "\C-xp" 'agl-muse-publish-to-html)
(global-set-key "\C-xP" 'agl-muse-publish-to-html)
;; (global-set-key "\C-x\C-p" 'agl-muse-publish-to-pdf)

;; Cause the HOME env-var isn't set on Windows and Emacs defaults to AppData/Roaming...
(setenv "HOME" (getenv "USERPROFILE"))

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (normal-top-level-add-subdirs-to-load-path))

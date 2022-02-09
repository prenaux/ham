;;
;; The settings below piggy back on a persistent connection that you start on
;; your machine to make tramp much quicker and resilient to lost connectivity.
;;
;;
;; Establish a resilient connection using Eternal Terminal:
;; ```
;; et -t "10022:22" -r "1492:1492" ${USER}@${DEVSERVER}:8080
;; ```
;;
;; To establish a shared connection that piggy backs on this connection:
;;
;; 1) in ~/.ssh/config:
;; ```
;; Host devf
;;   User prenaux
;;   HostName localhost
;;   HostKeyAlias prenaux.sb.facebook.com
;;   Port 10022
;;   ControlMaster auto
;;   ControlPersist yes
;;   ControlPath ~/.ssh/master-%C
;; ```
;; 2) check that it works in the terminal:
;; ```
;; ssh -t -t -l prenaux -o RemoteCommand='/bin/bash -i' devf
;; ```
;;
;; The last thing you might need to take care is the hooks or idle-time
;; functions you set in your ~/.emacs. If you ever invoke shell to run
;; commands (e.g. via `shell-command' function ...) in those hooks/idle-time
;; functions, those commands might be passed to remote machine and will be
;; very slow too. You need to wrap those shell commands with the current
;; buffer changed to somewhere else, like the 'scratch' buffer, for example:
;;
;;   (with-current-buffer (get-buffer "*scratch*")
;;        (shell-command "ls -al")) ;; just a sample shell command
;;
(provide 'ni-tramp)
(require 'tramp)

;; Add a ni-ssh method to make it clear its another connection.
(add-to-list 'tramp-methods
             `("ni-ssh"
               (tramp-login-program        "ssh")
               (tramp-login-args           (("-t" "-t") ("-l" "%u")
				                                    ("-o" "RemoteCommand='/bin/bash -i'")
					                                  ("%h")))
               (tramp-remote-shell         ,tramp-default-remote-shell)
               (tramp-remote-shell-login   ("-l"))
               (tramp-remote-shell-args    ("-c"))))

(custom-set-variables
  ;; run the emacs server over TCP for "remote" access
  '(server-port "1492")
  '(server-use-tcp t)
  ;; Make auto-complete in minibuffer as fast as possible
  '(tramp-completion-reread-directory-timeout nil)
  ;; You'll want to setup some ssh wizardry (more on this later),
  ;; but this ignores login prompts
  '(tramp-default-method "ni-ssh")
  ;; By default tramp clobbers the ssh ControlPath setting used below to avoid
  ;; the password prompt. Instead we want it to use the config in or
  ;; ~/.ssh/config.
  '(tramp-use-ssh-controlmaster-options nil)
)

;;;; Use bash as the shell to run commands
;; (setq tramp-encoding-shell "/bin/bash")
;; (add-to-list 'tramp-connection-properties list (regexp-quote "/ni-ssh:") "remote-shell" "/bin/bash")

;; Since we're going to be doing this a lot, the minibar message
;; tramp spits out for every file access is both spammy, distracting,
;; and often hides more relevant messages.
(setq tramp-message-show-message nil)
;; Let tramp search $PATH as given to the $USER on the remote machine
;; (necessary to find 'hphpd' for instance)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; Disable tramp source control
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; Disable backup of tramp files
;;(setq disable-tramp-backups nil) ;; allow all tramp files to be backuped
;;(setq disable-tramp-backups '("su" "sudo")) ;; only 'su' and 'sudo'
;;(setq disable-tramp-backups '("ssh" "sftp")) ;; only 'ssh' and 'sftp'
(defvar disable-tramp-backups '(all))

(eval-after-load "tramp"
  '(progn
     ;; Modified from https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html
     (setq backup-enable-predicate
           (lambda (name)
             (and (normal-backup-enable-predicate name)
                  ;; Disable all tramp backups
                  (and disable-tramp-backups
                       (member 'all disable-tramp-backups)
                       (not (file-remote-p name 'method)))
                  (not ;; disable backup for tramp with the listed methods
                   (let ((method (file-remote-p name 'method)))
                     (when (stringp method)
                       (member method disable-tramp-backups)))))))

     (defun tramp-set-auto-save--check (original)
       (if (funcall backup-enable-predicate (buffer-file-name))
           (funcall original)
         (auto-save-mode -1)))

     (advice-add 'tramp-set-auto-save :around #'tramp-set-auto-save--check)

     ))

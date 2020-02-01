;;
;; Commands to make working with GDB easier.
;;
;; The general flow is to have GDB open in a terminal window, not in Emacs
;; directly - because that just doesnt work that great imo. So ni-gdb
;; facilitate mostly writting commands for GDB, maybe one day we'll make it
;; communicate with the command line instance.
;;
(provide 'ni-gdb)
(NotBatchMode

 (defun nigdb-bp (&optional *dir-path-only-p)
   ""
   (interactive "P")
   (let ((filePath
          (if (equal major-mode 'dired-mode)
              (expand-file-name default-directory)
            (if (buffer-file-name)
                (buffer-file-name)
              (user-error "Current buffer is not associated with a file.")))))
     (kill-new
      (let ((cmd (concat "breakpoint set --file " filePath " --line " (number-to-string (1+ (count-lines 1 (point)))))))
        (message "Breakpoint command copied: %s" cmd)
        cmd))))

)

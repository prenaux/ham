(provide 'ni-emacs24-fixup)

;;
;;This prevents insanity where, click will 'half-set' the mark so that it
;; works as expected mostly, excepted that if you do a search (C-s) right
;; after you get the mark set from the (mouse-drag-region) to the result
;; of the search.
;;
;; It has the side effect from disabling the interactive visualisation of
;; region being selected with the mouse... if you do that... which I dont
;; so...
;;
(GNUEmacs24
 (defun mouse-drag-track (start-event  &optional
                                       do-mouse-drag-region-post-process)
   "Track mouse drags by highlighting area between point and cursor.
The region will be defined with mark and point.
DO-MOUSE-DRAG-REGION-POST-PROCESS should only be used by
`mouse-drag-region'."
   (mouse-minibuffer-check start-event)
   (setq mouse-selection-click-count-buffer (current-buffer))
   (deactivate-mark)
   (let* ((scroll-margin 0) ; Avoid margin scrolling (Bug#9541).
          (original-window (selected-window))
          ;; We've recorded what we needed from the current buffer and
          ;; window, now let's jump to the place of the event, where things
          ;; are happening.
          (_ (mouse-set-point start-event))
          (echo-keystrokes 0)
          (start-posn (event-start start-event))
          (start-point (posn-point start-posn))
          (start-window (posn-window start-posn))
          (start-window-start (window-start start-window))
          (start-hscroll (window-hscroll start-window))
          (bounds (window-edges start-window))
          (make-cursor-line-fully-visible nil)
          (top (nth 1 bounds))
          (bottom (if (window-minibuffer-p start-window)
                      (nth 3 bounds)
                    ;; Don't count the mode line.
                    (1- (nth 3 bounds))))
          (on-link (and mouse-1-click-follows-link
                        (or mouse-1-click-in-non-selected-windows
                            (eq start-window original-window))
                        ;; Use start-point before the intangibility
                        ;; treatment, in case we click on a link inside an
                        ;; intangible text.
                        (mouse-on-link-p start-posn)))
          (click-count (1- (event-click-count start-event)))
          (remap-double-click (and on-link
                                   (eq mouse-1-click-follows-link 'double)
                                   (= click-count 1)))
          ;; Suppress automatic hscrolling, because that is a nuisance
          ;; when setting point near the right fringe (but see below).
          (automatic-hscrolling-saved automatic-hscrolling)
          (automatic-hscrolling nil)
          event end end-point)

     (setq mouse-selection-click-count click-count)
     ;; In case the down click is in the middle of some intangible text,
     ;; use the end of that text, and put it in START-POINT.
     (if (< (point) start-point)
         (goto-char start-point))
     (setq start-point (point))
     (if remap-double-click
         (setq click-count 0))

     ;; Activate the region, using `mouse-start-end' to determine where
     ;; to put point and mark (e.g., double-click will select a word).

     ;;
     ;; PIERRE: --- See above for why I've commented this stuff --- note
     ;;         that er/expand-region on double-mouse-1 takes care of selecting
     ;;         a region on double click.
     ;;
     (setq transient-mark-mode
           (if (eq transient-mark-mode 'lambda)
               '(only)
             (cons 'only transient-mark-mode)))
     (let ((range (mouse-start-end start-point start-point click-count)))
       ;; (push-mark (nth 0 range) t t)
       (goto-char (nth 1 range)))

     ;; Track the mouse until we get a non-movement event.
     (track-mouse
       (while (progn
                (setq event (read-event))
                (or (mouse-movement-p event)
                    (memq (car-safe event) '(switch-frame select-window))))
         (unless (memq (car-safe event) '(switch-frame select-window))
           ;; Automatic hscrolling did not occur during the call to
           ;; `read-event'; but if the user subsequently drags the
           ;; mouse, go ahead and hscroll.
           (let ((automatic-hscrolling automatic-hscrolling-saved))
             (redisplay))
           (setq end (event-end event)
                 end-point (posn-point end))
           (if (and (eq (posn-window end) start-window)
                    (integer-or-marker-p end-point))
               (mouse--drag-set-mark-and-point start-point
                                               end-point click-count)
             (let ((mouse-row (cdr (cdr (mouse-position)))))
               (cond
                ((null mouse-row))
                ((< mouse-row top)
                 (mouse-scroll-subr start-window (- mouse-row top)
                                    nil start-point))
                ((>= mouse-row bottom)
                 (mouse-scroll-subr start-window (1+ (- mouse-row bottom))
                                    nil start-point))))))))

     ;; Handle the terminating event if possible.
     (when (consp event)
       ;; Ensure that point is on the end of the last event.
       (when (and (setq end-point (posn-point (event-end event)))
                  (eq (posn-window end) start-window)
                  (integer-or-marker-p end-point)
                  (/= start-point end-point))
         (mouse--drag-set-mark-and-point start-point
                                         end-point click-count))

       ;; Find its binding.
       (let* ((fun (key-binding (vector (car event))))
              (do-multi-click (and (> (event-click-count event) 0)
                                   (functionp fun)
                                   (not (memq fun '(mouse-set-point
                                                    mouse-set-region))))))
         (if (and (/= (mark) (point))
                  (not do-multi-click))

             ;; If point has moved, finish the drag.
             (let* (last-command this-command)
               (and mouse-drag-copy-region
                    do-mouse-drag-region-post-process
                    (let (deactivate-mark)
                      (copy-region-as-kill (mark) (point)))))

           ;; If point hasn't moved, run the binding of the
           ;; terminating up-event.
           (if do-multi-click
               (goto-char start-point)
             (deactivate-mark))
           (when (and (functionp fun)
                      (= start-hscroll (window-hscroll start-window))
                      ;; Don't run the up-event handler if the window
                      ;; start changed in a redisplay after the
                      ;; mouse-set-point for the down-mouse event at
                      ;; the beginning of this function.  When the
                      ;; window start has changed, the up-mouse event
                      ;; contains a different position due to the new
                      ;; window contents, and point is set again.
                      (or end-point
                          (= (window-start start-window)
                             start-window-start)))
             (when (and on-link
                        (= start-point (point))
                        (mouse--remap-link-click-p start-event event))
               ;; If we rebind to mouse-2, reselect previous selected
               ;; window, so that the mouse-2 event runs in the same
               ;; situation as if user had clicked it directly.  Fixes
               ;; the bug reported by juri@jurta.org on 2005-12-27.
               (if (or (vectorp on-link) (stringp on-link))
                   (setq event (aref on-link 0))
                 (select-window original-window)
                 (setcar event 'mouse-2)
                 ;; If this mouse click has never been done by the
                 ;; user, it doesn't have the necessary property to be
                 ;; interpreted correctly.
                 (put 'mouse-2 'event-kind 'mouse-click)))
             (push event unread-command-events))))))))

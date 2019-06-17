;; Happy hacking brian!

(setq lll (window-list (window-frame (selected-window))))

((car lll))
(cadr lll)





(window-tree)

((t
  (0 0 140 91)
  #<window 13 on *scratch*> #<window 37 on *Help*>)
 #<window 4 on  *Minibuf-0*>)

(defun swap-windows ()
  "Exchange windows in a two configuration layout."
  (interactive)

  (let ((window-list (window-list (window-frame (selected-window)))))
    ;; TODO: Can we support this for > 2 windows? Also, this looks
    ;; busted for a single window...
    (message (format "WL Len: %d" (length window-list)))
    (cond ((> (length window-list) 2)
	   (error "This only works for two windows."))
	  ((< (length window-list) 2)
	   (message "Cowardly refusing to swap a single window."))
	  (t
	   (let* ((first-window (car window-list))
		  (second-window (cadr window-list))
		  (first-buffer (window-buffer first-window))
		  (second-buffer (window-buffer second-window)))
	     (set-window-buffer first-window second-buffer)
	     (set-window-buffer second-window first-buffer))))))

;; Hirose Yuuji and Bob Wiener
(defun resize-window (&optional arg)
  "Resize window interactively."
  (interactive "p")
  (if (one-window-p) (error "(resize-window): Cannot resize sole window."))
  (or arg (setq arg 1))
  (let (c)
    (catch 'done
      (while t
	(message
	 "h=heighten, s=shrink, w=widen, n=narrow (by %d);  1-9=unit, q=quit"
	 arg)
	(setq c (read-char))
	(condition-case ()
	    (cond
	     ((= c ?h) (enlarge-window arg))
	     ((= c ?s) (shrink-window arg))
	     ((= c ?w) (enlarge-window-horizontally arg))
	     ((= c ?n) (shrink-window-horizontally arg))
	     ((= c ?\^G) (keyboard-quit))
	     ((= c ?q) (throw 'done t))
	     ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
	     (t (beep)))
	  (error (beep)))))
    (message "Done.")))

;; Bob Wiener
(defun check-parentheses ()
  "Check the buffer for unbalanced parentheses.
Stops at any that are unbalanced."
  (interactive)
  (let ((start-point (point)))
    (goto-char (point-min))
    (condition-case e
	(while (/= (point) (point-max))
	  (forward-sexp))
      (error
       ;; If this is an extra left paren error, we have to scan backwards to 
       ;; find the exact left paren in error
       (cond ((and (eq (car e) 'error)
		   (string-equal (car (cdr e)) "Unbalanced parentheses"))
	      ;; left paren error
	      (goto-char (point-max))
	      (while (/= (point) (point-min))
		(condition-case e (backward-sexp)
		  (error
		   (error "Probably an extra left parenthesis here.")))))
	     (t
	      (error "Probably an extra right parenthesis here.")))))
    (goto-char start-point)
    (message "All parentheses appear balanced.")))


;; Copped from load-library
;; Added silent flag  -- peter 02-Sep-96 
(defun which (exe &optional insert &optional silent) 
  "Show the full path name of an executable.
With a prefix argument, insert the full-path name at point.
This command searches the directories in `exec-path'"
  (interactive "sWhich: \nP")
  (catch 'answer
    (mapcar
     '(lambda (dir)
	(mapcar
	 '(lambda (suf)
	    (let ((try (expand-file-name (concat exe suf) dir)))
	      (and (file-executable-p try)
		   (null (file-directory-p try))
		   (progn
		     (if insert
			 (insert try)
		       (or silent
			   (message "%s is %s" exe try))
		      )
		     (throw 'answer try)))))
	 windows-suffixes))
     exec-path)
    (or silent
	(message "Can't find %s in search path" exe))
    nil))

;; Steve Gonedes
(defun which2 (file &optional append)
  (let ((result (find-if #'(lambda (dir)
                  (member file (directory-files dir nil)))
            exec-path)))
    (if append (concat (file-name-as-directory result) file)
        result)))

(defun string-reverse (string)
  (concat (reverse (append string nil))))

(defun my-toggle-scroll-bar-side ()
  "Toggle the side in which the scroll-bar is displayed."
  (interactive)
  (set-scroll-bar-mode nil
        (cond ((eq scroll-bar-mode 'left) 'right)
              ((eq scroll-bar-mode 'right) 'left))))

(defun my-info-other-frame ()
  "Start Info, the documentation browser, in another frame." 
  (interactive)
  (let ((pop-up-frames t)
	(buffer (current-buffer)))
    (pop-to-buffer buffer t)
    (raise-frame (window-frame (selected-window)))
    (message "starting *info* in other frame ..." )
    (info)))
(global-set-key "\C-x5i" 'my-info-other-frame )

;; Stephen Eglen
(defun count-words (beg end)
  "Count the number of words in the current region."
  (interactive "r")
  (save-excursion
    (let ( (words 0))
      (goto-char beg)
      (while (< (point) end)
	(forward-word 1)
	(setq words (1+ words)))
      (message "%d words in region" words))))

;; Roger E. Wichmann
(defun word-count (start end)
  "Count the number of lines, words and characters in the current region."
  (interactive "r")
  (let ((words 0) (lines 0) (chars 0))
    (save-excursion
      (goto-char start)
      (while (< (point) end) (forward-word 1) (setq words (1+ words))))
    (setq lines (count-lines start end) chars (- end start))
    (message "Region has %d lines, %d words and %d characters."
             lines words chars)))

;; Erik Naggum
(defun recursive-edit-with-single-window ()
  "Enter a recursive edit with the current window as the single window.
Upon exit, restore window configuration in current frame.
Exit with \\[exit-recursive-edit] (exit) or \\[abort-recursive-edit] (abort)."
  (interactive)
  (save-window-excursion
    (delete-other-windows)
    (recursive-edit)))

;; Steffen Ries
(defun query-replace-region (from to)
  "Do a `query-replace' in the region only."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (let ((mark-active nil))
	(goto-char (point-min))
	(call-interactively 'query-replace)))))

;; Anonymous
(defun shell-rectangle (start end command)
  (interactive "r\nsShell command: ")
  (let ((lst (extract-rectangle start end))
        (nl  (count-lines start end))
        (buf (current-buffer))
        (shell-buf (generate-new-buffer " shell-rectangle")))
    (set-buffer shell-buf)
    (while lst
      (insert (car lst) ?\n)
      (setq lst (cdr lst)))
    (shell-command-on-region (point-min) (point-max) command t t)
    (if (not (= nl (count-lines (point-min) (point-max))))
        (progn
          (display-buffer shell-buf)
          (if (y-or-n-p "shell-rectangle height mismatch.  Abort?")
              (error "Aborted shell-rectangle"))))
    (goto-char (point-min))
    (set-buffer buf)
    (operate-on-rectangle 'replace-shell-rectangle-line start end t)
    (kill-buffer shell-buf)))

(defun replace-shell-rectangle-line (startdelpos begextra endextra)
  (delete-region startdelpos (point))
  (insert (save-excursion
            (set-buffer shell-buf)
            (buffer-substring (point) (progn (forward-line) (1- (point)))))))

;; ECL
(defun my-delete-blank-lines-region (beg end)
  "Delete blank lines in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (delete-matching-lines "^[ \t]*$"))))

(defun my-delete-blank-lines-region2 ()
  "Delete blank lines in the region.
If the mark is not set or inactive, act like `delete-blank-lines'."
  (interactive)
  (if mark-active
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (mark))
          (goto-char (point-min))
          (delete-matching-lines "^[ \t]*$")))
    (delete-blank-lines)))

(defun my-copy-line (&optional arg)
  "Copy the rest of the current line (or ARG lines) to the kill-ring.
Like \\[kill-line], except that the lines are not really killed, just copied
to the kill-ring. See the documentation of `kill-line' for details."
  (interactive "P")
  (copy-region-as-kill (point)
                       (save-excursion
                         (if arg
                             (forward-visible-line (prefix-numeric-value arg))
                           (if (eobp) (signal 'end-of-buffer nil))
                           (if (or (looking-at "[ \t]*$") (and kill-whole-line (bolp)))
                               (forward-visible-line 1)
                             (end-of-visible-line)))
                         (point))))

(defun my-copy-line2 (&optional arg)
  "Copy the rest of the current line (or ARG lines) to the kill-ring.
Like \\[kill-line], except that the lines are not really killed, just
copied to the kill-ring. See the documentation of `kill-line' for more
details."
  (interactive "P")
  (let ((buffer-read-only t))
    (save-excursion
      (kill-line (prefix-numeric-value arg)))))

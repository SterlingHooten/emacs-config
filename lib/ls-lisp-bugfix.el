;; Time-stamp: <2003-05-19 08:56:41 Emilio C. Lopes>

(defun ls-lisp-insert-directory
  (file switches time-index wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.  This is an internal function
optionally called by the `ls-lisp.el' version of `insert-directory'.
It is called recursively if the -R switch is used.
SWITCHES is a *list* of characters.  TIME-INDEX is the time index into
file-attributes according to SWITCHES.  WILDCARD is nil or an *Emacs
regexp*.  FULL-DIRECTORY-P means file is a directory and SWITCHES does
not contain `d', so that a full listing is expected."
  ;; Sometimes we get ".../foo*/" as FILE.  While the shell and
  ;; `ls' don't mind, we certainly do, because it makes us think
  ;; there is no wildcard, only a directory name.
  (if (and ls-lisp-support-shell-wildcards
	   (string-match "[[?*]" file))
      (progn
	(or (not (eq (aref file (1- (length file))) ?/))
	    (setq file (substring file 0 (1- (length file)))))
	(setq wildcard t)))
  (if (or wildcard full-directory-p)
      (let* ((dir (file-name-as-directory file))
	     (default-directory dir)	; so that file-attributes works
	     (file-alist
	      (directory-files-and-attributes dir nil wildcard t))
	     (now (current-time))
	     (sum 0)
	     ;; do all bindings here for speed
	     total-line files elt short file-size fil attr)
	(cond ((memq ?A switches)
	       (setq file-alist
		     (ls-lisp-delete-matching "^\\.\\.?$" file-alist)))
	      ((not (memq ?a switches))
	       ;; if neither -A  nor -a, flush . files
	       (setq file-alist
		     (ls-lisp-delete-matching "^\\." file-alist))))
	(setq file-alist
	      (ls-lisp-handle-switches file-alist switches))
	(if (memq ?C switches)		; column (-C) format
	    (ls-lisp-column-format file-alist)
	  (setq total-line (cons (point) (car-safe file-alist)))
	  (setq files file-alist)
	  (while files			; long (-l) format
	    (setq elt (car files)
		  files (cdr files)
		  short (car elt)
		  attr (cdr elt)
		  file-size (nth 7 attr))
	    (and attr
		 (setq sum (+ file-size
			      ;; Even if neither SUM nor file's size
			      ;; overflow, their sum could.
			      (if (or (< sum (- 134217727 file-size))
				      (floatp sum)
				      (floatp file-size))
				  sum
				(float sum))))
		 (insert (ls-lisp-format short attr file-size
					 switches time-index now))))
	  ;; Insert total size of all files:
	  (save-excursion
	    (goto-char (car total-line))
	    (or (cdr total-line)
		;; Shell says ``No match'' if no files match
		;; the wildcard; let's say something similar.
		(insert "(No match)\n"))
	    (insert (format "total %.0f\n" (fceiling (/ sum 1024.0))))))
	(if (memq ?R switches)
	    ;; List the contents of all directories recursively.
	    ;; cadr of each element of `file-alist' is t for
	    ;; directory, string (name linked to) for symbolic
	    ;; link, or nil.
	    (while file-alist
	      (setq elt (car file-alist)
		    file-alist (cdr file-alist))
	      (when (and (eq (cadr elt) t) ; directory
			 (not (string-match "\\`\\.\\.?\\'" (car elt))))
		(setq elt (expand-file-name (car elt) dir))
		(insert "\n" elt ":\n")
		(ls-lisp-insert-directory
		 elt switches time-index wildcard full-directory-p)))))
    ;; If not full-directory-p, FILE *must not* end in /, as
    ;; file-attributes will not recognize a symlink to a directory,
    ;; so must make it a relative filename as ls does:
    (if (eq (aref file (1- (length file))) ?/)
	(setq file (substring file 0 -1)))
    (let ((fattr (file-attributes file)))
      (if fattr
          (progn
            (if (memq ?F switches)      ; classify switch
                (let (filedata)
                  (setq filedata (ls-lisp-classify (cons file fattr)))
                  (setq file (car filedata))))
            (insert (ls-lisp-format file fattr (nth 7 fattr)
                                    switches time-index (current-time))))
	(message "%s: doesn't exist or is inaccessible" file)
	(ding) (sit-for 2)))))

(provide 'ls-lisp-bugfix)

;;; save-history.el --- Save mini-buffer histories between sessions

;; Copyright (C) 2000, 2002 by Lars R. Clausen <lrclause@cs.uiuc.edu>>

;; Author: Lars R. Clausen <lrclause@cs.uiuc.edu>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; --------------------------------------------------------------------- ;;
;; Hook-functions to load and save minibuffer histories between          ;;
;; sessions.                                                             ;;
;;                                                                       ;;
;; The package `save-history' saves minibuffer histories when Emacs      ;;
;; closes and loads them at start-up.  The histories can be truncated,   ;;
;; and specific histories may be added.  The fancy thing would be to     ;;
;; have read-from-minibuffer tell save-history which histories have been ;;
;; used, but this will do for now.  I've picked all the -history         ;;
;; variables I could find as a default.                                  ;;
;; --------------------------------------------------------------------- ;;

;; Commentary (Kal Hodgson 2002):

;; Great package Lars!  I find this really useful while debugging
;; complex .emacs files. I hacked the following changes:

;; 1. fixed problem with saving window event
;; 2. fixed problem with saving undefined variables
;; 3. made save-history-varlist customizable

;;; Code:

(defcustom  save-history-varlist
   '(
;;  name-history
;;    coding-system-history
    command-history
    extended-command-history
    file-name-history
;;    buffer-name-history
;;    frame-name-history
    ssh-history
    minibuffer-history
    query-replace-history
;;    read-expression-history
    read-library-history
    regexp-history
    set-variable-value-history
    shell-command-history
    )
  "A list of variables that should be saved by `save-history-save'."
  :type '(repeat (choice (variable-item name-history)
			 (variable-item coding-system-history)
			 (variable-item command-history)
			 (variable-item extended-command-history)
			 (variable-item file-name-history)
			 (variable-item buffer-name-history)
			 (variable-item frame-name-history)
			 (variable-item ssh-history)
			 (variable-item minibuffer-history)
			 (variable-item query-replace-history)
			 (variable-item read-expression-history)
			 (variable-item read-library-history)
			 (variable-item regexp-history)
			 (variable-item set-variable-value-history)
			 (variable-item shell-command-history)
			 ))
  )

;; Make sure each var has been defined at some point.
(let ((copy save-history-varlist))
  (while (setq var (car copy))
    (defvar var nil)
    (setq copy (cdr copy))))

(defvar save-history-max-length 20
  "The maximum number of items that is saved for each history.
If this is nil, there is no limit.")

(defvar save-history-file
  "~/.emacs-histories"
  "The file in which minibuffer histories are saved.")

;; This function taken from desktop.el
;; We can be destructive because we're exiting when called anyway.
(defun save-history-truncate-list (l n)
  "Truncate LIST to at most N elements destructively and return LIST."
  (let ((here (nthcdr (1- n) l)))
    (if (consp here)
	(setcdr here nil))
    l))

(defun save-history-save ()
  "Save all histories in `save-history-varlist' to `save-history-file'"
  (interactive)
  (let ((old-buffer (current-buffer))
	(histbuffer (find-file-noselect save-history-file t t)))
    (switch-to-buffer histbuffer)

    (make-variable-buffer-local 'backup-inhibited)
    (setq backup-inhibited t)

    (if (> (buffer-size) 0)
	(delete-region 1 (buffer-size)))
    (insert "(setq\n")
    (mapcar (lambda (x)
	      (if (and x
		       ;; Catch undefined x. If we don't do this we
		       ;; get situations where attempting to exit
		       ;; emacs generates an error (Kal).
		       (condition-case nil (eval x) (error nil)))
		  (progn
		    (insert "  ")
		    (prin1 x histbuffer)
		    (insert "\n    '")
		    (if save-history-max-length
			(let ((truncated (save-history-truncate-list
					  (eval x)
					  save-history-max-length)))
			  (prin1 truncated histbuffer))
		      (prin1 (eval x) histbuffer))
		    (insert "\n"))))
	    save-history-varlist)
    (insert ")\n")

    ;; delete elements of the command history that contain "#"
    ;; characters --- these are usually mouse or window events which
    ;; we don't want to save (Kal).
    (goto-char (point-min))
    (while (search-forward "#" nil t)
      (re-search-backward ")\\|'" nil t) ;; catch first in list
      (forward-char 2) ;; there's always a space between elements
      (kill-sexp 1)) ;; kill it

    (basic-save-buffer)
    (switch-to-buffer old-buffer)))

(defun save-history-load ()
  "Load histories from `save-history-file'"
  (if (file-exists-p save-history-file)
      (load-file save-history-file)))

(add-hook 'after-init-hook 'save-history-load)
(add-hook 'kill-emacs-hook 'save-history-save)

(provide 'save-history)
;;; save-history.el ends here

;;; Local Variables:
;;; mirror-update-on-save: t
;;; mirror-file-path: "~/public_html/emacs/save-history.el"
;;; auto-recompile: t
;;; End:

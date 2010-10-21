;;; dired-rename-commands.el -- more dired commands

;; Author: Tom Wurgler <twurgler@goodyear.com>
;; Created: 10/18/2000
;; Keywords: dired, rename, copy

;
; $Id: dired-rename-commands.el,v 1.9 2001-07-12 09:17:02-04 wurgler Exp $
;
; $Log: dired-rename-commands.el,v $
; Revision 1.9  2001-07-12 09:17:02-04  wurgler
; Changed doc a bit
;
; Revision 1.8  2001-06-13 08:14:11-04  wurgler
; took out the *display* of the to history.  still there with esc-p but now allows
; for a blank string that just removes the from string from the names.
;
; Revision 1.7  2000-10-25 12:55:17-04  wurgler
; text fixes
;
; Revision 1.6  2000-10-25 12:48:45-04  wurgler
; check the number of "*" in the rename-mask.  Max is 9.
;
; Revision 1.5  2000-10-25 12:33:53-04  wurgler
; consolidated code
;
; Revision 1.4  2000-10-24 16:53:41-04  wurgler
; renamed translate-string to dired-ren-translate-string.
;
; Revision 1.3  2000-10-24 16:52:43-04  wurgler
; added the checker to dired-rename-string too...
;
; Revision 1.2  2000-10-24 16:49:26-04  wurgler
; added checking of possible name conflicts before any renames are done.
;
; Revision 1.1  2000-10-24 13:15:28-04  wurgler
; Initial revision
;
;; This file is not part of GNU Emacs.

;; These commands are free software; you can redistribute them and/or modify
;; them under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; These commands are distributed in the hope that they will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; I find using dired-do-rename-regexp to rename files to be very useful.
;; But I hate typing something like  \\(.*\\)name\\(.*\\) and then something
;; like \\2name\\1 as the destination string.

;; Some time ago, Vladimir Lanin wrote a unix c program that let you
;; use "*" as wildcards and then "#1" "#2" etc for each wildcard
;; position in the string (up to 9 of them).  For the example above,
;; one would type *name* and then #2name#1.  I liked this approach.

;; So I wrote this code as a front end to dired-do-rename-regexp to
;; handle Vladimir's syntax. It just parses the "*"s and the "#"s in
;; the from and to strings and then replaces them with the lisp regexp
;; needed and passes those arguments to dired-do-rename-regexp.  Just
;; less typing and lots less chance of errors.

;; I then extended it to a dired-cop-command to copy files in a like fashion
;; using dired-do-copy-regexp.

;;; Code:

(defvar dired-rename-from-history '())
(defvar dired-rename-to-history   '())

(defun dired-ren-command (&optional fstring rstring copy)
  "Emulate the UNIX 'ren' command on marked files.

Use a '*' as a wildcard (up to 9 of them) in the rename mask FSTRING.
Use a '#' for each equivalent position in the rename-to mask RSTRING.

For example, to rename tw1.dat and tw2.dat to dat.1tw and dat.2tw, you
would give a rename mask of 'tw*.*' and a rename-to mask of '#2.#1tw'.

If you just give it a string in the replace-mask, then replace that string
with the new string in the marked filenames.

Called from a program the args are FSTRING, RSTRING, COPY.

This command uses `dired-do-rename-regexp'. If COPY is non-nil, then
use `dired-do-copy-regexp' instead."

  (interactive)
  (if (not (eq major-mode 'dired-mode))
      (error "Not in dired mode")
    (if (not fstring)
 (progn
   (setq fstring
  (read-from-minibuffer
   (concat (if copy "Copy mask" "Rename mask")
    " [" (car dired-rename-from-history) "]: ")
   nil
   nil
   nil
   'dired-rename-from-history))
   (if (string-equal fstring "")
       (setq fstring (nth 0 dired-rename-from-history)))))
    (dired-ren-check-string fstring "#")
    (if (> (dired-ren-count-matches-in-string "*" fstring) 9)
 (error "Too many wildcards in mask (max is 9)."))
    (if (not rstring)
 (setq rstring
       (read-from-minibuffer (concat (if copy "Copy mask" "Rename mask")
         " '" fstring
         "' to"
         ": ")
        nil
        nil
        nil
        'dired-rename-to-history)))
    (dired-ren-check-string rstring "*")
    (setq fstring (dired-ren-translate-string fstring "\\." "%%"))
    (setq fstring (dired-ren-translate-string fstring "%%" "\\."))
    (setq fstring (dired-ren-translate-string fstring "*" "\\(.%%\\)"))
    (setq fstring (dired-ren-translate-string fstring "%%" "*"))
    (setq rstring (dired-ren-translate-string rstring "#" "\\"))
    (dired-check-ren-names fstring rstring)
    (if copy
 (dired-do-copy-regexp fstring rstring)
      (dired-do-rename-regexp fstring rstring))))

(defun dired-cop-command (&optional fstring rstring)
  "Like `dired-ren-command' but copies files instead of renaming them."
  (interactive)
  (dired-ren-command fstring rstring t))

(defun dired-ren-translate-string (string1 string2 string3)
  "Change every occurrence in STRING1 of STRING2 with STRING3."
  (let ((case-fold-search nil))
    (while (string-match string2 string1)
      (if (not (string-equal string3 ""))
   (setq string1 (concat
    (substring string1 0 (match-beginning 0))
    string3
    (substring string1 (match-end 0)))))))
  string1)

(defun dired-check-ren-names (regexp newname)
  "Check for possible name conflicts before doing any renames."
  (let ((oldfiles (dired-get-marked-files))
 (newfiles nil))
    (while oldfiles
      (setq from (car oldfiles))
      (setq new (dired-string-replace-match
   regexp (file-name-nondirectory from) newname))
      (and new
    (if newfiles
        (setq newfiles
       (cons (expand-file-name new (file-name-directory from))
      newfiles))
      (setq newfiles
     (list (expand-file-name new
        (file-name-directory from))))))
      (setq oldfiles (cdr oldfiles)))
    (setq newfiles (sort newfiles 'string-lessp))
    (while newfiles
      (if (or (string-equal (car newfiles) (car (cdr newfiles)))
       (file-exists-p (car newfiles)))
   (error "Name conflict.  No renames done, aborting...")
 (setq newfiles (cdr newfiles))))))
    
(defun dired-ren-check-string (string char)
  (if (string-match char string)
      (error "Can't have %s in this string." char)))

(defun dired-ren-count-matches-in-string (string1 string2)
  (let ((count 0)
 (start 0)
 (pos 0))
    (while (if (setq pos (string-match string1 string2 start))
        (progn
   (setq start (+ pos (length string1)))
   (setq count (1+ count)))))
    count))

(provide 'dired-rename-commands)

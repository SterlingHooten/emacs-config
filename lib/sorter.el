;;; sorter.el --- cycles dired listing between name/date/extension/size

;; Author: Tom Wurgler <twurgler@goodyear.com>   1999-04-27
;;         Emilio Lopes <eclig@gmx.net>          2002-08-02
;; Keywords: dired extensions files

;; sorter.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sorter.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; General
;; =======
;;
;; In a Dired buffer you can press `s' (`dired-sort-toggle-or-edit')
;; to toggle the listing of the files between alphabetical and time
;; order. This library extends this function to allow directory
;; listings sorted by name, by time, by suffix, by size or unsorted
;; with a single keypress. It requires the GNU `ls' program from the
;; `fileutils' package at the standard GNU sites. Make sure that the
;; variable `insert-directory-program' points to it.
;;
;; When reporting or investigating bugs in Dired be aware that this
;; library overrides the definition of some of its functions.
;;
;; Installation
;; ============
;;
;; Put this file in a directory listed in Emacs' `load-path' and add the
;; following line to your ~/.emacs file:
;;
;;    (add-hook 'dired-load-hook (lambda () (require 'sorter)))
;;
;; Pressing `s' in a dired buffer will now toggle between the different
;; sorting modes.
;;
;; You may also want to bind `dired-toggle-hidden' to some key in
;; `dired-mode-map'.

;;; Code:

(require 'dired)

(defvar sorter-sorting-switches "tSUX"
  "String of `ls' switches (single letters) that influence sorting.")

(defvar dired-sort-by-suffix-regexp "^-[^StU]*X[^StU]*$"
  "Regexp recognized by dired to set `by suffix' mode.")

(defvar dired-sort-by-size-regexp "^-[^XtU]*S[^XtU]*$"
  "Regexp recognized by dired to set `by size' mode.")

(defvar dired-sort-unsorted-regexp "^-[^XtS]*U[^XtS]*$"
  "Regexp recognized by dired to set `unsorted' mode.")

(defvar dired-show-hidden-switch "a"
  "Switch used by `ls' to show hidden files.")

;; WARNING! Overrides the original function definition in dired.el 
(defun dired-sort-set-modeline ()
  (setq mode-name
	(let (case-fold-search)
	  (cond ((string-match dired-sort-by-name-regexp dired-actual-switches)
		 "Dired by name")
		((string-match dired-sort-by-date-regexp dired-actual-switches)
		 "Dired by date")
		((string-match dired-sort-by-suffix-regexp dired-actual-switches)
		 "Dired by suffix")
		((string-match dired-sort-by-size-regexp dired-actual-switches)
		 "Dired by size")
                ((string-match dired-sort-unsorted-regexp dired-actual-switches)
                 "Dired unsorted")
		(t
		 (concat "Dired " dired-actual-switches)))))
  (force-mode-line-update))

(defun dired-sort (&optional arg)
  "Sort by name, date, extension, size or none and refresh the Dired buffer.
With a prefix argument you can edit the current listing switches instead."
  (interactive "P")
  (if arg
      (dired-sort-other
       (read-string "ls switches (must contain -l): " dired-actual-switches))
    (let ((c (upcase (char-to-string (read-char "Sort by: Name, daTe, eXtension, Size, Unsorted, Other?")))))
      (cond
       ((string= c "N") (sorter-set-switch ""))
       ((string= c "T") (sorter-set-switch (downcase c)))
       ((string-match "[XSU]" c) (sorter-set-switch c))
       ((string= c "O") (funcall this-command t))
       (t (error "Invalid input")))))
  (dired-sort-other dired-actual-switches))

(defun dired-toggle-hidden ()
  "Toggle showing hidden files."
  (interactive)
  (if (string-match dired-show-hidden-switch dired-actual-switches)
      (sorter-replace-switch dired-show-hidden-switch "")
    (setq dired-actual-switches (concat dired-actual-switches dired-show-hidden-switch)))
  (dired-sort-other dired-actual-switches))

(defun sorter-set-switch (switch)
  (sorter-replace-switch (concat "[" sorter-sorting-switches "+]") switch))

(defun sorter-replace-switch (old new)
  "Replace every occurence of OLD with NEW in `dired-actual-switches'.
If none is found simply add NEW to `dired-actual-switches'."
  (let (case-fold-search)
    (if (string-match old dired-actual-switches)
        (setq dired-actual-switches (replace-regexp-in-string old new dired-actual-switches 'FIXEDCASE 'LITERAL))
      (setq dired-actual-switches (concat dired-actual-switches new)))))

(provide 'sorter)

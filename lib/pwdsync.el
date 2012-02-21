;;; pwdsync --- tracking the shell's working directory using escape sequences

;; Copyright (C) 2004, 2005 Emilio C. Lopes.

;; Author: Emilio Lopes <eclig@gmx.net>
;; Created: 2004-04-16
;; Version: 0.9.1
;; Keywords: processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; If you have not received a copy of the GNU General Public License
;; along with this software, it can be obtained from the GNU Project's
;; World Wide Web server (http://www.gnu.org/copyleft/gpl.html), from
;; its FTP server (ftp://ftp.gnu.org/pub/gnu/GPL), by sending an eletronic
;; mail to this program's maintainer or by writting to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;; If you find this program useful please consider making a donation to
;; the Free Software Foundation. See http://www.fsf.org/help/donate.html
;; (USA), http://www.fsfeurope.org/help/donate.en.html (Europe) or
;; http://fsf.org.in (India) for details on how to accomplish this.
;; In some countries your donation is tax-deductible.

;;; Commentary:

;; This package implements a strategy for tracking the shell's working
;; directory suggested by Olin Shivers in the file "shell.el".  The idea is
;; to make the shell emit the working directory coded in an escape sequence
;; whenever it changes.  A process filter watches for and processes these
;; messages, changing Emacs' notion of the working directory accordingly.
;;
;; Installation
;; ============
;;
;; Put this file ("pwdsync.el") in a directory listed in your `load-path'
;; and byte-compile it.
;;
;; Add the following to your "~/.emacs":
;;
;;    (autoload 'pwdsync-mode "pwdsync"
;;      "Toggle tracking of the shell's working directory using escape sequences." t)
;;    (add-hook 'shell-mode-hook (lambda () (pwdsync-mode 42)))
;;
;; You also have to instruct your shell to emit an escape sequence whenever
;; the working directory changes.  See the documentation of the function
;; `pwdsync-mode' for details.
;;
;; ** ACHTUNG **
;;
;; The escape sequence used in the release 0.9 of this library
;; conflicted with ISO/IEC 2022:1994.  Thanks to Kalle Olavi
;; Niemitalo for pointing this out.
;;
;; If you are updating from that release you'll also have to modify
;; the initialisation file(s) of your shell to use the new escape
;; sequence.  Again, see the documentation of the function
;; `pwdsync-mode' for details.


;;; Code:

;;; User Variables:
(defgroup pwdsync nil
  "Directory tracking in shell buffers"
  :group 'shell)

(defcustom pwdsync-escape-sequence-regexp "\e\\[|pwdsync:\\(.+\\)|"
  "*Regular expression matching the shell's working directory \"messages.\"
This regular expression should have exactly one pair of parenthesis around
the subexpression matching the actual working directory.
If this variable is a cons pair, its car should be a string with a regular
expression and its cdr the number of the parenthesized subexpression
matching the actual working directory."
  :type '(choice regexp
                 (cons regexp integer)))

;;; Functions:
(defun pwdsync-filter (string)
  (let ((regexp (or (car-safe pwdsync-escape-sequence-regexp) pwdsync-escape-sequence-regexp))
        (match-number (or (cdr-safe pwdsync-escape-sequence-regexp) 1)))
    (save-match-data
      (when (string-match regexp string)
        (let ((pmark (process-mark (get-buffer-process (current-buffer))))
              (cwd))
          (save-excursion
            (save-restriction
              (widen)
              (let ((inhibit-field-text-motion t)
                    (buffer-read-only nil))
                (goto-char pmark)
                (when (re-search-backward regexp comint-last-output-start 'noerror)
                  (setq cwd (match-string-no-properties match-number))
                  (delete-region (match-beginning 0) (match-end 0))
                  (shell-process-cd cwd))))))))))

(define-minor-mode pwdsync-mode
  "Toggle tracking of the shell's working directory using escape sequences.
With optional argument ARG, turn directory tracking on iff ARG is positive.

To use this mode you have to instruct your shell to emit a certain escape
sequence whenever the working directory changes.
For GNU Bash add something like the following to your ~/.bashrc (tested with
version 2.05b):
   if [[ \"$EMACS\" == t ]]; then
       PROMPT_COMMAND='echo -ne \"\\e[|pwdsync:$PWD|\"'
   fi

For ZSH (tested with version 4.2.0):
   if [[ \"$EMACS\" == t ]]; then
       chpwd() { print -Pn \"\\e[|pwdsync:$PWD|\"; }
   fi

For TCSH (tested with version 6.13.04):
   if ( $?EMACS ) then
       alias cwdcmd 'echo -n \"\\033[|pwdsync:$cwd|\"'
   endif

Consult the manual of your shell for details.

It's also possible to use a different escape sequence.  Just modify the variable
`pwdsync-escape-sequence-regexp' accordingly."
  :global t
  :group 'pwdsync
  (when (memq 'pwdsync-filter comint-output-filter-functions)
    (remove-hook 'comint-output-filter-functions 'pwdsync-filter))
  (when pwdsync-mode
    (add-hook 'comint-output-filter-functions 'pwdsync-filter 'append)))

(provide 'pwdsync)

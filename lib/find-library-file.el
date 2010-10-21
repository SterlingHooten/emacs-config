;;; find-library-file --- find a library file in `load-path'.
;; Time-stamp: <1998-11-03 22:47:53 ecl>

;; Copyright (C) 1998 Emilio C. Lopes.

;; Author: Emilio Lopes <Emilio.Lopes@Physik.TU-Muenchen.DE>
;; Created: 06 Oct 1998
;; Version: 1.0
;; Keywords: convenience

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

;;; Commentary:

;; General
;; =======
;;
;; This library provides a fast way to find a library file in your
;; `load-path'.
;;
;; Please send bug reports, suggestions, improvements, etc. to the
;; author of this library (see e-mail address above), so that they can
;; be incorporated in future versions.
;;
;;
;; Installation
;; ============
;;
;; Put this file ("find-library-file.el") in a directory listed in your
;; `load-path' and byte-compile it.
;;
;; Add the following to your "~/.emacs":
;;
;;      (autoload 'find-library-file "find-library-file"
;;        "Visit file LIBRARY in `load-path'." t)
;;
;; You might also want to bind `find-library-file' to some key. See the
;; GNU Emacs on-line documentation for details.

;;; Code:

(defun find-library-file (library &optional function nosuffix path interactive-call)
  "Visit file LIBRARY in `load-path'.
That is the same file that `M-x load-library RET LIBRARY RET' would load,
except that byte-compiled files (`*.elc') are not considered.

If optional second argument FUNCTION is non-nil, it should be a function to
be called with the filename of LIBRARY as argument instead of `find-file'.

The remaining optional arguments have the same meaning as in `locate-library'."
  (interactive (list (read-string "Find library: ") nil nil nil t))
  (let ((filename (or (unless nosuffix
                        (locate-library (concat library ".el") t path nil))
                      (locate-library library t path nil))))
    (if filename
        (funcall (if (functionp function) function 'find-file) filename)        
      (if interactive-call
          (error "No library %s in search path" library)
        nil))))

(provide 'find-library-file)

;;; find-library-file.el ends here

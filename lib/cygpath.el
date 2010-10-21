;; Time-stamp: <2003-07-15 11:31:56 Emilio C. Lopes>

;; TODO
;; if interactive called the function should transform the filename-at-point.

;; (thing-at-point 'filename)
;; (ffap-string-at-point)


;; User variables

(defgroup cygpath nil
  "Filename conversion between DOS/Windows and Cygwin formats.
This library does not use Cygwin's \"cygpath\" or any external programm.")

(defcustom cygpath-default-cygdrive-prefix "/cygdrive"
  "Default prefix to access DOS drives within Cygwin.
Should not end with a slash."
  :type 'string
  :group 'cygpath)



;; Functions

(defun cygpath-windows2unix (path &optional cygdrive-prefix)
  "*Convert Windows filename PATH into the Unix (POSIX) form.
If optional argument CYGDRIVE-PREFIX is given, use it instead
of the default \"/cygdrive\"."
  (replace-regexp-in-string
   "\\`\\([a-z]\\):"
   (concat (or cygdrive-prefix cygpath-default-cygdrive-prefix) "/\\1")
   path))

(defun cygpath-unix2windows (path &optional cygdrive-prefix)
  "*Convert Unix filename PATH into the Windows form.
If optional argument CYGDRIVE-PREFIX is given, use it instead
of the default \"/cygdrive\"."
  (replace-regexp-in-string
   (concat "\\`" (or cygdrive-prefix cygpath-default-cygdrive-prefix) "/\\([a-z]\\)/")
   "\\1:/"                            
   path))

(defun cygpath-unix2dos (path &optional cygdrive-prefix)
  "*Like `cygpath-unix2windows', but also converts slashes to backslashes."
  (subst-char-in-string ?/ ?\\ (cygpath-unix2windows path cygdrive-prefix)))

;; (defun cygpath-flip-slashes (path) nil)
;; Flip slashes.
;; If the first slash found in PATH is a forward slash, change all
;; forward slashes to backslashes. Similarly if the first slash found
;; is a backslash.

;; (cygpath-unix2windows (cygpath-windows2unix "e:/usr/share/magic"))
;; (cygpath-unix2dos (cygpath-windows2unix "e:/usr/share/magic"))

(provide 'cygpath)

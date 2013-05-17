'(progn
   (setq server-auth-dir (locate-user-emacs-file "server/"))
   (server-start)
   (setq inhibit-quit t))


;;; sftp.el --- transparent SFTP support for GNU Emacs

;;; Code:

(require 'comint)
(eval-when-compile
  (require 'cl))

;;; User customization variables
(defgroup sftp nil
  "Accessing remote files and directories using FTP
   made as simple and transparent as possible."
  :group 'files
  :group 'comm
  :prefix "sftp-")

(defcustom sftp-program-name "sftp"
  "Name of FTP program to run."
  :group 'sftp
  :type 'string)

(defcustom sftp-program-args nil
  "A list of arguments passed to the FTP program when started."
  :group 'sftp
  :type '(repeat string))


;;; process handling
(defvar sftp-process-startup-hook nil)
(defvar sftp-process-busy nil)
(defvar sftp-command-output ""
  "Output of the last sftp command.")

(defun sftp-get-process (host user)
  "Return an FTP subprocess connected to HOST and logged in as USER.
Create a new process if needed."
  (let* ((name (sftp-ftp-process-buffer host user))
	 (proc (get-process name)))
    (if (and proc (memq (process-status proc) '(run open)))
	proc
      ;; Must delete dead process so that new process can reuse the name.
      (if proc (delete-process proc))

      ;; grab a suitable process.
      (setq proc (sftp-start-process host user name))

      ;; use `sftp-expand-dir' to cache the current working directory
      ;; as the "home" directory since SFTP can not handle `~' or `~user'.
      (sftp-expand-dir host user "~")

      ;; Run any user-specified hooks.  Note that proc, host and user are
      ;; dynamically bound at this point.
      (let ((sftp-this-user user)
            (sftp-this-host host))
        (run-hooks 'sftp-process-startup-hook))
      proc)))

(defun sftp-start-process (host user name)
  "Spawn a new FTP process ready to connect to machine HOST and give it NAME."
  (let* ((cmd (append (list sftp-program-name) sftp-program-args (list (format "%s@%s" user host))))
	 ;; Without the following binding, sftp-start-process
	 ;; recurses on file-accessible-directory-p, since it needs to
	 ;; restart its process in order to determine anything about
	 ;; default-directory.
	 (file-name-handler-alist)
	 (default-directory
	   (if (file-accessible-directory-p default-directory)
	       default-directory
	     exec-directory))
	 proc)
    ;; It would be nice to make process-connection-type nil,
    ;; but that doesn't work: ftp never responds.
    ;; Can anyone find a fix for that?
    (let ((process-connection-type t)
	  (buffer (get-buffer-create name)))
      (with-current-buffer buffer
	(internal-sftp-mode))
      (setq proc (apply 'start-process name name cmd)))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (set-marker (process-mark proc) (point)))
    (set-process-query-on-exit-flag proc nil)
    (set-process-filter proc 'sftp-process-filter)
    (accept-process-output proc)	; wait for ftp startup message
    proc))

(defun sftp-process-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (process-mark proc))
      (insert string)
      (set-marker (process-mark proc) (point))

      ;; no need to save match data here, Emacs does this
      ;; automatically for process filters
      (when (string-match comint-prompt-regexp string)
        (setq string (substring string 0 (match-beginning 0)))
        (setq sftp-process-busy nil))

      (setq sftp-command-output (concat sftp-command-output string)))))

(defun sftp-send-cmd (proc cmd &optional nowait)
  "Send the given SFTP CMD to the SFTP process PROC.
Return the output of CMD as a string.
If NOWAIT is given then the routine will return `nil' immediately."
  (if (memq (process-status proc) '(run open))
      (with-current-buffer (process-buffer proc)
        (setq sftp-process-busy t)
        (setq sftp-command-output "")
        (setq cmd (concat cmd "\n"))
	(goto-char (point-max))
        (insert cmd)
	(set-marker (process-mark proc) (point))
	(process-send-string proc cmd)
	(if nowait
	    nil
          ;; the process-filter sets `sftp-process-busy' to nil when
          ;; it sees the sftp prompt
          (while sftp-process-busy
            (accept-process-output nil 1))

          sftp-command-output))))


;; (sftp-send-cmd (sftp-get-process "eas254.muc" "eas254") "pwd")
;; (sftp-send-cmd (sftp-get-process "eas254.muc" "eas254") "dir")

(ert-deftest sftp-test-send-cmd ()
  "Test `sftp-send-cmd'."
  (let ((proc (sftp-get-process "eas254.muc" "eas254")))
    (should (equal (sftp-send-cmd proc "pwd")  "Remote working directory: /home/eas254\n"))))


;;; parsing of ls output
(defun sftp-ls (file parse &optional no-error wildcard)
  "Return the output of an `ls' command done over SFTP.
FILE is the full name of the remote file and PARSE specifies that
the output should be parsed and stored away in an internal cache."
  ;; If parse is t, we assume that file is a directory. i.e. we only parse
  ;; full directory listings.
  (let ((sftp-this-file (sftp-expand-file-name file)))
    (destructuring-bind (&optional host user name) (sftp-parse-remote-filename sftp-this-file)
      (if host
          (let ((key (directory-file-name sftp-this-file))
                result
                lscmd parse-func)
            (if (string-equal name "")
                (setq name
                      (sftp-real-file-name-as-directory (sftp-expand-dir host user "~"))))
            (if wildcard
                (progn
                  (sftp-cd host user (file-name-directory name))
                  (setq lscmd (format "ls \"%s\"" file)))
              (setq lscmd (format "ls \"%s\"" name)))
            (if (setq result (sftp-send-cmd (sftp-get-process host user) lscmd))
                (with-temp-buffer 
                  (erase-buffer)
                  (insert result)
                  (goto-char (point-min))
                  (if parse
                      (sftp-set-files sftp-this-file (sftp-parse-dir-listing)))
                  result)
              ;; FIXME: error handling
              (if no-error
                  nil
                (sftp-error host user
                            (concat "DIR failed: " result)))))
        (error "Should never happen. Please report. Bug ref. no.: 1")))))

(defvar sftp-files-hashtable (make-hash-table :test 'equal :size 97)
  "Hash table for storing directories and their respective files.")

(defun sftp-set-files (directory files)
  "For a given DIRECTORY, set or change the associated FILES hashtable."
  (and files
       (puthash (file-name-as-directory directory) files sftp-files-hashtable)))

(defun sftp-get-files (directory &optional no-error)
  "Given a DIRECTORY, return a hashtable of file entries.
This will give an error or return nil, depending on the value of
NO-ERROR, if a listing for DIRECTORY cannot be obtained."
  (setq directory (file-name-as-directory directory)) ; normalize
  (or (gethash directory sftp-files-hashtable)
      (save-match-data
	(and (sftp-ls directory t no-error)
	     (gethash directory sftp-files-hashtable)))))

(defun sftp-parse-dir-listing ()
  (save-match-data
    (and (re-search-forward directory-listing-before-filename-regexp nil t)
         (sftp-ls-parser))))

(defvar sftp-executable-regexp
  ;; adapted from `dired-re-exe'.
  "-[-r][-w][xs][-r][-w].[-r][-w].\\|-[-r][-w].[-r][-w][xs][-r][-w].\\|-[-r][-w].[-r][-w].[-r][-w][xst]"
  "Regular expression matching the `ls' permission string of an executable file.")

(defun sftp-ls-parser ()
  ;; Meant to be called by sftp-parse-dir-listing
  (let ((table (make-hash-table :test 'equal))
        file)
    (while (setq file (sftp-ls-parse-this-filename))
      (skip-chars-forward "\t 0-9")
      (let* ((file-type (following-char))
             (directoryp  (eq file-type ?d))
             (symlinkp    (eq file-type ?l))
             (socketp     (eq file-type ?s))
             (executablep (looking-at sftp-executable-regexp)))
        (when (or (and symlinkp    (string-match-p "@\\'" file))
                  (and directoryp  (string-match-p "/\\'" file))
                  (and executablep (string-match-p "*\\'" file))
                  (and socketp     (string-match-p "=\\'" file)))
          (setq file (substring file 0 -1)))
        (puthash file (or symlinkp directoryp) table))
      (forward-line 1))

    (puthash "."  t table)
    (puthash ".." t table)

    table))

(defmacro sftp-ls-parse-this-filename ()
  "Extract the filename from the current line of a SFTP `ls' listing."
  `(save-match-data
     (let ((eol (point-at-eol)))
       (beginning-of-line)
       (if (re-search-forward directory-listing-before-filename-regexp eol t)
	   (buffer-substring (point) eol)))))

'(progn
   (with-temp-buffer
     (insert (sftp-send-cmd (sftp-get-process "eas254.muc" "eas254") "ls"))
     (goto-char (point-min))
     (sftp-parse-dir-listing))
   )

;;; filename manipulation
(defvar sftp-remote-filename-format
  '("\\`/sftp:\\(\\([^/:]*\\)@\\)?\\([^@/:]*[^@/:.]\\):\\(.*\\)" . (3 2 4))
  "Format of a fully expanded remote file name.
This is a list of the form \(REGEXP HOST USER NAME\),
where REGEXP is a regular expression matching
the full remote name, and HOST, USER, and NAME are the numbers of
parenthesized expressions in REGEXP for the components (in that order).")

(defun sftp-parse-remote-filename (filename)
  "Parse FILENAME as a remote filename and return the list (HOST USER FNAME).
Return nil if FILENAME does not match `sftp-remote-filename-format'."
  (save-match-data
    (destructuring-bind (regexp hostn usern fnamen) sftp-remote-filename-format
      (if (posix-string-match regexp filename)
          (let ((host  (match-string hostn filename))
                (user  (match-string usern filename))
                (fname (match-string fnamen filename)))
            (if (zerop (length user))
                (setq user (sftp-get-user host)))
            (list host user fname))
        nil))))

(ert-deftest sftp-test-parse-remote-filename ()
  "Test `sftp-parse-remote-filename'."
  (should (equal (sftp-parse-remote-filename "/sftp:eas254@eas254.muc:foo")
                 '("eas254.muc" "eas254" "foo")))
  (should (equal (sftp-parse-remote-filename "/sftp:eas254@eas254.muc:/tmp/bar")
                 '("eas254.muc" "eas254" "/tmp/bar")))
  (should-error (sftp-parse-remote-filename "/sftp:eas254.muc:/tmp/bar")))


(defun sftp-expand-file-name (name &optional default)
  "See `expand-file-name'."
  (save-match-data
    (setq default (or default default-directory))
    (cond ((eq (string-to-char name) ?~)
	   (sftp-real-expand-file-name name))
	  ((eq (string-to-char name) ?/)
	   (sftp-canonicalise-filename name))
	  ((and (eq system-type 'windows-nt)
		(eq (string-to-char name) ?\\))
	   (sftp-canonicalise-filename name))
	  ((and (eq system-type 'windows-nt)
		(or (string-match "\\`[a-zA-Z]:" name)
		    (string-match "\\`[a-zA-Z]:" default)))
	   (sftp-real-expand-file-name name default))
	  ((zerop (length name))
	   (sftp-canonicalise-filename default))
	  ((sftp-canonicalise-filename
	    (concat (file-name-as-directory default) name))))))

(defun sftp-canonicalise-filename (filename)
  "Take a string FILENAME and short-circuit //, /. and /.."
  (destructuring-bind (&optional host user fname) (sftp-parse-remote-filename filename)
    (if host
        ;; this is a remote filename
        (save-match-data
          (cond
           ;; if remote name is absolute, we are ready
           ((string-match-p "\\`/" fname)
            t)

           ;; Name starts with ~ or ~user.  Resolve that part of the
           ;; name, making it absolute
           ((string-match "\\`~[^/]*" fname)
            (let* ((tilda (match-string 0 fname))
                   (rest (substring fname (match-end 0)))
                   (dir (sftp-expand-dir host user tilda)))
              (if dir
                  ;; C-x d /ftp:anonymous@ftp.gnu.org:~/ RET
                  ;; seems to cause `rest' to sometimes be empty.
                  ;; Maybe it's an error for `rest' to be empty here,
                  ;; but until we figure this out, this quick fix
                  ;; seems to do the trick.
                  (setq fname (cond ((string-equal rest "") dir)
                                    ((string-equal dir "/") rest)
                                    (t (concat dir rest))))
                (error "User \"%s\" is not known"
                       (substring tilda 1)))))

           ;; non-absolute filename, assume it's relative to the home dir
           (t
            (let ((dir (sftp-expand-dir host user "~")))
              (if dir
                  (setq fname (concat (sftp-real-file-name-as-directory dir) fname))
                (error "Unable to determine home directory")))))

          ;; At this point, FNAME is an (remote) absolute filename.
          ;; Now use the real `expand-file-name' to resolve path
          ;; components like `.' and `..'.  This works because
          ;; `expand-file-name' does not consult the file system when
          ;; performing these simplifications.  On Windows systems
          ;; take care of its "intricacies" before proceeding.
          (unless (string-match "\\`//" fname)
            (if (not (memq system-type '(ms-dos windows-nt)))
                (setq fname (sftp-real-expand-file-name fname))
              ;; Windows UNC default dirs do not make sense for ftp.
              (setq fname (if (and default-directory
                                   (string-match "\\`//" default-directory))
                              (sftp-real-expand-file-name fname "c:/")
                            (sftp-real-expand-file-name fname)))
              ;; Strip off possible drive specifier.
              (if (string-match "\\`[a-zA-Z]:" fname)
                  (setq fname (substring fname 2))))
            (if (string-match "\\`//" fname)
                (setq fname (substring fname 1))))

          (sftp-replace-name-component filename fname))

      ;; local filename, just expand it normally
      (if (eq (string-to-char filename) ?/)
          (sftp-real-expand-file-name filename)
        (sftp-real-expand-file-name
         (sftp-real-file-name-nondirectory filename)
         (sftp-real-file-name-directory filename))))))

;; (sftp-canonicalise-filename "/sftp:eas254@eas254.muc:~bar/foo")
;; (sftp-canonicalise-filename "/sftp:eas254@eas254.muc:~eas254")
;; (sftp-canonicalise-filename "/sftp:eas254@eas254.muc:~/")

(ert-deftest sftp-test-canonicalise-filename ()
  "Test `sftp-canonicalise-filename'."
  (should-error (sftp-canonicalise-filename "/sftp:eas254@eas254.muc:~bar/foo"))
  (should (equal (sftp-canonicalise-filename "/sftp:eas254@eas254.muc:~eas254") 
                 "/sftp:eas254@eas254.muc:/home/eas254"))
  (should (equal (sftp-canonicalise-filename "/sftp:eas254@eas254.muc:~/") 
                 "/sftp:eas254@eas254.muc:/home/eas254")))


(defun sftp-replace-name-component (fullname name)
  "Replace the filename component of FULLNAME with NAME.
FULLNAME should match `sftp-remote-filename-format'."
  (save-match-data
    (if (posix-string-match (car sftp-remote-filename-format) fullname)
	(let* ((ns (cdr sftp-remote-filename-format))
	       (elt (nth 2 ns)))
	  (concat (substring fullname 0 (match-beginning elt))
		  name
		  (substring fullname (match-end elt)))))))

(defconst sftp-expand-dir-hashtable (make-hash-table :test 'equal))

(defun sftp-expand-dir (host user dir)
  "Return the result of a PWD in the current SFTP session after cd'ing to directory DIR.
Use the connection to machine HOST logged in as user USER."
  (let* ((key (concat host "/" user "/" dir))
         (result (gethash key sftp-expand-dir-hashtable)))
    (unless result
      (cond
       ((string-equal dir (format "~%s" user))
        (setq result (sftp-expand-dir host user "~")))
       ((string-equal dir "~")
        ;; sftp's CD does not accept `~' or `~user' as an argument, so
        ;; just return the CWD.
        (setq result (sftp-get-cwd host user)))
       (t
        (let ((current (sftp-get-cwd host user)))
          (unwind-protect
              (and (sftp-cd host user dir)
                   (setq result (sftp-get-cwd host user)))
            (sftp-cd host user current))))))
    (when result
      (puthash key result sftp-expand-dir-hashtable))
    result))

;; (sftp-expand-dir "eas254.muc" "eas254" "~")

(defun sftp-get-cwd (host user)
  "Attempt to get the current working directory for the given HOST/USER pair.
Returns DIR, the current working directory, or nil if not found."
  (let ((result (sftp-send-cmd (sftp-get-process host user) "pwd")))
    (and (stringp result)
	(save-match-data
	  (and (string-match ": \\(.+\\)$" result)
	       (match-string 1 result))))))

(defun sftp-cd (host user dir &optional noerror)
  (let ((result (sftp-send-cmd (sftp-get-process host user) (format "cd \"%s\"" dir))))
    (or (and (stringp result)
             (zerop (length result)))
        noerror
        (sftp-error host user (concat "CD failed: " result)))))


(define-derived-mode internal-sftp-mode comint-mode "Internal sftp"
  "Major mode for interacting with the FTP process.

\\{comint-mode-map}"
  (set (make-local-variable 'sftp-process-busy) nil)
  (setq comint-prompt-regexp "^sftp> ")
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp))

;; Return the name of the buffer that collects output from the ftp process
;; connected to the given HOST and USER pair.
(defun sftp-ftp-process-buffer (host user)
  (concat "*sftp " user "@" host "*"))


(defun sftp-file-name-directory (filename)
  "See `file-name-directory'."
  (destructuring-bind (&optional host user fname) (sftp-parse-remote-filename filename)
    (if host
	(if (string-match-p "\\`~[^/]*\\'" fname)
            filename
          (sftp-replace-name-component filename (sftp-real-file-name-directory fname)))
      (sftp-real-file-name-directory filename))))


;;; Define ways of getting at unmodified Emacs primitives,
;;; turning off our handler.

(defun sftp-run-real-handler (operation args)
 (let ((inhibit-file-name-handlers
	 (cons 'sftp-hook-function
	       (cons 'sftp-completion-hook-function
		     (and (eq inhibit-file-name-operation operation)
			  inhibit-file-name-handlers))))
	(inhibit-file-name-operation operation))
   (tramp-run-real-handler operation args)))

(defun sftp-real-file-name-directory (&rest args)
  (sftp-run-real-handler 'file-name-directory args))
(defun sftp-real-file-name-nondirectory (&rest args)
  (sftp-run-real-handler 'file-name-nondirectory args))
(defun sftp-real-file-name-as-directory (&rest args)
  (sftp-run-real-handler 'file-name-as-directory args))
(defun sftp-real-expand-file-name (&rest args)
  (sftp-run-real-handler 'expand-file-name args))


(provide 'sftp)
;;; sftp.el ends here

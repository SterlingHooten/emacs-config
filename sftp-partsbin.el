;; (defun sftp-hook-function (operation &rest args)
;;   (error "foo"))

(progn
  (setq server-auth-dir (locate-user-emacs-file "server/"))
  (server-start)
  (setq inhibit-quit t))

'(progn

   (let ((edebug-save-windows nil)
         (file-name-handler-alist
          (cons '("\\`/sftp:" . sftp-hook-function)
                file-name-handler-alist)))
     ;; (find-file-name-handler "/sftp:eas254@eas254.muc:foo" 'insert-file-contents)
     (file-exists-p "/sftp:eas254@eas254.muc:pipes.sh")
     ;; (file-name-as-directory "/sftp:eas254@eas254.muc:foo")
     ;; (expand-file-name "/sftp:eas254@eas254.muc:foo")
     )

   (setq sftp-ftp-name-arg "")
   (setq sftp-ftp-name-res nil)
   (setq sftp-ls-cache-file nil)
   (setq sftp-ls-cache-res nil)

   (sftp-ftp-name "/sftp:eas254@eas254.muc:foo")
   (sftp-ftp-name "/sftp:eas254@eas254.muc:/tmp/bar")
   )


;;; sftp.el --- transparent FTP support for GNU Emacs

;;; Code:

(require 'comint)

;;;; ------------------------------------------------------------
;;;; User customization variables.
;;;; ------------------------------------------------------------

(defgroup sftp nil
  "Accessing remote files and directories using FTP
   made as simple and transparent as possible."
  :group 'files
  :group 'comm
  :prefix "sftp-")

(defcustom sftp-name-format
  '("\\`/sftp:\\(\\([^/:]*\\)@\\)?\\([^@/:]*[^@/:.]\\):\\(.*\\)" . (3 2 4))
  "Format of a fully expanded remote file name.

This is a list of the form \(REGEXP HOST USER NAME\),
where REGEXP is a regular expression matching
the full remote name, and HOST, USER, and NAME are the numbers of
parenthesized expressions in REGEXP for the components (in that order)."
  :group 'sftp
  :type '(list (regexp  :tag "Name regexp")
	       (integer :tag "Host group")
	       (integer :tag "User group")
	       (integer :tag "Name group")))

;; sftp-multi-skip-msgs should only match ###-, where ### is one of
;; the number codes corresponding to sftp-good-msgs or sftp-fatal-msgs.
;; Otherwise, sftp will go into multi-skip mode, and never come out.

(defcustom sftp-tmp-name-template
  (expand-file-name "sftp" temporary-file-directory)
  "Template used to create temporary files."
  :group 'sftp
  :type 'directory)

(defcustom sftp-gateway-tmp-name-template "/tmp/sftp"
  "Template used to create temporary files when FTP-ing through a gateway.

Files starting with this prefix need to be accessible from BOTH the local
machine and the gateway machine, and need to have the SAME name on both
machines, that is, /tmp is probably NOT what you want, since that is rarely
cross-mounted."
  :group 'sftp
  :type 'directory)

(defcustom sftp-netrc-filename "~/.netrc"
  "File in .netrc format to search for passwords."
  :group 'sftp
  :type 'file)

(defcustom sftp-disable-netrc-security-check (eq system-type 'windows-nt)
  "If non-nil avoid checking permissions on the .netrc file."
  :group 'sftp
  :type 'boolean)

(defcustom sftp-default-user nil
  "User name to use when none is specified in a file name.

If non-nil but not a string, you are prompted for the name.
If nil, the value of `sftp-netrc-default-user' is used.
If that is nil too, then your login name is used.

Once a connection to a given host has been initiated, the user name
and password information for that host are cached and re-used by
sftp.  Use \\[sftp-set-user] to change the cached values,
since setting `sftp-default-user' directly does not affect
the cached information."
  :group 'sftp
  :type '(choice (const :tag "Default" nil)
		 string
		 (other :tag "Prompt" t)))

(defcustom sftp-netrc-default-user nil
  "Alternate default user name to use when none is specified.

This variable is set from the `default' command in your `.netrc' file,
if there is one."
  :group 'sftp
  :type '(choice (const :tag "Default" nil)
		 string))

(defcustom sftp-default-password nil
  "Password to use when the user name equals `sftp-default-user'."
  :group 'sftp
  :type '(choice (const :tag "Default" nil)
		 string))

(defcustom sftp-default-account nil
  "Account to use when the user name equals `sftp-default-user'."
  :group 'sftp
  :type '(choice (const :tag "Default" nil)
		 string))

(defcustom sftp-netrc-default-password nil
  "Password to use when the user name equals `sftp-netrc-default-user'."
  :group 'sftp
  :type '(choice (const :tag "Default" nil)
		 string))

(defcustom sftp-netrc-default-account nil
  "Account to use when the user name equals `sftp-netrc-default-user'."
  :group 'sftp
  :type '(choice (const :tag "Default" nil)
		 string))

(defcustom sftp-gateway-host nil
  "Name of host to use as gateway machine when local FTP isn't possible."
  :group 'sftp
  :type '(choice (const :tag "Default" nil)
		 string))

(defcustom sftp-local-host-regexp ".*"
  "Regexp selecting hosts which can be reached directly with FTP.

For other hosts the FTP process is started on `sftp-gateway-host'
instead, and/or reached via `sftp-gateway-ftp-program-name'."
  :group 'sftp
  :type 'regexp)

(defcustom sftp-gateway-program-interactive nil
  "If non-nil then the gateway program should give a shell prompt.

Both telnet and rlogin do something like this."
  :group 'sftp
  :type 'boolean)

(defcustom sftp-gateway-program remote-shell-program
  "Name of program to spawn a shell on the gateway machine.

Valid candidates are rsh (remsh on some systems), telnet and rlogin.
See also the gateway variable above."
  :group 'sftp
  :type '(choice (const "rsh")
		 (const "telnet")
		 (const "rlogin")
		 string))

(defcustom sftp-gateway-prompt-pattern "^[^#$%>;\n]*[#$%>;] *"
  "Regexp matching prompt after complete login sequence on gateway machine.

A match for this means the shell is now awaiting input.  Make this regexp as
strict as possible; it shouldn't match *anything* at all except the user's
initial prompt.  The above string will fail under most SUN-3's since it
matches the login banner."
  :group 'sftp
  :type 'regexp)

(defvar sftp-gateway-setup-term-command
  (if (eq system-type 'hpux)
      "stty -onlcr -echo\n"
    "stty -echo nl\n")
  "*Set up terminal after logging in to the gateway machine.
This command should stop the terminal from echoing each command, and
arrange to strip out trailing ^M characters.")

(defcustom sftp-process-verbose t
  "If non-nil then be chatty about interaction with the FTP process."
  :group 'sftp
  :type 'boolean)

(defcustom sftp-ftp-program-name "sftp"
  "Name of FTP program to run."
  :group 'sftp
  :type 'string)

(defcustom sftp-gateway-ftp-program-name "sftp"
  "Name of FTP program to run when accessing non-local hosts."
  :group 'sftp
  :type 'string)

(defcustom sftp-ftp-program-args nil
  "A list of arguments passed to the FTP program when started."
  :group 'sftp
  :type '(repeat string))

(defcustom sftp-make-backup-files ()
  "Non-nil means make backup files for \"magic\" remote files."
  :group 'sftp
  :type 'boolean)

(defcustom sftp-retry-time 5
  "Number of seconds to wait before retry if file or listing doesn't arrive.
This might need to be increased for very slow connections."
  :group 'sftp
  :type 'integer)

(defcustom sftp-auto-save 0
  "If 1, allow sftp files to be auto-saved.
If 0, inhibit auto-saving of sftp files.
Don't use any other value."
  :group 'sftp
  :type '(choice (const :tag "Suppress" 0)
		 (const :tag "Allow" 1)))


;;;; ------------------------------------------------------------
;;;; Hash table support.
;;;; ------------------------------------------------------------

(require 'backquote)

(defun sftp-hash-entry-exists-p (key tbl)
  "Return whether there is an association for KEY in table TBL."
  (and tbl (not (eq (gethash key tbl 'unknown) 'unknown))))

(defun sftp-hash-table-keys (tbl)
  "Return a sorted list of all the active keys in table TBL, as strings."
  ;; (let ((keys nil))
  ;;   (maphash (lambda (k v) (push k keys)) tbl)
  ;;   (sort keys 'string-lessp))
  (sort (all-completions "" tbl) 'string-lessp))

;;;; ------------------------------------------------------------
;;;; Internal variables.
;;;; ------------------------------------------------------------

(defvar sftp-data-buffer-name " *sftp data*"
  "Buffer name to hold directory listing data received from FTP process.")

(defvar sftp-netrc-modtime nil
  "Last modified time of the netrc file from file-attributes.")

(defvar sftp-user-hashtable (make-hash-table :test 'equal)
  "Hash table holding associations between HOST, USER pairs.")

(defvar sftp-passwd-hashtable (make-hash-table :test 'equal)
  "Mapping between a HOST, USER pair and a PASSWORD for them.
All HOST values should be in lower case.")

(defvar sftp-account-hashtable (make-hash-table :test 'equal)
  "Mapping between a HOST, USER pair and an ACCOUNT password for them.")

(defvar sftp-files-hashtable (make-hash-table :test 'equal :size 97)
  "Hash table for storing directories and their respective files.")

(defvar sftp-inodes-hashtable (make-hash-table :test 'equal :size 97)
  "Hash table for storing file names and their \"inode numbers\".")

(defvar sftp-next-inode-number 1
  "Next \"inode number\" value.  We give each file name a unique number.")

(defvar sftp-ls-cache-file nil
  "Last file passed to `sftp-ls'.")

(defvar sftp-ls-cache-res nil
  "Last result returned from `sftp-ls'.")

(defconst sftp-expand-dir-hashtable (make-hash-table :test 'equal))

(defconst sftp-expand-dir-regexp "^5.0 \\([^: ]+\\):")

;; These are local variables in each FTP process buffer.
(defvar sftp-process-string "")
(defvar sftp-process-result-line nil)
(defvar sftp-process-busy nil)
(defvar sftp-process-result t)
(defvar sftp-process-multi-skip nil)
(defvar sftp-process-msg nil)
(defvar sftp-process-continue nil)

;; These variables are bound by one function and examined by another.
;; Leave them void globally for error checking.
(defvar sftp-this-file)
(defvar sftp-this-dir)
(defvar sftp-this-user)
(defvar sftp-this-host)
(defvar sftp-this-msg)
(defvar sftp-completion-ignored-pattern)
(defvar sftp-trample-marker)

;; New error symbols.
(put 'ftp-error 'error-conditions '(ftp-error file-error error))
;; (put 'ftp-error 'error-message "FTP error")

;;; ------------------------------------------------------------
;;; Enhanced message support.
;;; ------------------------------------------------------------

(defun sftp-message (fmt &rest args)
  "Display message in echo area, but indicate if truncated.
Args are as in `message': a format string, plus arguments to be formatted."
  (let ((msg (apply 'format fmt args))
	(max (window-width (minibuffer-window))))
    (if noninteractive
	msg
      (if (>= (length msg) max)
	  ;; Take just the last MAX - 3 chars of the string.
	  (setq msg (concat "> " (substring msg (- 3 max)))))
      (message "%s" msg))))

(defun sftp-abbreviate-filename (file &optional new)
  "Abbreviate the file name FILE relative to the `default-directory'.
If the optional parameter NEW is given and the non-directory parts match,
only return the directory part of FILE."
  (save-match-data
    (if (and default-directory
	     (string-match (concat "\\`"
				   (regexp-quote default-directory)
				   ".") file))
	(setq file (substring file (1- (match-end 0)))))
    (if (and new
	     (string-equal (file-name-nondirectory file)
			   (file-name-nondirectory new)))
	(setq file (file-name-directory file)))
    (or file "./")))

;;;; ------------------------------------------------------------
;;;; User / Host mapping support.
;;;; ------------------------------------------------------------

(defun sftp-set-user (host user)
  "For a given HOST, set or change the default USER."
  (interactive "sHost: \nsUser: ")
  (puthash host user sftp-user-hashtable))

(defun sftp-get-user (host)
  "Given a HOST, return the default user."
  (sftp-parse-netrc)
  (let ((user (gethash host sftp-user-hashtable)))
    (or user
	(prog1
	    (setq user
		  (cond ((stringp sftp-default-user)
			 ;; We have a default name.  Use it.
			 sftp-default-user)
			(sftp-default-user
			 ;; Ask the user.
			 (let ((enable-recursive-minibuffers t))
			   (read-string (format "User for %s: " host)
					(user-login-name))))
			(sftp-netrc-default-user)
			;; Default to the user's login name.
			(t
			 (user-login-name))))
	  (sftp-set-user host user)))))

;;;; ------------------------------------------------------------
;;;; Password support.
;;;; ------------------------------------------------------------

(defmacro sftp-generate-passwd-key (host user)
  `(and (stringp ,host) (stringp ,user) (concat (downcase ,host) "/" ,user)))

(defmacro sftp-lookup-passwd (host user)
  `(gethash (sftp-generate-passwd-key ,host ,user)
	    sftp-passwd-hashtable))

(defun sftp-set-passwd (host user password)
  "For a given HOST and USER, set or change the associated PASSWORD."
  (interactive (list (read-string "Host: ")
		     (read-string "User: ")
		     (read-passwd "Password: ")))
  (puthash (sftp-generate-passwd-key host user)
	   password sftp-passwd-hashtable))

(defun sftp-get-passwd (host user)
  "Return the password for specified HOST and USER, asking user if necessary."
  (sftp-parse-netrc)

  ;; look up password in the hash table first; user might have overridden the
  ;; defaults.
  (cond ((sftp-lookup-passwd host user))

	;; See if default user and password set.
	((and (stringp sftp-default-user)
	      sftp-default-password
	      (string-equal user sftp-default-user))
	 sftp-default-password)

	;; See if default user and password set from .netrc file.
	((and (stringp sftp-netrc-default-user)
	      sftp-netrc-default-password
	      (string-equal user sftp-netrc-default-user))
	 sftp-netrc-default-password)
	
	;; see if same user has logged in to other hosts; if so then prompt
	;; with the password that was used there.
	(t
	 (let* ((other (sftp-get-host-with-passwd user))
		(passwd (read-passwd
                         (format "Password for %s@%s: " user host))))
	   (sftp-set-passwd host user passwd)
	   passwd))))

;;;; ------------------------------------------------------------
;;;; Account support
;;;; ------------------------------------------------------------

;; Account passwords must be either specified in the .netrc file, or set
;; manually by calling sftp-set-account.  For the moment, sftp doesn't
;; check to see whether the FTP process is actually prompting for an account
;; password.

(defun sftp-set-account (host user account)
  "For a given HOST and USER, set or change the associated ACCOUNT password."
  (interactive (list (read-string "Host: ")
		     (read-string "User: ")
		     (read-passwd "Account password: ")))
  (puthash (sftp-generate-passwd-key host user)
	   account sftp-account-hashtable))

(defun sftp-get-account (host user)
  "Given a HOST and USER, return the FTP account."
  (sftp-parse-netrc)
  (or (gethash (sftp-generate-passwd-key host user)
	       sftp-account-hashtable)
      (and (stringp sftp-default-user)
	   (string-equal user sftp-default-user)
	   sftp-default-account)
      (and (stringp sftp-netrc-default-user)
	   (string-equal user sftp-netrc-default-user)
	   sftp-netrc-default-account)))

;;;; ------------------------------------------------------------
;;;; ~/.netrc support
;;;; ------------------------------------------------------------

(defun sftp-chase-symlinks (file)
  "Return the filename that FILE references, following all symbolic links."
  (let (temp)
    (while (setq temp (sftp-real-file-symlink-p file))
      (setq file
	    (if (file-name-absolute-p temp)
		temp
	      ;; Wouldn't `expand-file-name' be better than `concat' ?
	      ;; It would fail when `a/b/..' != `a', tho.  --Stef
	      (concat (file-name-directory file) temp)))))
  file)

;; Move along current line looking for the value of the TOKEN.
;; Valid separators between TOKEN and its value are commas and
;; whitespace.  Second arg LIMIT is a limit for the search.

(defun sftp-parse-netrc-token (token limit)
  (if (search-forward token limit t)
      (let (beg)
	(skip-chars-forward ", \t\r\n" limit)
	(if (eq (following-char) ?\")	;quoted token value
	    (progn (forward-char 1)
		   (setq beg (point))
		   (skip-chars-forward "^\"" limit)
		   (forward-char 1)
		   (buffer-substring beg (1- (point))))
	  (setq beg (point))
	  (skip-chars-forward "^, \t\r\n" limit)
	  (buffer-substring beg (point))))))

;; Extract the values for the tokens `machine', `login',
;; `password' and `account' in the current buffer.  If successful,
;; record the information found.

(defun sftp-parse-netrc-group ()
  (let ((start (point))
	(end (save-excursion
	       (if (looking-at "machine\\>")
		   ;; Skip `machine' and the machine name that follows.
		   (progn
		     (skip-chars-forward "^ \t\r\n")
		     (skip-chars-forward " \t\r\n")
		     (skip-chars-forward "^ \t\r\n"))
		 ;; Skip `default'.
		 (skip-chars-forward "^ \t\r\n"))
	       ;; Find start of the next `machine' or `default'
	       ;; or the end of the buffer.
	       (if (re-search-forward "machine\\>\\|default\\>" nil t)
		   (match-beginning 0)
		 (point-max))))
	machine login password account)
    (setq machine  (sftp-parse-netrc-token "machine"  end)
	  login    (sftp-parse-netrc-token "login"    end)
	  password (sftp-parse-netrc-token "password" end)
	  account  (sftp-parse-netrc-token "account"  end))
    (if (and machine login)
	;; found a `machine` token.
	(progn
	  (sftp-set-user machine login)
	  (sftp-set-passwd machine login password)
	  (and account
	       (sftp-set-account machine login account)))
      (goto-char start)
      (if (search-forward "default" end t)
	  ;; found a `default' token
	  (progn
	    (setq login    (sftp-parse-netrc-token "login"    end)
		  password (sftp-parse-netrc-token "password" end)
		  account  (sftp-parse-netrc-token "account"  end))
	    (and login
		 (setq sftp-netrc-default-user login))
	    (and password
		 (setq sftp-netrc-default-password password))
	    (and account
		 (setq sftp-netrc-default-account account)))))
    (goto-char end)))

;; Read in ~/.netrc, if one exists.  If ~/.netrc file exists and has
;; the correct permissions then extract the \`machine\', \`login\',
;; \`password\' and \`account\' information from within.

(defun sftp-parse-netrc ()
  ;; We set this before actually doing it to avoid the possibility
  ;; of an infinite loop if sftp-netrc-filename is an FTP file.
  (interactive)
  (let (file attr)
    (let ((default-directory "/"))
      (setq file (sftp-chase-symlinks
		  (sftp-real-expand-file-name sftp-netrc-filename)))
      (setq attr (sftp-real-file-attributes file)))
    (if (and attr			; file exists.
	     (not (equal (nth 5 attr) sftp-netrc-modtime)))	; file changed
	(save-match-data
	  (if (or sftp-disable-netrc-security-check
		  (and (eq (nth 2 attr) (user-uid)) ; Same uids.
		       (string-match ".r..------" (nth 8 attr))))
	      (with-current-buffer
		;; we are cheating a bit here.  I'm trying to do the equivalent
		;; of find-file on the .netrc file, but then nuke it afterwards.
		;; with the bit of logic below we should be able to have
		;; encrypted .netrc files.
                  (generate-new-buffer "*ftp-.netrc*")
		(sftp-real-insert-file-contents file)
		(setq buffer-file-name file)
		(setq default-directory (file-name-directory file))
		(normal-mode t)
		(run-hooks 'find-file-hook)
		(setq buffer-file-name nil)
		(goto-char (point-min))
		(while (search-forward-regexp "^[ \t]*#.*$" nil t)
		  (replace-match ""))
		(goto-char (point-min))
		(skip-chars-forward " \t\r\n")
		(while (not (eobp))
		  (sftp-parse-netrc-group))
		(kill-buffer (current-buffer)))
	    (sftp-message "%s either not owned by you or badly protected."
			      sftp-netrc-filename)
	    (sit-for 1))
	  (setq sftp-netrc-modtime (nth 5 attr))))))

;; Return a list of prefixes of the form 'user@host:' to be used when
;; completion is done in the root directory.

(defun sftp-generate-root-prefixes ()
  (sftp-parse-netrc)
  (save-match-data
    (let (res)
      (maphash
       (lambda (key value)
	 (if (string-match "\\`[^/]*\\(/\\).*\\'" key)
	     (let ((host (substring key 0 (match-beginning 1)))
		   (user (substring key (match-end 1))))
	       (push (concat user "@" host ":") res))))
       sftp-passwd-hashtable)
      (maphash
       (lambda (host user) (push (concat host ":") res))
       sftp-user-hashtable)
      (or res (list nil)))))

;;;; ------------------------------------------------------------
;;;; Remote file name syntax support.
;;;; ------------------------------------------------------------

(defmacro sftp-ftp-name-component (n ns name)
  "Extract the Nth FTP file name component from NS."
  `(let ((elt (nth ,n ,ns)))
     (match-string elt ,name)))

(defvar sftp-ftp-name-arg "")
(defvar sftp-ftp-name-res nil)

;; Parse NAME according to `sftp-name-format' (which see).
;; Returns a list (HOST USER NAME), or nil if NAME does not match the format.
(defun sftp-ftp-name (name)
  (if (string-equal name sftp-ftp-name-arg)
      sftp-ftp-name-res
    (setq sftp-ftp-name-arg name
	  sftp-ftp-name-res
	  (save-match-data
	    (if (posix-string-match (car sftp-name-format) name)
		(let* ((ns (cdr sftp-name-format))
		       (host (sftp-ftp-name-component 0 ns name))
		       (user (sftp-ftp-name-component 1 ns name))
		       (name (sftp-ftp-name-component 2 ns name)))
		  (if (zerop (length user))
		      (setq user (sftp-get-user host)))
		  (list host user name))
	      nil)))))

;; Take a FULLNAME that matches according to sftp-name-format and
;; replace the name component with NAME.
(defun sftp-replace-name-component (fullname name)
  (save-match-data
    (if (posix-string-match (car sftp-name-format) fullname)
	(let* ((ns (cdr sftp-name-format))
	       (elt (nth 2 ns)))
	  (concat (substring fullname 0 (match-beginning elt))
		  name
		  (substring fullname (match-end elt)))))))

;;;; ------------------------------------------------------------
;;;; Miscellaneous utils.
;;;; ------------------------------------------------------------

;; (setq sftp-tmp-keymap (make-sparse-keymap))
;; (define-key sftp-tmp-keymap "\C-m" 'exit-minibuffer)

(defun sftp-repaint-minibuffer ()
  "Clear any existing minibuffer message; let the minibuffer contents show."
  (message nil))

;; Return the name of the buffer that collects output from the ftp process
;; connected to the given HOST and USER pair.
(defun sftp-ftp-process-buffer (host user)
  (concat "*sftp " user "@" host "*"))

;; Display the last chunk of output from the ftp process for the given HOST
;; USER pair, and signal an error including MSG in the text.
(defun sftp-error (host user msg)
  (save-excursion  ;; Prevent pop-to-buffer from changing current buffer.
    (let ((cur (selected-window))
	  (pop-up-windows t))
      (pop-to-buffer
       (get-buffer-create
	(sftp-ftp-process-buffer host user)))
      (goto-char (point-max))
      (select-window cur))
    (signal 'ftp-error (list (format "FTP Error: %s" msg)))))

(defun sftp-set-buffer-mode ()
  "Set correct modes for the current buffer if visiting a remote file."
  (if (and (stringp buffer-file-name)
	   (sftp-ftp-name buffer-file-name))
      (auto-save-mode sftp-auto-save)))

(defun sftp-kill-ftp-process (&optional buffer)
  "Kill the FTP process associated with BUFFER (the current buffer, if nil).
If the BUFFER's visited filename or `default-directory' is an FTP filename
then kill the related FTP process."
  (interactive "bKill FTP process associated with buffer: ")
  (if (null buffer)
      (setq buffer (current-buffer))
    (setq buffer (get-buffer buffer)))
  (let ((file (or (buffer-file-name buffer)
		  (with-current-buffer buffer default-directory))))
    (if file
	(let ((parsed (sftp-ftp-name (expand-file-name file))))
	  (if parsed
	      (let ((host (nth 0 parsed))
		    (user (nth 1 parsed)))
		(kill-buffer (get-buffer (sftp-ftp-process-buffer host user)))))))))

(defun sftp-quote-string (string)
  "Quote any characters in STRING that may confuse the FTP process."
  ;; This is said to be wrong; ftp is said to need quoting only for ",
  ;; and that by doubling it.  But experiment says UNIX-style kind of
  ;; quoting is correct when talking to ftp on GNU/Linux systems, and
  ;; W32-style kind of quoting on, yes, W32 systems.
  (if (stringp string)
      (shell-quote-argument string)
    ""))

(defun sftp-barf-if-not-directory (directory)
  (or (file-directory-p directory)
      (signal 'file-error
	      (list "Opening directory"
		    (if (file-exists-p directory)
			"not a directory"
		      "no such file or directory")
		    directory))))

;;;; ------------------------------------------------------------
;;;; FTP process filter support.
;;;; ------------------------------------------------------------

;; Call the function specified by CONT.  CONT can be either a function
;; or a list of a function and some args.  The first two parameters
;; passed to the function will be RESULT and LINE.  The remaining args
;; will be taken from CONT if a list was passed.

(defun sftp-call-cont (cont result line)
  (when cont
    (if (and (listp cont)
	     (not (eq (car cont) 'lambda)))
	(apply (car cont) result line (cdr cont))
      (funcall cont result line))))

(defun sftp-process-filter (proc str)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)

      (comint-output-filter proc str)
      ;; Replace STR by the result of the comint processing.
      (setq str (buffer-substring comint-last-output-start (process-mark proc)))

      (save-match-data
        (while sftp-process-busy
          (if (string-match "^s?ftp> " str)
              (progn
                (setq sftp-process-string (concat sftp-process-string
                                                  (substring str 0 (match-beginning 0))))
                (setq sftp-process-busy nil))
            (setq sftp-process-string (concat sftp-process-string str)))))

      ;; sftp-process-result-line => sftp-process-string

      ;; has the ftp client finished?  if so then do some clean-up
      ;; actions.
      (unless sftp-process-busy
	;; issue the "done" message since we've finished.
	(when (and sftp-process-msg
		   sftp-process-verbose)
	  (sftp-message "%s...done" sftp-process-msg)
	  (sftp-repaint-minibuffer)
	  (setq sftp-process-msg nil))

	;; is there a continuation we should be calling?  if so,
	;; we'd better call it, making sure we only call it once.
	(when sftp-process-continue
	  (let ((cont sftp-process-continue))
	    (setq sftp-process-continue nil)
	    (sftp-call-cont cont
                            sftp-process-result
                            sftp-process-string))))
      )))

(defun sftp-process-sentinel (proc str)
  "When FTP process changes state, nuke all file-entries in cache."
  (let ((name (process-name proc)))
    (when (string-match "\\*s?ftp \\([^@]+\\)@\\([^*]+\\)\\*" name)
      (let ((user (match-string 1 name))
	    (host (match-string 2 name)))
	(sftp-wipe-file-entries host user))))
  (setq sftp-ls-cache-file nil))

;;;; ------------------------------------------------------------
;;;; Gateway support.
;;;; ------------------------------------------------------------

(defun sftp-use-gateway-p (host)
  "Return whether to access this HOST via a  gateway."
    (not (string-match-p sftp-local-host-regexp host)))


;;; ------------------------------------------------------------
;;; Temporary file location and deletion...
;;; ------------------------------------------------------------

(defun sftp-make-tmp-name (host &optional suffix)
  "This routine will return the name of a new file."
  (make-temp-file (if (sftp-use-gateway-p host)
		      sftp-gateway-tmp-name-template
		    sftp-tmp-name-template)
		  nil suffix))

(defun sftp-del-tmp-name (filename)
  "Force to delete temporary file."
  (delete-file filename))


;;;; ------------------------------------------------------------
;;;; Interactive gateway program support.
;;;; ------------------------------------------------------------

(defvar sftp-gwp-running t)
(defvar sftp-gwp-status nil)

(defun sftp-gwp-sentinel (proc str)
  (setq sftp-gwp-running nil))

(defun sftp-gwp-filter (proc str)
  (comint-output-filter proc str)
  (with-current-buffer (process-buffer proc)
    ;; Replace STR by the result of the comint processing.
    (setq str (buffer-substring comint-last-output-start (process-mark proc))))
  (cond ((string-match "login: *$" str)
	 (process-send-string proc
                              (concat
                               (let ((sftp-default-user t))
                                 (sftp-get-user sftp-gateway-host))
                               "\n")))
	((string-match "Password: *$" str)
	 (process-send-string proc
                              (concat
                               (sftp-get-passwd sftp-gateway-host
                                                    (sftp-get-user
                                                     sftp-gateway-host))
                               "\n")))
	((string-match sftp-gateway-fatal-msgs str)
	 (delete-process proc)
	 (setq sftp-gwp-running nil))
	((string-match sftp-gateway-prompt-pattern str)
	 (setq sftp-gwp-running nil
	       sftp-gwp-status t))))

(defun sftp-gwp-start (host user name args)
  "Login to the gateway machine and fire up an FTP process."
  (let (;; It would be nice to make process-connection-type nil,
	;; but that doesn't work: ftp never responds.
	;; Can anyone find a fix for that?
	(proc (let ((process-connection-type t))
		(start-process name name
			       sftp-gateway-program
			       sftp-gateway-host)))
	(ftp (mapconcat 'identity args " ")))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc 'sftp-gwp-sentinel)
    (set-process-filter proc 'sftp-gwp-filter)
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (set-marker (process-mark proc) (point)))
    (setq sftp-gwp-running t
	  sftp-gwp-status nil)
    (sftp-message "Connecting to gateway %s..." sftp-gateway-host)
    (while sftp-gwp-running		;perform login sequence
      (accept-process-output proc))
    (unless sftp-gwp-status
      (sftp-error host user "unable to login to gateway"))
    (sftp-message "Connecting to gateway %s...done" sftp-gateway-host)
    (setq sftp-gwp-running t
	  sftp-gwp-status nil)
    (process-send-string proc sftp-gateway-setup-term-command)
    (while sftp-gwp-running		;zap ^M's and double echoing.
      (accept-process-output proc))
    (unless sftp-gwp-status
      (sftp-error host user "unable to set terminal modes on gateway"))
    (setq sftp-gwp-running t
	  sftp-gwp-status nil)
    (process-send-string proc (concat "exec " ftp "\n")) ;spawn ftp process
    proc))

;;;; ------------------------------------------------------------
;;;; Support for sending commands to the ftp process.
;;;; ------------------------------------------------------------

(defun sftp-raw-send-cmd (proc cmd &optional msg cont nowait)
  "Low-level routine to send the given FTP CMD to the FTP process PROC.
MSG is an optional message to output before and after the command.
If CONT is non-nil then it is either a function or a list of function
and some arguments.  The function will be called when the FTP command
has completed.
If CONT is nil then this routine will return (RESULT . LINE) where RESULT
is whether the command was successful, and LINE is the line from the FTP
process that caused the command to complete.
If NOWAIT is given then the routine will return immediately the command has
been queued with no result.  CONT will still be called, however."
  (if (memq (process-status proc) '(run open))
      (with-current-buffer (process-buffer proc)
	(sftp-wait-not-busy proc)
	(setq sftp-process-string ""
	      sftp-process-result-line ""
	      sftp-process-busy t
	      sftp-process-result t
	      sftp-process-multi-skip nil
	      sftp-process-msg msg
	      sftp-process-continue cont
	      cmd (concat cmd "\n"))
	(and msg sftp-process-verbose (sftp-message "%s..." msg))
	(goto-char (point-max))
        (insert cmd)
        (set-marker comint-last-input-start (process-mark proc))
	(move-marker comint-last-input-end (point))
	(process-send-string proc cmd)
	(set-marker (process-mark proc) (point))
	(if nowait
	    nil
	  (sftp-wait-not-busy proc)
	  (if cont
	      nil			;cont has already been called
	    (cons sftp-process-result sftp-process-result-line))))))

;; Wait for the sftp process PROC not to be busy.
(defun sftp-wait-not-busy (proc)
  (with-current-buffer (process-buffer proc)
    (condition-case nil
	;; This is a kludge to let user quit in case ftp gets hung.
	;; It matters because this function can be called from the filter.
	;; It is bad to allow quitting in a filter, but getting hung
	;; is worse.  By binding quit-flag to nil, we might avoid
	;; most of the probability of getting screwed because the user
	;; wants to quit some command.
	(let ((quit-flag nil)
	      (inhibit-quit nil))
	  (while sftp-process-busy
	    (accept-process-output proc)))
      (quit
       ;; If the user does quit out of this,
       ;; kill the process.  That stops any transfer in progress.
       ;; The next operation will open a new ftp connection.
       (delete-process proc)
       (signal 'quit nil)))))

(define-derived-mode internal-sftp-mode comint-mode "Internal sftp"
  "Major mode for interacting with the FTP process.

\\{comint-mode-map}"
  (make-local-variable 'sftp-process-string)
  (setq sftp-process-string "")
  (make-local-variable 'sftp-process-busy)
  (make-local-variable 'sftp-process-result)
  (make-local-variable 'sftp-process-msg)
  (make-local-variable 'sftp-process-multi-skip)
  (make-local-variable 'sftp-process-result-line)
  (make-local-variable 'sftp-process-continue)
  (setq sftp-process-result-line "")
  (setq comint-prompt-regexp "^sftp> ")
  (make-local-variable 'comint-password-prompt-regexp)
  ;; This is a regexp that can't match anything.
  ;; sftp has its own ways of handling passwords.
  (setq comint-password-prompt-regexp "\\`a\\`")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start comint-prompt-regexp))

(defvar sftp-process-startup-hook nil)

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

      ;; Run any user-specified hooks.  Note that proc, host and user are
      ;; dynamically bound at this point.
      (let ((sftp-this-user user)
            (sftp-this-host host))
        (run-hooks 'sftp-process-startup-hook))
      proc)))

(defun sftp-start-process (host user name)
  "Spawn a new FTP process ready to connect to machine HOST and give it NAME.
If HOST is only FTP-able through a gateway machine then spawn a shell
on the gateway machine to do the FTP instead."
  (let* ((use-gateway (sftp-use-gateway-p host))
	 (ftp-prog (if use-gateway
		       sftp-gateway-ftp-program-name
		     sftp-ftp-program-name))
	 (args (append (list ftp-prog) sftp-ftp-program-args (list (format "%s@%s" user host))))
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
	  ;; Copy this so we don't alter it permanently.
	  (process-environment (copy-tree process-environment))
	  (buffer (get-buffer-create name)))
      (with-current-buffer buffer
	(internal-sftp-mode))
      ;; This tells GNU ftp not to output any fancy escape sequences.
      (setenv "TERM" "dumb")
      (if use-gateway
	  (if sftp-gateway-program-interactive
	      (setq proc (sftp-gwp-start host user name args))
	    (setq proc (apply 'start-process name name
			      (append (list sftp-gateway-program
					    sftp-gateway-host)
				      args))))
	(setq proc (apply 'start-process name name args))))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (set-marker (process-mark proc) (point)))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc 'sftp-process-sentinel)
    (set-process-filter proc 'sftp-process-filter)
    (accept-process-output proc)	;wait for ftp startup message
    proc))

;; Variables for caching host and host-type
(defvar sftp-host-cache nil)

(defun sftp-send-cmd (host user cmd &optional msg cont nowait)
  "Find an FTP process connected to HOST logged in as USER and send it CMD.
MSG is an optional status message to be output before and after issuing the
command.
See the documentation for `sftp-raw-send-cmd' for a description of CONT
and NOWAIT."
  ;; Handle conversion to remote file name syntax and remote ls option
  ;; capability.
  (let ((cmd0 (car cmd))
	(cmd1 (nth 1 cmd))
        (cmd2 (nth 2 cmd))
	(sftp-this-user user)
	(sftp-this-host host)
	(sftp-this-msg msg)
	result)
    
    ;; Turn the command into one long string
    (setq cmd0 (symbol-name cmd0))
    (setq cmd (concat cmd0
		      (and cmd1 (concat " " cmd1))
		      (and cmd2 (concat " " cmd2))))

    ;; Actually send the resulting command.
    (let (afsc-result
          afsc-line)
      (sftp-raw-send-cmd
       (sftp-get-process host user)
       cmd
       msg
       (list (lambda (result line host user cmd msg cont nowait)
               (unless cont
                 (setq afsc-result result)
                 (setq afsc-line line))
               (if result
                   (sftp-call-cont cont result line)
                 (sftp-raw-send-cmd
                  (sftp-get-process host user)
                  cmd
                  msg
                  (list (lambda (result line cont)
                          (or cont (setq afsc-result result
                                         afsc-line line))
                          (sftp-call-cont cont result line))
                        cont)
                  nowait)))
             host user cmd msg cont nowait)
       nowait)

      (if nowait
          nil
        (if cont
            nil
          (cons afsc-result afsc-line))))))


;;;; ------------------------------------------------------------
;;;; Remote file and directory listing support.
;;;; ------------------------------------------------------------

(defvar sftp-parse-list-func-alist nil
  "Alist saying how to parse directory listings for certain OS types.
Association list of (TYPE . FUNC) pairs.  The FUNC is a routine which
can parse the output from a DIR listing for a host of type TYPE.")

;; With no-error nil, this function returns:
;; an error if file is not an sftp-name
;;                      (This should never happen.)
;; an error if either the listing is unreadable or there is an ftp error.
;; the listing (a string), if everything works.
;;
;; With no-error t, it returns:
;; an error if not an sftp-name
;; error if listing is unreadable (most likely caused by a slow connection)
;; nil if ftp error (this is because although asking to list a nonexistent
;;                   directory on a remote unix machine usually (except
;;                   maybe for dumb hosts) returns an ls error, but no
;;                   ftp error, if the same is done on a VMS machine,
;;                   an ftp error is returned. Need to trap the error
;;                   so we can go on and try to list the parent.)
;; the listing, if everything works.

;; If WILDCARD is non-nil, then this implements the guts of insert-directory
;; in the wildcard case.  Then we make a relative directory listing
;; of FILE within the directory specified by `default-directory'.

(defvar sftp-before-parse-ls-hook nil
  "Normal hook run before parsing the text of an FTP directory listing.")

(defvar sftp-after-parse-ls-hook nil
  "Normal hook run after parsing the text of an FTP directory listing.")

(defun sftp-host-type (h u)
  (edebug))

(defun sftp-ls (file parse &optional no-error wildcard)
  "Return the output of a `DIR' or `ls' command done over FTP.
FILE is the full name of the remote file and PARSE specifies that
the output should be parsed and stored away in the internal cache."
  ;; If parse is t, we assume that file is a directory. i.e. we only parse
  ;; full directory listings.
  (let* ((sftp-this-file (sftp-expand-file-name file))
	 (parsed (sftp-ftp-name sftp-this-file)))
    (if parsed
	(let* ((host (nth 0 parsed))
	       (user (nth 1 parsed))
	       (name (sftp-quote-string (nth 2 parsed)))
	       (key (directory-file-name sftp-this-file))
	       result
               lscmd parse-func)
	  (if (string-equal name "")
	      (setq name
		    (sftp-real-file-name-as-directory
		     (sftp-expand-dir host user "~"))))
	  (if (and sftp-ls-cache-file
		   (string-equal key sftp-ls-cache-file))
	      sftp-ls-cache-res
	    (if wildcard
		(progn
		  (sftp-cd host user (file-name-directory name))
		  (setq lscmd (list 'ls file)))
	      (setq lscmd (list 'dir name)))
	    (unwind-protect
		(if (car (setq result (sftp-send-cmd
				       host
				       user
				       lscmd
				       (format "Listing %s"
					       (sftp-abbreviate-filename
						sftp-this-file)))))
		    (with-current-buffer (get-buffer-create
                                          sftp-data-buffer-name)
		      (erase-buffer)
		      (insert result)
                      ;; remove ^M inserted by the win32 ftp client
                      (while (re-search-forward "\r$" nil t)
                        (replace-match ""))
                      (goto-char 1)
		      (run-hooks 'sftp-before-parse-ls-hook)
		      (if parse
			  (sftp-set-files sftp-this-file (sftp-parse-dired-listing)))
                      ;; Place this hook here to convert the contents of the
                      ;; buffer to a ls compatible format if the host system
                      ;; that is being queried is other than Unix i.e. VMS
                      ;; returns an ls format that really sucks.
                      (run-hooks 'sftp-after-parse-ls-hook)
		      (setq sftp-ls-cache-file key
			    sftp-ls-cache-res (buffer-string))
		      ;; (kill-buffer (current-buffer))
		      (if (equal sftp-ls-cache-res "total 0\n")
			  ;; wu-ftpd seems to return a successful result
			  ;; with an empty file-listing when doing a
			  ;; `DIR /some/file/.' which leads sftp to
			  ;; believe that /some/file is a directory ;-(
			  nil
			sftp-ls-cache-res))
		  (if no-error
		      nil
		    (sftp-error host user
				    (concat "DIR failed: " (cdr result)))))
	      )))
      (error "Should never happen. Please report. Bug ref. no.: 1"))))

;;;; ------------------------------------------------------------
;;;; Directory information caching support.
;;;; ------------------------------------------------------------

(defvar sftp-add-file-entry-alist nil
  "Alist saying how to add file entries on certain OS types.
Association list of pairs (TYPE . FUNC), where FUNC is a function
to be used to add a file entry for the OS TYPE.
The main reason for this alist is to deal with file versions in VMS.")

(defvar sftp-delete-file-entry-alist nil
  "Alist saying how to delete files on certain OS types.
Association list of pairs (TYPE . FUNC), where FUNC is a function
to be used to delete a file entry for the OS TYPE.
The main reason for this alist is to deal with file versions in VMS.")

(defun sftp-add-file-entry (name &optional dir-p)
  "Add a file entry for file NAME, if its directory info exists."
  (funcall (or (cdr (assq (sftp-host-type
			   (car (sftp-ftp-name name)))
			  sftp-add-file-entry-alist))
	       'sftp-internal-add-file-entry)
	   name dir-p)
  (setq sftp-ls-cache-file nil))

(defun sftp-delete-file-entry (name &optional dir-p)
  "Delete the file entry for file NAME, if its directory info exists."
  (funcall (or (cdr (assq (sftp-host-type
			   (car (sftp-ftp-name name)))
			  sftp-delete-file-entry-alist))
	       'sftp-internal-delete-file-entry)
	   name dir-p)
  (setq sftp-ls-cache-file nil))

(defmacro sftp-parse-filename ()
  ;;Extract the filename from the current line of a dired-like listing.
  `(save-match-data
     (let ((eol (progn (end-of-line) (point))))
       (beginning-of-line)
       (if (re-search-forward directory-listing-before-filename-regexp eol t)
	   (buffer-substring (point) eol)))))

;; This deals with the F switch. Should also do something about
;; unquoting names obtained with the SysV b switch and the GNU Q
;; switch. See Sebastian's dired-get-filename.

(defun sftp-ls-parser (switches)
  ;; Meant to be called by sftp-parse-dired-listing
  (let ((tbl (make-hash-table :test 'equal))
	(used-F (and (stringp switches)
		     (string-match "F" switches)))
	file-type symlink directory file)
    (while (setq file (sftp-parse-filename))
      (beginning-of-line)
      (skip-chars-forward "\t 0-9")
      (setq file-type (following-char)
	    directory (eq file-type ?d))
      (if (eq file-type ?l)
	  (let ((end (string-match " -> " file)))
	    (if end
		;; Sometimes `ls' appends a @ at the end of the target.
		(setq symlink (substring file (match-end 0)
					 (string-match "@\\'" file))
		      file (substring file 0 end))
	      ;; Shouldn't happen
	      (setq symlink "")))
	(setq symlink nil))
      ;; Only do a costly regexp search if the F switch was used.
      (if (and used-F
	       (not (string-equal file ""))
	       (looking-at
		".[-r][-w]\\([^ ]\\)[-r][-w]\\([^ ]\\)[-r][-w]\\([^ ]\\)"))
	  (let ((socket (eq file-type ?s))
		(executable
		 (and (not symlink) ; x bits don't mean a thing for symlinks
		      (string-match
		       "[xst]"
		       (concat (match-string 1)
			       (match-string 2)
			       (match-string 3))))))
	    ;; Some ls's with the F switch mark symlinks with an @ (ULTRIX)
	    ;; and others don't. (sigh...) Beware, that some Unix's don't
	    ;; seem to believe in the F-switch
	    (if (or (and symlink (string-match "@\\'" file))
		    (and directory (string-match "/\\'" file))
		    (and executable (string-match "*\\'" file))
		    (and socket (string-match "=\\'" file)))
		(setq file (substring file 0 -1)))))
      (puthash file (or symlink directory) tbl)
      (forward-line 1))
    (puthash "." t tbl)
    (puthash ".." t tbl)
    tbl))

;;; The dl stuff for descriptive listings

(defvar sftp-dl-dir-regexp nil
  "Regexp matching directories which are listed in dl format.
This regexp should not be anchored with a trailing `$', because it should
match subdirectories as well.")

(defun sftp-add-dl-dir (dir)
  "Interactively add a DIR to `sftp-dl-dir-regexp'."
  (interactive
   (list (read-string "Directory: "
		      (let ((name (or (buffer-file-name) default-directory)))
			(and name (sftp-ftp-name name)
			     (file-name-directory name))))))
  (if (not (and sftp-dl-dir-regexp
		(string-match sftp-dl-dir-regexp dir)))
      (setq sftp-dl-dir-regexp
	    (concat "^" (regexp-quote dir)
		    (and sftp-dl-dir-regexp "\\|")
		    sftp-dl-dir-regexp))))

(defmacro sftp-dl-parser ()
  ;; Parse the current buffer, which is assumed to be a descriptive
  ;; listing, and return a hashtable.
  `(let ((tbl (make-hash-table :test 'equal)))
     (while (not (eobp))
       (puthash
        (buffer-substring (point)
                          (progn
                            (skip-chars-forward "^ /\n")
                            (point)))
        (eq (following-char) ?/)
        tbl)
       (forward-line 1))
     (puthash "." t tbl)
     (puthash ".." t tbl)
     tbl))

;; Parse the current buffer which is assumed to be in a dired-like listing
;; format, and return a hashtable as the result. If the listing is not really
;; a listing, then return nil.

(defun sftp-parse-dired-listing (&optional switches)
  (save-match-data
    (cond
     ((looking-at "^total [0-9]+$")
      (forward-line 1)
      ;; Some systems put in a blank line here.
      (if (eolp) (forward-line 1))
      (sftp-ls-parser switches))
     ((looking-at "[^\n]+\\( not found\\|: Not a directory\\)\n\\'")
      ;; It's an ls error message.
      nil)
     ((eobp) ; i.e. (zerop (buffer-size))
      ;; This could be one of:
      ;; (1) An Ultrix ls error message
      ;; (2) A listing with the A switch of an empty directory
      ;;     on a machine which doesn't give a total line.
      ;; (3) The twilight zone.
      ;; We'll assume (1) for now.
      nil)
     ((re-search-forward directory-listing-before-filename-regexp nil t)
      (beginning-of-line)
      (sftp-ls-parser switches))
     ((re-search-forward "^[^ \n\t]+ +\\([0-9]+\\|-\\|=\\) " nil t)
      ;; It's a dl listing (I hope).
      ;; file is bound by the call to sftp-ls
      (sftp-add-dl-dir sftp-this-file)
      (beginning-of-line)
      (sftp-dl-parser))
     (t nil))))

(defun sftp-set-files (directory files)
  "For a given DIRECTORY, set or change the associated FILES hashtable."
  (and files (puthash (file-name-as-directory directory)
		      files sftp-files-hashtable)))

(defun sftp-switches-ok (switches)
  "Return SWITCHES (a string) if suitable for our use."
  (and (stringp switches)
       ;; We allow the A switch, which lists all files except "." and
       ;; "..".  This is OK because we manually insert these entries
       ;; in the hash table.
       (string-match
	"--\\(almost-\\)?all\\>\\|\\(\\`\\| \\)-[[:alpha:]]*[aA]" switches)
       (string-match "\\(\\`\\| \\)-[[:alpha:]]*l" switches)
       (not (string-match
	     "--recursive\\>\\|\\(\\`\\| \\)-[[:alpha:]]*R" switches))
       switches))

(defun sftp-get-files (directory &optional no-error)
  "Given a DIRECTORY, return a hashtable of file entries.
This will give an error or return nil, depending on the value of
NO-ERROR, if a listing for DIRECTORY cannot be obtained."
  (setq directory (file-name-as-directory directory)) ;normalize
  (or (gethash directory sftp-files-hashtable)
      (save-match-data
	(and (sftp-ls directory t no-error)
	     (gethash directory sftp-files-hashtable)))))

;; Given NAME, return the file part that can be used for looking up the
;; file's entry in a hashtable.
(defmacro sftp-get-file-part (name)
  `(let ((file (file-name-nondirectory ,name)))
     (if (string-equal file "")
	 "."
       file)))

;; Return whether sftp-file-entry-p and sftp-get-file-entry are
;; allowed to determine if NAME is a sub-directory by listing it directly,
;; rather than listing its parent directory. This is used for efficiency so
;; that a wasted listing is not done:
;; 1. When looking for a .dired file in dired-x.el.
;; 2. The syntax of FILE and DIR make it impossible that FILE could be a valid
;;     subdirectory. This is of course an OS dependent judgment.

(defvar dired-local-variables-file)
(defmacro sftp-allow-child-lookup (dir file)
  `(not
    (let* ((efile ,file)		; expand once.
           (edir ,dir)
           (parsed (sftp-ftp-name edir)))
      (and (boundp 'dired-local-variables-file) ; in the dired-x package
           (stringp dired-local-variables-file)
           (string-equal dired-local-variables-file efile)))))

(defun sftp-file-entry-p (name)
  "Given NAME, return whether there is a file entry for it."
  (let* ((name (directory-file-name name))
	 (dir (file-name-directory name))
	 (ent (gethash dir sftp-files-hashtable))
	 (file (sftp-get-file-part name)))
    (if ent
	(sftp-hash-entry-exists-p file ent)
      (or (and (sftp-allow-child-lookup dir file)
	       (setq ent (sftp-get-files name t))
	       ;; Try a child lookup. i.e. try to list file as a
	       ;; subdirectory of dir. This is a good idea because
	       ;; we may not have read permission for file's parent. Also,
	       ;; people tend to work down directory trees anyway. We use
	       ;; no-error ;; because if file does not exist as a subdir.,
	       ;; then dumb hosts will give an ftp error. Smart unix hosts
	       ;; will simply send back the ls
	       ;; error message.
	       (gethash "." ent))
	  ;; Child lookup failed, so try the parent.
	  (sftp-hash-entry-exists-p
	   file (sftp-get-files dir 'no-error))))))

(defun sftp-get-file-entry (name)
  "Given NAME, return the given file entry.
The entry will be either t for a directory, nil for a normal file,
or a string for a symlink.  If the file isn't in the hashtable,
this also returns nil."
  (let* ((name (directory-file-name name))
	 (dir (file-name-directory name))
	 (ent (gethash dir sftp-files-hashtable))
	 (file (sftp-get-file-part name)))
    (if ent
	(gethash file ent)
      (or (and (sftp-allow-child-lookup dir file)
	       (setq ent (sftp-get-files name t))
	       (gethash "." ent))
	  ;; i.e. it's a directory by child lookup
	  (and (setq ent (sftp-get-files dir t))
	       (gethash file ent))))))

(defun sftp-internal-delete-file-entry (name &optional dir-p)
  (when dir-p
    (setq name (file-name-as-directory name))
    (remhash name sftp-files-hashtable)
    (setq name (directory-file-name name)))
  ;; Note that file-name-as-directory followed by directory-file-name
  ;; serves to canonicalize directory file names to their unix form.
  ;; i.e. in VMS, FOO.DIR -> FOO/ -> FOO
  (let ((files (gethash (file-name-directory name) sftp-files-hashtable)))
    (if files
	(remhash (sftp-get-file-part name) files))))

(defun sftp-internal-add-file-entry (name &optional dir-p)
  (and dir-p
       (setq name (directory-file-name name)))
  (let ((files (gethash (file-name-directory name) sftp-files-hashtable)))
    (if files
	(puthash (sftp-get-file-part name) dir-p files))))

(defun sftp-wipe-file-entries (host user)
  "Get rid of entry for HOST, USER pair from file entry information hashtable."
  (let ((new-tbl (make-hash-table :test 'equal
				  :size (hash-table-size
					 sftp-files-hashtable))))
    (maphash
     (lambda (key val)
       (let ((parsed (sftp-ftp-name key)))
         (if parsed
             (let ((h (nth 0 parsed))
                   (u (nth 1 parsed)))
               (or (and (equal host h) (equal user u))
                   (puthash key val new-tbl))))))
     sftp-files-hashtable)
    (setq sftp-files-hashtable new-tbl)))

(defun sftp-cd (host user dir &optional noerror)
  (let ((result (sftp-send-cmd host user (list 'cd dir) "Doing CD")))
    (if noerror
        result
      (or (car result)
	  (sftp-error host user (concat "CD failed: " (cdr result)))))))

(defun sftp-get-pwd (host user)
  "Attempt to get the current working directory for the given HOST/USER pair.
Returns (DIR . LINE) where DIR is either the directory or nil if not found,
and LINE is the relevant success or fail line from the FTP-client."
  (let* ((result (sftp-send-cmd host user '(pwd) "Getting PWD"))
	 (line (cdr result))
	 dir)
    (if (car result)
	(save-match-data
	  (and (string-match ": \\(.+\\)$" line)
	       (setq dir (match-string 1 line)))))
    (cons dir line)))

;;; ------------------------------------------------------------
;;; expand-file-name and friends...which currently don't work
;;; ------------------------------------------------------------

(defun sftp-expand-dir (host user dir)
  "Return the result of doing a PWD in the current FTP session.
Use the connection to machine HOST
logged in as user USER and cd'd to directory DIR."
  (let* ((key (concat host "/" user "/" dir))
	 (res (gethash key sftp-expand-dir-hashtable)))
    (or res
	(progn
	  (or res
	      (if (string-equal dir "~")
		  (setq res (car (sftp-get-pwd host user)))
		(let ((home (sftp-expand-dir host user "~")))
		  (unwind-protect
		      (and (sftp-cd host user dir)
			   (setq res (car (sftp-get-pwd host user))))
		    (sftp-cd host user home)))))
	  (if res
	      (let ((sftp-this-user user)
		    (sftp-this-host host))
		(puthash key res sftp-expand-dir-hashtable)))
	  res))))

(defun sftp-canonize-filename (n)
  "Take a string N and short-circuit //, /. and /.."
  (if (string-match "[^:]+//" n)		;don't upset Apollo users
      (setq n (substring n (1- (match-end 0)))))
  (let ((parsed (sftp-ftp-name n)))
    (if parsed
	(let ((host (car parsed))
	      (user (nth 1 parsed))
	      (name (nth 2 parsed)))

	  ;; See if remote name is absolute.  If so then just expand it and
	  ;; replace the name component of the overall name.
	  (cond ((string-match "\\`/" name)
		 name)

		;; Name starts with ~ or ~user.  Resolve that part of the name
		;; making it absolute then re-expand it.
		((string-match "\\`~[^/]*" name)
		 (let* ((tilda (match-string 0 name))
			(rest (substring name (match-end 0)))
			(dir (sftp-expand-dir host user tilda)))
		   (if dir
                       ;; C-x d /ftp:anonymous@ftp.gnu.org:~/ RET
                       ;; seems to cause `rest' to sometimes be empty.
                       ;; Maybe it's an error for `rest' to be empty here,
                       ;; but until we figure this out, this quick fix
                       ;; seems to do the trick.
		       (setq name (cond ((string-equal rest "") dir)
					((string-equal dir "/") rest)
					(t (concat dir rest))))
		     (error "User \"%s\" is not known"
			    (substring tilda 1)))))

		;; relative name.  Tack on homedir and re-expand.
		(t
		 (let ((dir (sftp-expand-dir host user "~")))
		   (if dir
		       (setq name (concat
				   (sftp-real-file-name-as-directory dir)
				   name))
		     (error "Unable to obtain CWD")))))

	  ;; If name starts with //, preserve that, for apollo system.
	  (unless (string-match "\\`//" name)
            (if (not (eq system-type 'windows-nt))
                (setq name (sftp-real-expand-file-name name))
              ;; Windows UNC default dirs do not make sense for ftp.
              (setq name (if (and default-directory
				  (string-match "\\`//" default-directory))
                             (sftp-real-expand-file-name name "c:/")
                           (sftp-real-expand-file-name name)))
              ;; Strip off possible drive specifier.
              (if (string-match "\\`[a-zA-Z]:" name)
                  (setq name (substring name 2))))
            (if (string-match "\\`//" name)
                (setq name (substring name 1))))

	  ;; Now substitute the expanded name back into the overall filename.
	  (sftp-replace-name-component n name))

      ;; non-sftp name.  Just expand normally.
      (if (eq (string-to-char n) ?/)
	  (sftp-real-expand-file-name n)
	(sftp-real-expand-file-name
	 (sftp-real-file-name-nondirectory n)
	 (sftp-real-file-name-directory n))))))

(defun sftp-expand-file-name (name &optional default)
  "Documented as `expand-file-name'."
  (save-match-data
    (setq default (or default default-directory))
    (cond ((eq (string-to-char name) ?~)
	   (sftp-real-expand-file-name name))
	  ((eq (string-to-char name) ?/)
	   (sftp-canonize-filename name))
	  ((and (eq system-type 'windows-nt)
		(eq (string-to-char name) ?\\))
	   (sftp-canonize-filename name))
	  ((and (eq system-type 'windows-nt)
		(or (string-match "\\`[a-zA-Z]:" name)
		    (string-match "\\`[a-zA-Z]:" default)))
	   (sftp-real-expand-file-name name default))
	  ((zerop (length name))
	   (sftp-canonize-filename default))
	  ((sftp-canonize-filename
	    (concat (file-name-as-directory default) name))))))

;;; These are problems--they are currently not enabled.



(defun sftp-file-name-as-directory (name)
  "Documented as `file-name-as-directory'."
  (let ((parsed (sftp-ftp-name name)))
    (if parsed
	(if (string-equal (nth 2 parsed) "")
	    name
	  (sftp-real-file-name-as-directory name))
      (sftp-real-file-name-as-directory name))))

(defun sftp-file-name-directory (name)
  "Documented as `file-name-directory'."
  (let ((parsed (sftp-ftp-name name)))
    (if parsed
	(let ((filename (nth 2 parsed)))
	  (if (string-match-p "\\`~[^/]*\\'" filename)
	      name
	    (sftp-replace-name-component
	     name
	     (sftp-real-file-name-directory filename))))
      (sftp-real-file-name-directory name))))

(defun sftp-file-name-nondirectory (name)
  "Documented as `file-name-nondirectory'."
  (let ((parsed (sftp-ftp-name name)))
    (if parsed
	(let ((filename (nth 2 parsed)))
	  (if (string-match-p "\\`~[^/]*\\'" filename)
	      ""
	    (sftp-real-file-name-nondirectory filename)))
      (sftp-real-file-name-nondirectory name))))

(defun sftp-directory-file-name (dir)
  "Documented as `directory-file-name'."
  (let ((parsed (sftp-ftp-name dir)))
    (if parsed
	(sftp-replace-name-component
	 dir
	 (sftp-real-directory-file-name (nth 2 parsed)))
      (sftp-real-directory-file-name dir))))


;;; Hooks that handle Emacs primitives.

(defun sftp-write-region (start end filename &optional append visit)
  (setq filename (expand-file-name filename))
  (let ((parsed (sftp-ftp-name filename)))
    (if parsed
	(let* ((host (nth 0 parsed))
	       (user (nth 1 parsed))
	       (name (sftp-quote-string (nth 2 parsed)))
	       (temp (sftp-make-tmp-name host))
	       (cmd (if append 'append 'put))
	       (abbr (sftp-abbreviate-filename filename))
	       ;; we need to reset `last-coding-system-used' to its
	       ;; value immediately after calling the real write-region,
	       ;; so that `basic-save-buffer' doesn't see whatever value
	       ;; might be used when communicating with the ftp process.
	       (coding-system-used last-coding-system-used))
	  (unwind-protect
	      (progn
		(let ((filename (buffer-file-name))
		      (mod-p (buffer-modified-p)))
		  (unwind-protect
		      (progn
			(sftp-real-write-region start end temp nil
						    (or visit 'quiet))
			(setq coding-system-used last-coding-system-used))
		    ;; cleanup forms
		    (setq coding-system-used last-coding-system-used)
		    (setq buffer-file-name filename)
		    (restore-buffer-modified-p mod-p)))
		
		;; put or append the file.
		(let ((result (sftp-send-cmd host user
						 (list cmd temp name)
						 (format "Writing %s" abbr))))
		  (or (car result)
		      (signal 'ftp-error
			      (list
			       "Opening output file"
			       (format "FTP Error: \"%s\"" (cdr result))
			       filename)))))
	    (sftp-del-tmp-name temp))
	  (if (eq visit t)
	      (progn
		(set-visited-file-modtime (sftp-file-modtime filename))
		(sftp-set-buffer-mode)
		(setq buffer-file-name filename)
		(set-buffer-modified-p nil)))
	  ;; ensure `last-coding-system-used' has an appropriate value
	  (setq last-coding-system-used coding-system-used)
	  (sftp-message "Wrote %s" abbr)
	  (sftp-add-file-entry filename))
      (sftp-real-write-region start end filename append visit))))

(defun sftp-insert-file-contents (filename &optional visit beg end replace)
  (barf-if-buffer-read-only)
  (setq filename (expand-file-name filename))
  (let ((parsed (sftp-ftp-name filename)))
    (if parsed
	(progn
	  (if visit
	      (setq buffer-file-name filename))
	  (if (or (file-exists-p filename)
		  (progn
		    (setq sftp-ls-cache-file nil)
		    (remhash (file-name-directory filename)
			     sftp-files-hashtable)
		    (file-exists-p filename)))
	      (let* ((host (nth 0 parsed))
		     (user (nth 1 parsed))
		     (name (sftp-quote-string (nth 2 parsed)))
		     (temp (sftp-make-tmp-name host))
		     (abbr (sftp-abbreviate-filename filename))
		     (coding-system-used last-coding-system-used)
		     size)
		(unwind-protect
		    (progn
		      (let ((result (sftp-send-cmd host user
                                                   (list 'get name temp)
                                                   (format "Retrieving %s" abbr))))
			(or (car result)
			    (signal 'ftp-error
				    (list
				     "Opening input file"
				     (format "FTP Error: \"%s\"" (cdr result))
				     filename))))
		      (if (or (sftp-real-file-readable-p temp)
			      (sleep-for sftp-retry-time)
			      ;; Wait for file to hopefully appear.
			      (sftp-real-file-readable-p temp))
			  (setq
			   size
			   (nth 1 (sftp-real-insert-file-contents
				   temp visit beg end replace))
			   coding-system-used last-coding-system-used)
			(signal 'ftp-error
				(list
				 "Opening input file:"
				 (format
				  "FTP Error: %s not arrived or readable"
				  filename)))))
		  (sftp-del-tmp-name temp))
		(if visit
		    (progn
		      (set-visited-file-modtime
                       (sftp-file-modtime filename))
		      (setq buffer-file-name filename)))
		(setq last-coding-system-used coding-system-used)
		(list filename size))
	    (signal 'file-error
		    (list
		     "Opening input file"
		     filename))))
      (sftp-real-insert-file-contents filename visit beg end replace))))

(defun sftp-expand-symlink (file dir)
  (let ((res (if (file-name-absolute-p file)
		 (sftp-replace-name-component dir file)
	       (expand-file-name file dir))))
    (if (file-symlink-p res)
	(sftp-expand-symlink
	 (sftp-get-file-entry res)
	 (file-name-directory (directory-file-name res)))
      res)))

(defun sftp-file-symlink-p (file)
  ;; call sftp-expand-file-name rather than the normal
  ;; expand-file-name to stop loops when using a package that
  ;; redefines both file-symlink-p and expand-file-name.
  (setq file (sftp-expand-file-name file))
  (if (sftp-ftp-name file)
      (condition-case nil
	  (let ((ent (sftp-get-files (file-name-directory file))))
	    (and ent
		 (stringp (setq ent
				(gethash (sftp-get-file-part file) ent)))
		 ent))
	;; If we can't read the parent directory, just assume
	;; this file is not a symlink.
	;; This makes it possible to access a directory that
	;; whose parent is not readable.
	(file-error nil))
    (sftp-real-file-symlink-p file)))

(defun sftp-file-exists-p (name)
  (setq name (expand-file-name name))
  (if (sftp-ftp-name name)
      (if (sftp-file-entry-p name)
	  (let ((file-ent (sftp-get-file-entry name)))
	    (if (stringp file-ent)
		(file-exists-p
		 (sftp-expand-symlink file-ent
					  (file-name-directory
					   (directory-file-name name))))
	      t)))
    (sftp-real-file-exists-p name)))

(defun sftp-file-directory-p (name)
  (setq name (expand-file-name name))
  (if (sftp-ftp-name name)
      ;; We do a file-name-as-directory on name here because some
      ;; machines (VMS) use a .DIR to indicate the filename associated
      ;; with a directory. This needs to be canonicalized.
      (let ((file-ent (sftp-get-file-entry
		       (sftp-file-name-as-directory name))))
	(if (stringp file-ent)
	    ;; Calling file-directory-p doesn't work because sftp
	    ;; is temporarily disabled for this operation.
	    (sftp-file-directory-p
	     (sftp-expand-symlink file-ent
				      (file-name-directory
				       (directory-file-name name))))
	  file-ent))
    (sftp-real-file-directory-p name)))

(defun sftp-directory-files (directory &optional full match
					   &rest v19-args)
  (setq directory (expand-file-name directory))
  (if (sftp-ftp-name directory)
      (progn
	(sftp-barf-if-not-directory directory)
	(let ((tail (sftp-hash-table-keys
		     (sftp-get-files directory)))
	      files f)
	  (setq directory (file-name-as-directory directory))
	  (while tail
	    (setq f (car tail)
		  tail (cdr tail))
	    (if (or (not match) (string-match-p match f))
		(setq files
		      (cons (if full (concat directory f) f) files))))
	  (nreverse files)))
    (apply 'sftp-real-directory-files directory full match v19-args)))

(defun sftp-directory-files-and-attributes
  (directory &optional full match nosort id-format)
  (setq directory (expand-file-name directory))
  (if (sftp-ftp-name directory)
      (mapcar
       (lambda (file)
	 (cons file (file-attributes (expand-file-name file directory))))
       (sftp-directory-files directory full match nosort))
    (sftp-real-directory-files-and-attributes
     directory full match nosort id-format)))

(defun sftp-file-attributes (file &optional id-format)
  (setq file (expand-file-name file))
  (let ((parsed (sftp-ftp-name file)))
    (if parsed
	(let ((part (sftp-get-file-part file))
	      (files (sftp-get-files (file-name-directory file))))
	  (if (sftp-hash-entry-exists-p part files)
	      (let ((host (nth 0 parsed))
		    (user (nth 1 parsed))
		    (name (nth 2 parsed))
		    (dirp (gethash part files))
		    (inode (gethash file sftp-inodes-hashtable)))
		(unless inode
		  (setq inode sftp-next-inode-number
			sftp-next-inode-number (1+ inode))
		  (puthash file inode sftp-inodes-hashtable))
		(list (if (and (stringp dirp) (file-name-absolute-p dirp))
			  (sftp-expand-symlink dirp
						   (file-name-directory file))
			dirp)		;0 file type
		      -1		;1 link count
		      -1		;2 uid
		      -1		;3 gid
		      '(0 0)		;4 atime
		      (sftp-file-modtime file) ;5 mtime
		      '(0 0)		;6 ctime
		      (sftp-file-size file)	;7 size
		      (concat (if (stringp dirp) "l" (if dirp "d" "-"))
			      "?????????") ;8 mode
		      nil		;9 gid weird
		      inode		;10 "inode number".
		      -1		;11 device number [v19 only]
		      ))))
      (if id-format
	  (sftp-real-file-attributes file id-format)
	(sftp-real-file-attributes file)))))

(defun sftp-file-newer-than-file-p (f1 f2)
  (let ((f1-parsed (sftp-ftp-name f1))
        (f2-parsed (sftp-ftp-name f2)))
    (if (or f1-parsed f2-parsed)
        (let ((f1-mt (nth 5 (file-attributes f1)))
              (f2-mt (nth 5 (file-attributes f2))))
          (cond ((null f1-mt) nil)
                ((null f2-mt) t)
                (t (> (float-time f1-mt) (float-time f2-mt)))))
      (sftp-real-file-newer-than-file-p f1 f2))))

(defun sftp-file-writable-p (file)
  (let ((sftp-process-verbose nil))
    (setq file (expand-file-name file))
    (if (sftp-ftp-name file)
	(or (file-exists-p file)	;guess here for speed
	    (file-directory-p (file-name-directory file)))
      (sftp-real-file-writable-p file))))

(defun sftp-file-readable-p (file)
  (let ((sftp-process-verbose nil))
    (setq file (expand-file-name file))
    (if (sftp-ftp-name file)
	(file-exists-p file)
      (sftp-real-file-readable-p file))))

(defun sftp-file-executable-p (file)
  (let ((sftp-process-verbose nil))
    (setq file (expand-file-name file))
    (if (sftp-ftp-name file)
	(file-exists-p file)
      (sftp-real-file-executable-p file))))

(defun sftp-delete-file (file &optional trash)
  (interactive (list (read-file-name "Delete file: " nil default-directory)
		     (null current-prefix-arg)))
  (setq file (expand-file-name file))
  (let ((parsed (sftp-ftp-name file)))
    (if parsed
	(let* ((host (nth 0 parsed))
	       (user (nth 1 parsed))
	       (name (sftp-quote-string (nth 2 parsed)))
	       (abbr (sftp-abbreviate-filename file))
	       (result (sftp-send-cmd host user
					  (list 'delete name)
					  (format "Deleting %s" abbr))))
	  (or (car result)
	      (signal 'ftp-error
		      (list
		       "Removing old name"
		       (format "FTP Error: \"%s\"" (cdr result))
		       file)))
	  (sftp-delete-file-entry file))
      (sftp-real-delete-file file trash))))

(defun sftp-file-modtime (file)
  "Return the modification time of remote file FILE.
Value is (0 0) if the modification time cannot be determined."
  (let* ((parsed (sftp-ftp-name file))
	 ;; At least one FTP server (wu-ftpd) can return a "226
	 ;; Transfer complete" before the "213 MODTIME".  Let's skip
	 ;; that.
	 (sftp-skip-msgs (concat sftp-skip-msgs "\\|^226"))
         (res (sftp-send-cmd (car parsed) (cadr parsed)
                                 (list 'quote "mdtm" (cadr (cdr parsed)))))
	 (line (cdr res))
	 (modtime '(0 0)))
    ;; MDTM should return "213 YYYYMMDDhhmmss" GMT on success
    ;; following the Internet draft for FTP extensions.
    ;; Bob@rattlesnake.com reports that is returns something different
    ;; for at least one FTP server.  So, let's use the response only
    ;; if it matches the Internet draft.
    (when (string-match-p "^213 [0-9]\\{14\\}$" line)
      (setq modtime
	    (encode-time
	     (string-to-number (substring line 16 18))
	     (string-to-number (substring line 14 16))
	     (string-to-number (substring line 12 14))
	     (string-to-number (substring line 10 12))
	     (string-to-number (substring line  8 10))
	     (string-to-number (substring line  4  8))
	     0)))
    modtime))

(defun sftp-verify-visited-file-modtime (buf)
  (let ((name (buffer-file-name buf)))
    (if (and (stringp name) (sftp-ftp-name name))
        (let ((file-mdtm (sftp-file-modtime name))
              (buf-mdtm (with-current-buffer buf (visited-file-modtime))))
          (or (zerop (car file-mdtm))
              (<= (float-time file-mdtm) (float-time buf-mdtm))))
      (sftp-real-verify-visited-file-modtime buf))))

(defun sftp-file-size (file)
  "Return the size of remote file FILE. Return -1 if can't get it."
  (let* ((parsed (sftp-ftp-name file))
	 (host (nth 0 parsed))
	 (user (nth 1 parsed))
	 (name (sftp-quote-string (nth 2 parsed)))
	 ;; At least one FTP server (wu-ftpd) can return a "226
	 ;; Transfer complete" before the "213 SIZE".  Let's skip
	 ;; that.
	 (sftp-skip-msgs (concat sftp-skip-msgs "\\|^226"))
	 (res (unwind-protect
                  (sftp-send-cmd host user (list 'quote "size" name))))
	 (line (cdr res)))
    (if (string-match "^213 \\([0-9]+\\)$" line)
	(string-to-number (match-string 1 line))
      -1)))


;;;; ------------------------------------------------------------
;;;; File copying support... totally re-written 6/24/92.
;;;; ------------------------------------------------------------

(defun sftp-barf-or-query-if-file-exists (absname querystring interactive)
  (if (file-exists-p absname)
      (if (not interactive)
	  (signal 'file-already-exists (list absname))
	(if (not (yes-or-no-p (format "File %s already exists; %s anyway? "
				      absname querystring)))
	    (signal 'file-already-exists (list absname))))))

;; async local copy commented out for now since I don't seem to get
;; the process sentinel called for some processes.
;;
;; (defun sftp-copy-file-locally (filename newname ok-if-already-exists
;; 					    keep-date cont)
;;   "Kludge to copy a local file and call a continuation when the copy
;; finishes."
;;   ;; check to see if we can overwrite
;;   (if (or (not ok-if-already-exists)
;; 	  (numberp ok-if-already-exists))
;;       (sftp-barf-or-query-if-file-exists newname "copy to it"
;; 					     (numberp ok-if-already-exists)))
;;   (let ((proc (start-process " *copy*"
;; 			     (generate-new-buffer "*copy*")
;; 			     "cp"
;; 			     filename
;; 			     newname))
;; 	res)
;;     (set-process-sentinel proc 'sftp-copy-file-locally-sentinel)
;;     (process-kill-without-query proc)
;;     (with-current-buffer (process-buffer proc)
;;       (set (make-local-variable 'copy-cont) cont))))
;;
;; (defun sftp-copy-file-locally-sentinel (proc status)
;;   (with-current-buffer (process-buffer proc)
;;     (let ((cont copy-cont)
;; 	  (result (buffer-string)))
;;       (unwind-protect
;; 	  (if (and (string-equal status "finished\n")
;; 		   (zerop (length result)))
;; 	      (sftp-call-cont cont t nil)
;; 	    (sftp-call-cont cont
;; 				nil
;; 				(if (zerop (length result))
;; 				    (substring status 0 -1)
;; 				  (substring result 0 -1))))
;; 	(kill-buffer (current-buffer))))))

;; this is the extended version of sftp-copy-file-internal that works
;; asynchronously if asked nicely.
(defun sftp-copy-file-internal (filename newname ok-if-already-exists
					     keep-date &optional msg cont nowait)
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))

  (or (file-exists-p filename)
      (signal 'file-error
	      (list "Copy file" "no such file or directory" filename)))

  ;; canonicalize newname if a directory.
  (if (file-directory-p newname)
      (setq newname (expand-file-name (file-name-nondirectory filename) newname)))

  (let ((f-parsed (sftp-ftp-name filename))
	(t-parsed (sftp-ftp-name newname)))

    ;; local file to local file copy?
    (if (and (not f-parsed) (not t-parsed))
	(progn
	  (sftp-real-copy-file filename newname ok-if-already-exists
				   keep-date)
	  (if cont
	      (sftp-call-cont cont t "Copied locally")))
      ;; one or both files are remote.
      (let* ((f-host (and f-parsed (nth 0 f-parsed)))
	     (f-user (and f-parsed (nth 1 f-parsed)))
	     (f-name (and f-parsed (sftp-quote-string (nth 2 f-parsed))))
	     (f-abbr (sftp-abbreviate-filename filename))
	     (t-host (and t-parsed (nth 0 t-parsed)))
	     (t-user (and t-parsed (nth 1 t-parsed)))
	     (t-name (and t-parsed (sftp-quote-string (nth 2 t-parsed))))
	     (t-abbr (sftp-abbreviate-filename newname filename))
	     temp1
	     temp2)

	;; check to see if we can overwrite
	(if (or (not ok-if-already-exists)
		(numberp ok-if-already-exists))
	    (sftp-barf-or-query-if-file-exists newname "copy to it"
						   (numberp ok-if-already-exists)))

	;; do the copying.
	(if f-parsed

	    ;; filename was remote.
	    (progn
	      (if (or (sftp-use-gateway-p f-host)
		      t-parsed)
		  ;; have to use intermediate file if we are getting via
		  ;; gateway machine or we are doing a remote to remote copy.
		  (setq temp1 (sftp-make-tmp-name f-host)))
	      
	      (sftp-send-cmd
	       f-host
	       f-user
	       (list 'get f-name (or temp1 (sftp-quote-string newname)))
	       (or msg
		   (if (and temp1 t-parsed)
		       (format "Getting %s" f-abbr)
		     (format "Copying %s to %s" f-abbr t-abbr)))
	       (list 'sftp-cf1
		     filename newname msg
		     f-parsed f-host f-user f-name f-abbr
		     t-parsed t-host t-user t-name t-abbr
		     temp1 temp2 cont nowait)
	       nowait))

	  ;; filename wasn't remote.  newname must be remote.  call the
	  ;; function which does the remainder of the copying work.
	  (sftp-cf1 t nil
			filename newname msg
			f-parsed f-host f-user f-name f-abbr
			t-parsed t-host t-user t-name t-abbr
			nil nil cont nowait))))))

(defvar sftp-waiting-flag nil)

;; next part of copying routine.
(defun sftp-cf1 (result line
			    filename newname msg
			    f-parsed f-host f-user f-name f-abbr
			    t-parsed t-host t-user t-name t-abbr
			    temp1 temp2 cont nowait)
  (if line
      ;; filename must have been remote, and we must have just done a GET.
      (unwind-protect
	  (or result
	      ;; GET failed for some reason.  Clean up and get out.
	      (progn
		(and temp1 (sftp-del-tmp-name temp1))
		(or cont
		    (if sftp-waiting-flag
			(throw 'ftp-error t)
		      (signal 'ftp-error
			      (list "Opening input file"
				    (format "FTP Error: \"%s\"" line)
				    filename))))))))

  (if result
      ;; We now have to copy either temp1 or filename to newname.
      (if t-parsed

	  ;; newname was remote.
	  (progn
	    (if (sftp-use-gateway-p t-host)
		(setq temp2 (sftp-make-tmp-name t-host)))

	    ;; make sure data is moved into the right place for the
	    ;; outgoing transfer.  gateway temporary files complicate
	    ;; things nicely.
	    (if temp1
		(if temp2
		    (if (string-equal temp1 temp2)
			(setq temp1 nil)
		      (sftp-real-copy-file temp1 temp2 t))
		  (setq temp2 temp1 temp1 nil))
	      (if temp2
		  (sftp-real-copy-file filename temp2 t)))
	    
	    (sftp-send-cmd
	     t-host
	     t-user
	     (list 'put (or temp2 (sftp-quote-string filename)) t-name)
	     (or msg
		 (if (and temp2 f-parsed)
		     (format "Putting %s" newname)
		   (format "Copying %s to %s" f-abbr t-abbr)))
	     (list 'sftp-cf2
		   newname t-host t-user temp1 temp2 cont)
	     nowait))

	;; newname wasn't remote.
	(sftp-cf2 t nil newname t-host t-user temp1 temp2 cont))

    ;; first copy failed, tell caller
    (sftp-call-cont cont result line)))

;; last part of copying routine.
(defun sftp-cf2 (result line newname t-host t-user temp1 temp2 cont)
  (unwind-protect
      (if line
	  ;; result from doing a local to remote copy.
	  (unwind-protect
	      (progn
		(or result
		    (or cont
			(if sftp-waiting-flag
			    (throw 'ftp-error t)
			  (signal 'ftp-error
				  (list "Opening output file"
					(format "FTP Error: \"%s\"" line)
					newname)))))

		(sftp-add-file-entry newname)))
	;; newname was local.
	(if temp1
	    (sftp-real-copy-file temp1 newname t)))

    ;; clean up
    (and temp1 (sftp-del-tmp-name temp1))
    (and temp2 (sftp-del-tmp-name temp2))
    (sftp-call-cont cont result line)))

(defun sftp-copy-file (filename newname &optional ok-if-already-exists
				    keep-date preserve-uid-gid
				    preserve-selinux-context)
  (interactive "fCopy file: \nFCopy %s to file: \np")
  (sftp-copy-file-internal filename
			       newname
			       ok-if-already-exists
			       keep-date
			       nil
			       nil
			       (called-interactively-p 'interactive)))

(defun sftp-copy-files-async (okay-p line verbose-p files)
  "Copy some files in the background.
OKAY-P must be t, and LINE does not matter.  They are here to make this
 function a valid CONT argument for `sftp-raw-send-cmd'.
If VERBOSE-P is non-nil, print progress report in the echo area.
 When all the files have been copied already, a message is shown anyway.
FILES is a list of files to copy in the form
  (from-file to-file ok-if-already-exists keep-date)
E.g.,
  (sftp-copy-files-async t nil t '((\"a\" \"b\" t t) (\"c\" \"d\" t t)))"
  (unless okay-p (error "%s: %s" 'sftp-copy-files-async line))
  (if files
      (let* ((ff (car files))
             (from-file    (nth 0 ff))
             (to-file      (nth 1 ff))
             (ok-if-exists (nth 2 ff))
             (keep-date    (nth 3 ff)))
        (sftp-copy-file-internal
         from-file to-file ok-if-exists keep-date
         (and verbose-p (format "%s --> %s" from-file to-file))
         (list 'sftp-copy-files-async verbose-p (cdr files))
         t))
    (message "%s: done" 'sftp-copy-files-async)))


;;;; ------------------------------------------------------------
;;;; File renaming support.
;;;; ------------------------------------------------------------

(defun sftp-rename-remote-to-remote (filename newname f-parsed t-parsed)
  "Rename remote file FILENAME to remote file NEWNAME."
  (let ((f-host (nth 0 f-parsed))
	(f-user (nth 1 f-parsed))
	(t-host (nth 0 t-parsed))
	(t-user (nth 1 t-parsed)))
    (if (and (string-equal f-host t-host)
	     (string-equal f-user t-user))
	(let* ((f-name (sftp-quote-string (nth 2 f-parsed)))
	       (t-name (sftp-quote-string (nth 2 t-parsed)))
	       (cmd (list 'rename f-name t-name))
	       (fabbr (sftp-abbreviate-filename filename))
	       (nabbr (sftp-abbreviate-filename newname filename))
	       (result (sftp-send-cmd f-host f-user cmd
					  (format "Renaming %s to %s"
						  fabbr
						  nabbr))))
	  (or (car result)
	      (signal 'ftp-error
		      (list
		       "Renaming"
		       (format "FTP Error: \"%s\"" (cdr result))
		       filename
		       newname)))
	  (sftp-add-file-entry newname)
	  (sftp-delete-file-entry filename))
      (sftp-copy-file-internal filename newname t nil)
      (delete-file filename))))

(defun sftp-rename-local-to-remote (filename newname)
  "Rename local file FILENAME to remote file NEWNAME."
  (let* ((fabbr (sftp-abbreviate-filename filename))
	 (nabbr (sftp-abbreviate-filename newname filename))
	 (msg (format "Renaming %s to %s" fabbr nabbr)))
    (sftp-copy-file-internal filename newname t nil msg)
    (let (sftp-process-verbose)
      (delete-file filename))))

(defun sftp-rename-remote-to-local (filename newname)
  "Rename remote file FILENAME to local file NEWNAME."
  (let* ((fabbr (sftp-abbreviate-filename filename))
	 (nabbr (sftp-abbreviate-filename newname filename))
	 (msg (format "Renaming %s to %s" fabbr nabbr)))
    (sftp-copy-file-internal filename newname t nil msg)
    (let (sftp-process-verbose)
      (delete-file filename))))

(defun sftp-rename-file (filename newname &optional ok-if-already-exists)
  (interactive "fRename file: \nFRename %s to file: \np")
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  (let* ((f-parsed (sftp-ftp-name filename))
	 (t-parsed (sftp-ftp-name newname)))
    (if (and (or f-parsed t-parsed)
	     (or (not ok-if-already-exists)
		 (numberp ok-if-already-exists)))
	(sftp-barf-or-query-if-file-exists
	 newname
	 "rename to it"
	 (numberp ok-if-already-exists)))
    (if f-parsed
	(if t-parsed
	    (sftp-rename-remote-to-remote filename newname f-parsed
					      t-parsed)
	  (sftp-rename-remote-to-local filename newname))
      (if t-parsed
	  (sftp-rename-local-to-remote filename newname)
	(sftp-real-rename-file filename newname ok-if-already-exists)))))

;;;; ------------------------------------------------------------
;;;; File name completion support.
;;;; ------------------------------------------------------------

;; If the file entry is not a directory (nor a symlink pointing to a directory)
;; returns whether the file (or file pointed to by the symlink) is ignored
;; by completion-ignored-extensions.
;; Note that `sftp-this-dir' and `sftp-completion-ignored-pattern'
;; are used as free variables.
(defun sftp-file-entry-not-ignored-p (symname val)
  (if (stringp val)
      (let ((file (sftp-expand-symlink val sftp-this-dir)))
	(or (file-directory-p file)
	    (and (file-exists-p file)
		 (not (string-match sftp-completion-ignored-pattern
				    symname)))))
    (or val				; is a directory name
	(not (string-match sftp-completion-ignored-pattern symname)))))

(defun sftp-root-dir-p (dir)
  ;; Maybe we should use something more like
  ;; (equal dir (file-name-directory (directory-file-name dir)))  -stef
  (or (and (eq system-type 'windows-nt)
	   (string-match "\\`[a-zA-Z]:[/\\]\\'" dir))
      (string-equal "/" dir)))

(defun sftp-file-name-all-completions (file dir)
  (let ((sftp-this-dir (expand-file-name dir)))
    (if (sftp-ftp-name sftp-this-dir)
	(progn
	  (sftp-barf-if-not-directory sftp-this-dir)
	  (setq sftp-this-dir
		(sftp-real-file-name-as-directory sftp-this-dir))
	  (let* ((tbl (sftp-get-files sftp-this-dir))
		 (completions (all-completions file tbl)))

	    ;; see whether each matching file is a directory or not...
	    (mapcar
             (lambda (file)
               (let ((ent (gethash file tbl)))
                 (if (and ent
                          (or (not (stringp ent))
                              (file-directory-p
                               (sftp-expand-symlink ent
                                                        sftp-this-dir))))
                     (concat file "/")
		   file)))
	     completions)))

      (if (sftp-root-dir-p sftp-this-dir)
	  (nconc (all-completions file (sftp-generate-root-prefixes))
		 (sftp-real-file-name-all-completions file
							  sftp-this-dir))
	(sftp-real-file-name-all-completions file sftp-this-dir)))))

(defun sftp-file-name-completion (file dir &optional predicate)
  (let ((sftp-this-dir (expand-file-name dir)))
    (if (sftp-ftp-name sftp-this-dir)
	(progn
	  (sftp-barf-if-not-directory sftp-this-dir)
	  (if (equal file "")
	      ""
	    (setq sftp-this-dir
		  (sftp-real-file-name-as-directory sftp-this-dir))	;real?
	    (let* ((tbl (sftp-get-files sftp-this-dir))
		   (sftp-completion-ignored-pattern
		    (mapconcat (lambda (s) (if (stringp s)
                                          (concat (regexp-quote s) "$")
                                        "/")) ; / never in filename
			       completion-ignored-extensions
			       "\\|")))
	      (save-match-data
		(or (sftp-file-name-completion-1
		     file tbl sftp-this-dir
		     'sftp-file-entry-not-ignored-p)
		    (sftp-file-name-completion-1
		     file tbl sftp-this-dir))))))

      (if (sftp-root-dir-p sftp-this-dir)
	  (try-completion
	   file
	   (nconc (sftp-generate-root-prefixes)
		  (sftp-real-file-name-all-completions
		   file sftp-this-dir))
	   predicate)
	(if predicate
	    (sftp-real-file-name-completion
	     file sftp-this-dir predicate)
	  (sftp-real-file-name-completion
	   file sftp-this-dir))))))


(defun sftp-file-name-completion-1 (file tbl dir &optional predicate)
  (let ((bestmatch (try-completion file tbl predicate)))
    (if bestmatch
	(if (eq bestmatch t)
	    (if (file-directory-p (expand-file-name file dir))
		(concat file "/")
	      t)
	  (if (and (eq (try-completion bestmatch tbl predicate) t)
		   (file-directory-p
		    (expand-file-name bestmatch dir)))
	      (concat bestmatch "/")
	    bestmatch)))))

;; Put these lines uncommented in your .emacs if you want C-r to refresh
;; sftp's cache whilst doing filename completion.
;;
;;(define-key minibuffer-local-completion-map "\C-r" 'sftp-re-read-dir)
;;(define-key minibuffer-local-must-match-map "\C-r" 'sftp-re-read-dir)

;;;###autoload
(defalias 'sftp-re-read-dir 'sftp-reread-dir)

;;;###autoload
(defun sftp-reread-dir (&optional dir)
  "Reread remote directory DIR to update the directory cache.
The implementation of remote FTP file names caches directory contents
for speed.  Therefore, when new remote files are created, Emacs
may not know they exist.  You can use this command to reread a specific
directory, so that Emacs will know its current contents."
  (interactive)
  (if dir
      (setq dir (expand-file-name dir))
    (setq dir (file-name-directory (expand-file-name (buffer-string)))))
  (if (sftp-ftp-name dir)
      (progn
	(setq sftp-ls-cache-file nil)
	(remhash dir sftp-files-hashtable)
	(sftp-get-files dir t))))

(defun sftp-make-directory (dir &optional parents)
  (interactive (list (expand-file-name (read-directory-name "Make directory: "))))
  (if parents
      (let ((parent (file-name-directory (directory-file-name dir))))
	(or (file-exists-p parent)
	    (sftp-make-directory parent parents))))
  (if (file-exists-p dir)
      (error "Cannot make directory %s: file already exists" dir)
    (let ((parsed (sftp-ftp-name dir)))
      (if parsed
	  (let* ((host (nth 0 parsed))
		 (user (nth 1 parsed))
		 ;; Some ftp's on unix machines (at least on Suns)
		 ;; insist that mkdir take a filename, and not a
		 ;; directory-name name as an arg. Argh!! This is a bug.
		 ;; Non-unix machines will probably always insist
		 ;; that mkdir takes a directory-name as an arg
		 ;; (as the ftp man page says it should).
		 (name (sftp-quote-string
			(if (eq (sftp-host-type host) 'unix)
			    (sftp-real-directory-file-name (nth 2 parsed))
			  (sftp-real-file-name-as-directory
			   (nth 2 parsed)))))
		 (abbr (sftp-abbreviate-filename dir))
		 (result (sftp-send-cmd host user
					    (list 'mkdir name)
					    (format "Making directory %s"
						    abbr))))
	    (or (car result)
		(sftp-error host user
				(format "Could not make directory %s: %s"
					dir
					(cdr result))))
	    (sftp-add-file-entry dir t))
	(sftp-real-make-directory dir)))))

(defun sftp-delete-directory (dir &optional recursive)
  (if (file-directory-p dir)
      (let ((parsed (sftp-ftp-name dir)))
	(if recursive
	    (mapc
	     (lambda (file)
	       (if (file-directory-p file)
		   (sftp-delete-directory file recursive)
		 (delete-file file)))
	     ;; We do not want to delete "." and "..".
	     (directory-files
	      dir 'full "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*")))
	(if parsed
	    (let* ((host (nth 0 parsed))
		   (user (nth 1 parsed))
		   ;; Some ftp's on unix machines (at least on Suns)
		   ;; insist that rmdir take a filename, and not a
		   ;; directory-name name as an arg. Argh!! This is a bug.
		   ;; Non-unix machines will probably always insist
		   ;; that rmdir takes a directory-name as an arg
		   ;; (as the ftp man page says it should).
		   (name (sftp-quote-string
			  (if (eq (sftp-host-type host) 'unix)
			      (sftp-real-directory-file-name
			       (nth 2 parsed))
			    (sftp-real-file-name-as-directory
			     (nth 2 parsed)))))
		   (abbr (sftp-abbreviate-filename dir))
		   (result
		    (progn
		      ;; CWD must not in this directory.
		      (sftp-cd host user "/" 'noerror)
		      (sftp-send-cmd host user
					 (list 'rmdir name)
					 (format "Removing directory %s"
						 abbr)))))
	      (or (car result)
		  (sftp-error host user
				  (format "Could not remove directory %s: %s"
					  dir
					  (cdr result))))
	      (sftp-delete-file-entry dir t))
	  (sftp-real-delete-directory dir recursive)))
    (error "Not a directory: %s" dir)))

;; Make a local copy of FILE and return its name.

(defun sftp-file-local-copy (file)
  (let* ((fn1 (expand-file-name file))
	 (pa1 (sftp-ftp-name fn1)))
    (if pa1
	(let ((tmp1 (sftp-make-tmp-name (car pa1)
					    (file-name-extension file t))))
	  (sftp-copy-file-internal fn1 tmp1 t nil
				       (format "Getting %s" fn1))
	  tmp1))))

(defun sftp-file-remote-p (file &optional identification connected)
  (let* ((parsed (sftp-ftp-name file))
	 (host (nth 0 parsed))
	 (user (nth 1 parsed))
	 (localname (nth 2 parsed)))
    (and (or (not connected)
	     (let ((proc (get-process (sftp-ftp-process-buffer host user))))
	       (and proc (processp proc)
		    (memq (process-status proc) '(run open)))))
	 (cond
	  ((eq identification 'method) (and parsed "ftp"))
	  ((eq identification 'user) user)
	  ((eq identification 'host) host)
	  ((eq identification 'localname) localname)
	  (t (sftp-replace-name-component file ""))))))

(defun sftp-load (file &optional noerror nomessage nosuffix)
  (if (sftp-ftp-name file)
      (let ((tryfiles (if nosuffix
			  (list file)
			(list (concat file ".elc") (concat file ".el") file)))
	    ;; make sure there are no references to temp files
	    (load-force-doc-strings t)
	    copy)
	(while (and tryfiles (not copy))
	  (catch 'ftp-error
	    (let ((sftp-waiting-flag t))
	      (condition-case error
		  (setq copy (sftp-file-local-copy (car tryfiles)))
		(ftp-error nil))))
	  (setq tryfiles (cdr tryfiles)))
	(if copy
	    (unwind-protect
		(funcall 'load copy noerror nomessage nosuffix)
	      (delete-file copy))
	  (or noerror
	      (signal 'file-error (list "Cannot open load file" file)))
	  nil))
    (sftp-real-load file noerror nomessage nosuffix)))

;; Calculate default-unhandled-directory for a given sftp buffer.
(defun sftp-unhandled-file-name-directory (filename)
  nil)


;; Need the following functions for making filenames of compressed
;; files, because some OS's (unlike UNIX) do not allow a filename to
;; have two extensions.

(defvar sftp-make-compressed-filename-alist nil
  "Alist of host-type-specific functions to process file names for compression.
Each element has the form (TYPE . FUNC).
FUNC should take one argument, a file name, and return a list
of the form (COMPRESSING NEWNAME).
COMPRESSING should be t if the specified file should be compressed,
and nil if it should be uncompressed (that is, if it is a compressed file).
NEWNAME should be the name to give the new compressed or uncompressed file.")

(declare-function dired-compress-file "dired-aux" (file))

(defun sftp-dired-compress-file (name)
  "Handler used by `dired-compress-file'."
  (let ((parsed (sftp-ftp-name name))
	conversion-func)
    (if (and parsed
	     (setq conversion-func
		   (cdr (assq (sftp-host-type (car parsed))
			      sftp-make-compressed-filename-alist))))
	(let* ((decision
		(save-match-data (funcall conversion-func name)))
	       (compressing (car decision))
	       (newfile (nth 1 decision)))
	  (if compressing
	      (sftp-compress name newfile)
	    (sftp-uncompress name newfile)))
      (let (file-name-handler-alist)
	(dired-compress-file name)))))

;; Copy FILE to this machine, compress it, and copy out to NFILE.
(defun sftp-compress (file nfile)
  (let* ((parsed (sftp-ftp-name file))
	 (tmp1 (sftp-make-tmp-name (car parsed)))
	 (tmp2 (sftp-make-tmp-name (car parsed)))
	 (abbr (sftp-abbreviate-filename file))
	 (nabbr (sftp-abbreviate-filename nfile))
	 (msg1 (format "Getting %s" abbr))
	 (msg2 (format "Putting %s" nabbr)))
    (unwind-protect
	(progn
	  (sftp-copy-file-internal file tmp1 t nil msg1)
	  (and sftp-process-verbose
	       (sftp-message "Compressing %s..." abbr))
	  (call-process-region (point)
			       (point)
			       shell-file-name
			       nil
			       t
			       nil
			       "-c"
			       (format "compress -f -c < %s > %s" tmp1 tmp2))
	  (and sftp-process-verbose
	       (sftp-message "Compressing %s...done" abbr))
	  (if (zerop (buffer-size))
	      (progn
		(let (sftp-process-verbose)
		  (delete-file file))
		(sftp-copy-file-internal tmp2 nfile t nil msg2))))
      (sftp-del-tmp-name tmp1)
      (sftp-del-tmp-name tmp2))))

;; Copy FILE to this machine, uncompress it, and copy out to NFILE.
(defun sftp-uncompress (file nfile)
  (let* ((parsed (sftp-ftp-name file))
	 (tmp1 (sftp-make-tmp-name (car parsed)))
	 (tmp2 (sftp-make-tmp-name (car parsed)))
	 (abbr (sftp-abbreviate-filename file))
	 (nabbr (sftp-abbreviate-filename nfile))
	 (msg1 (format "Getting %s" abbr))
	 (msg2 (format "Putting %s" nabbr)))
    (unwind-protect
	(progn
	  (sftp-copy-file-internal file tmp1 t nil msg1)
	  (and sftp-process-verbose
	       (sftp-message "Uncompressing %s..." abbr))
	  (call-process-region (point)
			       (point)
			       shell-file-name
			       nil
			       t
			       nil
			       "-c"
			       (format "uncompress -c < %s > %s" tmp1 tmp2))
	  (and sftp-process-verbose
	       (sftp-message "Uncompressing %s...done" abbr))
	  (if (zerop (buffer-size))
	      (progn
		(let (sftp-process-verbose)
		  (delete-file file))
		(sftp-copy-file-internal tmp2 nfile t nil msg2))))
      (sftp-del-tmp-name tmp1)
      (sftp-del-tmp-name tmp2))))

(defun sftp-find-backup-file-name (fn)
  ;; Either return the ordinary backup name, etc.,
  ;; or return nil meaning don't make a backup.
  (if sftp-make-backup-files
      (sftp-real-find-backup-file-name fn)))

;;; Define the handler for special file names
;;; that causes sftp to be invoked.

;;;###autoload
(defun sftp-hook-function (operation &rest args)
  (let ((fn (get operation 'sftp)))
    (if fn
	;; Catch also errors in process-filter.
	(condition-case err
	    (let ((debug-on-error t))
	      (save-match-data (apply fn args)))
	  (error (signal (car err) (cdr err))))
      (sftp-run-real-handler operation args))))

;; The following code is commented out because Tramp now deals with
;; Sftp filenames, too.

;;-;;; This regexp takes care of real sftp file names (with a slash
;;-;;; and colon).
;;-;;; Don't allow the host name to end in a period--some systems use /.:
;;-;;;###autoload
;;-(or (assoc "^/[^/:]*[^/:.]:" file-name-handler-alist)
;;-    (setq file-name-handler-alist
;;-	  (cons '("^/[^/:]*[^/:.]:" . sftp-hook-function)
;;-		file-name-handler-alist)))
;;-
;;-;;; This regexp recognizes absolute filenames with only one component,
;;-;;; for the sake of hostname completion.
;;-;;;###autoload
;;-(or (assoc "^/[^/:]*\\'" file-name-handler-alist)
;;-    (setq file-name-handler-alist
;;-	  (cons '("^/[^/:]*\\'" . sftp-completion-hook-function)
;;-		file-name-handler-alist)))
;;-
;;-;;; This regexp recognizes absolute filenames with only one component
;;-;;; on Windows, for the sake of hostname completion.
;;-;;; NB. Do not mark this as autoload, because it is very common to
;;-;;; do completions in the root directory of drives on Windows.
;;-(and (memq system-type '(ms-dos windows-nt))
;;-     (or (assoc "^[a-zA-Z]:/[^/:]*\\'" file-name-handler-alist)
;;-	 (setq file-name-handler-alist
;;-	       (cons '("^[a-zA-Z]:/[^/:]*\\'" .
;;-		       sftp-completion-hook-function)
;;-		     file-name-handler-alist))))

;;; The above two forms are sufficient to cause this file to be loaded
;;; if the user ever uses a file name with a colon in it.

;;; This sets the mode
(add-hook 'find-file-hook 'sftp-set-buffer-mode)

;;; Now say where to find the handlers for particular operations.

(put 'file-name-directory 'sftp 'sftp-file-name-directory)
(put 'file-name-nondirectory 'sftp 'sftp-file-name-nondirectory)
(put 'file-name-as-directory 'sftp 'sftp-file-name-as-directory)
(put 'directory-file-name 'sftp 'sftp-directory-file-name)
(put 'expand-file-name 'sftp 'sftp-expand-file-name)
(put 'make-directory 'sftp 'sftp-make-directory)
(put 'delete-directory 'sftp 'sftp-delete-directory)
(put 'insert-file-contents 'sftp 'sftp-insert-file-contents)
(put 'directory-files 'sftp 'sftp-directory-files)
(put 'directory-files-and-attributes 'sftp
     'sftp-directory-files-and-attributes)
(put 'file-directory-p 'sftp 'sftp-file-directory-p)
(put 'file-writable-p 'sftp 'sftp-file-writable-p)
(put 'file-readable-p 'sftp 'sftp-file-readable-p)
(put 'file-executable-p 'sftp 'sftp-file-executable-p)
(put 'file-symlink-p 'sftp 'sftp-file-symlink-p)
(put 'delete-file 'sftp 'sftp-delete-file)
(put 'verify-visited-file-modtime 'sftp
     'sftp-verify-visited-file-modtime)
(put 'file-exists-p 'sftp 'sftp-file-exists-p)
(put 'write-region 'sftp 'sftp-write-region)
(put 'copy-file 'sftp 'sftp-copy-file)
(put 'rename-file 'sftp 'sftp-rename-file)
(put 'file-attributes 'sftp 'sftp-file-attributes)
(put 'file-newer-than-file-p 'sftp 'sftp-file-newer-than-file-p)
(put 'file-name-all-completions 'sftp 'sftp-file-name-all-completions)
(put 'file-name-completion 'sftp 'sftp-file-name-completion)
(put 'insert-directory 'sftp 'sftp-insert-directory)
(put 'file-local-copy 'sftp 'sftp-file-local-copy)
(put 'file-remote-p 'sftp 'sftp-file-remote-p)
(put 'unhandled-file-name-directory 'sftp
     'sftp-unhandled-file-name-directory)
(put 'file-name-sans-versions 'sftp 'sftp-file-name-sans-versions)
(put 'dired-uncache 'sftp 'sftp-dired-uncache)
(put 'dired-compress-file 'sftp 'sftp-dired-compress-file)
(put 'load 'sftp 'sftp-load)
(put 'find-backup-file-name 'sftp 'sftp-find-backup-file-name)
(put 'set-file-modes 'sftp 'sftp-set-file-modes)

;; Turn off truename processing to save time.
;; Treat each name as its own truename.
(put 'file-truename 'sftp 'identity)

;; We must return non-nil in order to mask our inability to do the job.
;; Otherwise there are errors when applied to the target file during
;; copying from a (localhost) Tramp file.
(put 'set-file-times 'sftp 'ignore)

;; Turn off RCS/SCCS processing to save time.
;; This returns nil for any file name as argument.
(put 'vc-registered 'sftp 'null)

;; We can handle process-file in a restricted way (just for chown).
;; Nothing possible for `start-file-process'.
(put 'process-file 'sftp 'sftp-process-file)
(put 'start-file-process 'sftp 'ignore)
(put 'shell-command 'sftp 'sftp-shell-command)

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
(defun sftp-real-directory-file-name (&rest args)
  (sftp-run-real-handler 'directory-file-name args))
(defun sftp-real-expand-file-name (&rest args)
  (sftp-run-real-handler 'expand-file-name args))
(defun sftp-real-make-directory (&rest args)
  (sftp-run-real-handler 'make-directory args))
(defun sftp-real-delete-directory (&rest args)
  (sftp-run-real-handler 'delete-directory args))
(defun sftp-real-insert-file-contents (&rest args)
  (sftp-run-real-handler 'insert-file-contents args))
(defun sftp-real-directory-files (&rest args)
  (sftp-run-real-handler 'directory-files args))
(defun sftp-real-directory-files-and-attributes (&rest args)
  (sftp-run-real-handler 'directory-files-and-attributes args))
(defun sftp-real-file-directory-p (&rest args)
  (sftp-run-real-handler 'file-directory-p args))
(defun sftp-real-file-writable-p (&rest args)
  (sftp-run-real-handler 'file-writable-p args))
(defun sftp-real-file-readable-p (&rest args)
  (sftp-run-real-handler 'file-readable-p args))
(defun sftp-real-file-executable-p (&rest args)
  (sftp-run-real-handler 'file-executable-p args))
(defun sftp-real-file-symlink-p (&rest args)
  (sftp-run-real-handler 'file-symlink-p args))
(defun sftp-real-delete-file (&rest args)
  (sftp-run-real-handler 'delete-file args))
(defun sftp-real-verify-visited-file-modtime (&rest args)
  (sftp-run-real-handler 'verify-visited-file-modtime args))
(defun sftp-real-file-exists-p (&rest args)
  (sftp-run-real-handler 'file-exists-p args))
(defun sftp-real-write-region (&rest args)
  (sftp-run-real-handler 'write-region args))
(defun sftp-real-backup-buffer (&rest args)
  (sftp-run-real-handler 'backup-buffer args))
(defun sftp-real-copy-file (&rest args)
  (sftp-run-real-handler 'copy-file args))
(defun sftp-real-rename-file (&rest args)
  (sftp-run-real-handler 'rename-file args))
(defun sftp-real-file-attributes (&rest args)
  (sftp-run-real-handler 'file-attributes args))
(defun sftp-real-file-newer-than-file-p (&rest args)
  (sftp-run-real-handler 'file-newer-than-file-p args))
(defun sftp-real-file-name-all-completions (&rest args)
  (sftp-run-real-handler 'file-name-all-completions args))
(defun sftp-real-file-name-completion (&rest args)
  (sftp-run-real-handler 'file-name-completion args))
(defun sftp-real-insert-directory (&rest args)
  (sftp-run-real-handler 'insert-directory args))
(defun sftp-real-file-name-sans-versions (&rest args)
  (sftp-run-real-handler 'file-name-sans-versions args))
(defun sftp-real-shell-command (&rest args)
  (sftp-run-real-handler 'shell-command args))
(defun sftp-real-load (&rest args)
  (sftp-run-real-handler 'load args))
(defun sftp-real-find-backup-file-name (&rest args)
  (sftp-run-real-handler 'find-backup-file-name args))

;; Here we support using dired on remote hosts.
;; I have turned off the support for using dired on foreign directory formats.
;; That involves too many unclean hooks.
;; It would be cleaner to support such operations by
;; converting the foreign directory format to something dired can understand;
;; something close to ls -l output.
;; The logical place to do this is in the functions sftp-parse-...-listing.

;; Some of the old dired hooks would still be needed even if this is done.
;; I have preserved (and modernized) those hooks.
;; So the format conversion should be all that is needed.

;; When called from dired, SWITCHES may start with "--dired".
;; `sftp-ls' handles this.

(defun sftp-insert-directory (file switches &optional wildcard full)
  (if (not (sftp-ftp-name (expand-file-name file)))
      (sftp-real-insert-directory file switches wildcard full)
    ;; We used to follow symlinks on `file' here.  Apparently it was done
    ;; because some FTP servers react to "ls foo" by listing the symlink foo
    ;; rather than the directory it points to.  Now that sftp-ls uses
    ;; "cd foo; ls" instead, this is not necessary any more.
    (let ((beg (point))
	  (end (point-marker)))
      (set-marker-insertion-type end t)
      (insert
       (cond
	(wildcard
	 (let ((default-directory (file-name-directory file)))
	   (sftp-ls (file-name-nondirectory file) nil nil t)))
	(full
	 (sftp-ls file 'parse))
	(t
	 (setq file (directory-file-name file))
	 (let* ((dirlist (sftp-ls (or (file-name-directory file) ".") 'parse))
		(filename (file-name-nondirectory file))
		(case-fold-search nil))
	   ;; FIXME: This presumes a particular output format, which is
	   ;; basically Unix.
	   (if (string-match (concat "^.+[^ ] " (regexp-quote filename)
				     "\\( -> .*\\)?[@/*=]?\n") dirlist)
	       (match-string 0 dirlist)
	     "")))))

      ;; Insert "  " for dired's alignment sanity.
      (goto-char beg)
      (while (re-search-forward "^\\(\\S-\\)" end 'move)
	(replace-match "  \\1"))

      ;; The inserted file could be from somewhere else.
      (when (and (not wildcard) (not full)
		 (search-backward
		  (if (zerop (length (file-name-nondirectory
				      (expand-file-name file))))
		      "."
		    (file-name-nondirectory file))
		  nil 'noerror))
	(replace-match (file-relative-name (expand-file-name file)) t)
	(goto-char end))

      (set-marker end nil))))

(defun sftp-dired-uncache (dir)
  (if (sftp-ftp-name (expand-file-name dir))
      (setq sftp-ls-cache-file nil)))

(defvar sftp-sans-version-alist nil
  "Alist of mapping host type into function to remove file version numbers.")

(defun sftp-file-name-sans-versions (file keep-backup-version)
  (let* ((short (sftp-abbreviate-filename file))
	 (parsed (sftp-ftp-name short))
	 (func (if parsed (cdr (assq (sftp-host-type (car parsed))
                                     sftp-sans-version-alist)))))
    (if func (funcall func file keep-backup-version)
      (sftp-real-file-name-sans-versions file keep-backup-version))))

;; This is the handler for shell-command.
(defun sftp-shell-command (command &optional output-buffer error-buffer)
  (let* ((parsed (sftp-ftp-name default-directory))
	 (host (nth 0 parsed))
	 (user (nth 1 parsed))
	 (name (nth 2 parsed)))
    (if (not parsed)
	(sftp-real-shell-command command output-buffer error-buffer)
      (if (> (length name) 0)		; else it's $HOME
	  (setq command (concat "cd " name "; " command)))
      ;; Remove port from the hostname
      (when (string-match "\\(.*\\)#" host)
	(setq host (match-string 1 host)))
      (setq command
	    (format  "%s %s \"%s\""	; remsh -l USER does not work well
					; on a hp-ux machine I tried
		     remote-shell-program host command))
      (sftp-message "Remote command '%s' ..." command)
      ;; Cannot call sftp-real-dired-run-shell-command here as it
      ;; would prepend "cd default-directory" --- which bombs because
      ;; default-directory is in sftp syntax for remote file names.
      (sftp-real-shell-command command output-buffer error-buffer))))

;;; This is the handler for process-file.
(defun sftp-process-file (program infile buffer display &rest arguments)
  ;; PROGRAM is always one of those below in the cond in dired.el.
  ;; The ARGUMENTS are (nearly) always files.
  (if (sftp-ftp-name default-directory)
      ;; Can't use sftp-dired-host-type here because the current
      ;; buffer is *dired-check-process output*
      (condition-case oops
	  (cond ((equal (or (bound-and-true-p dired-chmod-program) "chmod")
			program)
		 (sftp-call-chmod arguments))
		;; ((equal "chgrp" program))
		;; ((equal dired-chown-program program))
		(t (error "Unknown remote command: %s" program)))
	(ftp-error (insert (format "%s: %s, %s\n"
				   (nth 1 oops)
				   (nth 2 oops)
				   (nth 3 oops)))
		   ;; Caller expects nonzero value to mean failure.
		   1)
	(error (insert (format "%s\n" (nth 1 oops)))
	       1))
    (apply 'call-process program infile buffer display arguments)))

;; Handle an attempt to run chmod on a remote file
;; by using the ftp chmod command.
(defun sftp-call-chmod (args)
  (if (< (length args) 2)
      (error "sftp-call-chmod: missing mode and/or filename: %s" args))
  (let ((mode (car args))
	(rest (cdr args)))
    (if (equal "--" (car rest))
	(setq rest (cdr rest)))
    (mapc
     (lambda (file)
       (setq file (expand-file-name file))
       (let ((parsed (sftp-ftp-name file)))
         (if parsed
             (let* ((host (nth 0 parsed))
                    (user (nth 1 parsed))
                    (name (sftp-quote-string (nth 2 parsed)))
                    (abbr (sftp-abbreviate-filename file))
                    (result (sftp-send-cmd host user
                                               (list 'chmod mode name)
                                               (format "doing chmod %s"
                                                       abbr))))
               (or (car result)
		   (sftp-error
		    host user (concat "CHMOD failed: " (cdr result))))))))
     rest))
  (setq sftp-ls-cache-file nil)	;Stop confusing Dired.
  0)

(defun sftp-set-file-modes (filename mode)
  (sftp-call-chmod (list (format "%o" mode) filename)))

(provide 'sftp)

;;; sftp.el ends here

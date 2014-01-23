;; init-shell: Shell/Comint initialization

;; TODO: http://snarfed.org/why_i_run_shells_inside_emacs

(setq comint-use-prompt-regexp nil)
(setq comint-use-prompt-regexp-instead-of-fields nil)

;; Newer versions of comint don't use prompt regexp anymore, but the
;; comint redirection commands (e.g. `comint-redirect-send-command')
;; still need this correctly set.
(setq shell-prompt-pattern "^[^\n]*[$#] ")

(setq shell-completion-fignore '("~" "#" "%"))
(setq comint-completion-autolist t)

(setq comint-input-ring-size 4000)
(setq comint-input-autoexpand nil)
(setq shell-input-autoexpand nil)
(setq comint-input-ignoredups t)
(setq comint-buffer-maximum-size (* 8 1024))

(setq comint-process-echoes nil)

(setq comint-input-ignored-regexps
      (list (rx bos (* (syntax whitespace)) eos)
            (rx bos (or "y" "yes" "n" "no" "j" "ja" "nein") eos)))

(setq comint-input-filter
      (lambda (str)
        (not
         (catch 'matched
           (dolist (rx comint-input-ignored-regexps)
             (and (string-match-p rx str)
                  (throw 'matched t)))))))

;; (add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(defun shell-bash-setup ()
  "*Bash specific setup for `shell-mode-hook'."
  (add-hook 'comint-preoutput-filter-functions 'shell-filter-ctrl-a-ctrl-b)
  (setq comint-input-ring-file-name
        (or (getenv "HISTFILE")
            (if (file-exists-p "~/.bash.d/.bash_history")
                "~/.bash.d/.bash_history"
              "~/.bash_history")))
  (comint-read-input-ring 'silent)
  (when (require-soft 'bacom)
    ;; (add-hook 'comint-dynamic-complete-functions 'bacom-dynamic-complete)
    (setq comint-dynamic-complete-functions '(bacom-dynamic-complete
                                              comint-complete-from-history))))

(defun comint-complete-from-history ()
  "*Complete symbol at point from history entries."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (and bounds
         (destructuring-bind (beg . end) bounds
           (let ((candidates
                  (let (lst)
                    (dotimes (idx (1- (ring-size comint-input-ring)) lst)
                      (setq lst
                            (nconc lst
                                   (save-match-data
                                     ;; split on whitespace or punctuation
                                     (split-string (ring-ref comint-input-ring idx)
                                                   "\\(?:\\s-\\|\\s.\\)+"
                                                   'omit-nulls))))))))
             (and candidates
                  (completion-in-region beg end candidates)))))))
(add-hook 'comint-dynamic-complete-functions 'comint-complete-from-history 'append)

(add-hook 'shell-mode-hook
          (lambda ()
            (let ((shell (file-name-sans-extension
                          (file-name-nondirectory
                           (car (process-command (get-buffer-process (current-buffer))))))))
              (when (string= shell "bash")
                (shell-bash-setup)))))

(add-hook 'shell-mode-hook
          (lambda ()
            (setq comint-scroll-to-bottom-on-output 'others)
            
            ;; sanitise the syntax table
            (modify-syntax-entry ?\, ".") ; punctuation
            (modify-syntax-entry ?\= ".") ; punctuation
            (modify-syntax-entry ?\@ "_") ; symbol
            (modify-syntax-entry ?\` "$") ; paired delimiters
            (modify-syntax-entry ?\# "<") ; comment starter
            (modify-syntax-entry ?\. "_")
            (modify-syntax-entry ?\: "_")
            (modify-syntax-entry ?\' "\"") ; string quote
            ;; Accept `,' as the first char of an abbrev
            (abbrev-table-put shell-mode-abbrev-table
                              :regexp "\\(?:[^[:word:],]\\|^\\)\\(,?[[:word:]]+\\)[^[:word:]]*")
            (abbrev-table-put shell-mode-abbrev-table :case-fixed t)
            (abbrev-mode 1)))

(defmacro define-shell-abbrev (abbrev expansion)
  "*Define ABBREV to expand to EXPANSION in `shell-mode'."
  `(define-abbrev shell-mode-abbrev-table ,abbrev ,expansion))

(define-shell-abbrev ",g" "| grep")
(define-shell-abbrev ",gi" "| grep -i")
(define-shell-abbrev ",gg" "2>&1 | grep")
(define-shell-abbrev ",s" "| sort ")
(define-shell-abbrev ",sn" "| sort --numeric-sort")
(define-shell-abbrev ",wcl" " | wc --lines")
(define-shell-abbrev ",nul" "/dev/null")
(define-shell-abbrev ",nn" "> /dev/null 2>&1")
(define-shell-abbrev ",t" "| tee")
(define-shell-abbrev ",tt" "2>&1| tee")
(define-shell-abbrev ",T" "| tail")
(define-shell-abbrev ",H" "| head")
(define-shell-abbrev ",h" "--help")
(define-shell-abbrev ",v" "--verbose")
(define-shell-abbrev ",V" "--version")
(define-shell-abbrev ",x" "| xargs")

(fset 'sh 'shell)

(defun shell-setup-keys ()
  "*Key bindings for shell-mode."
  (defkey shell-mode-map "C-x C-r" 'view-file))

(add-hook 'shell-mode-hook 'shell-setup-keys)

(ansi-color-map-update 'ansi-color-names-vector
                       ["black" "red2" "green3" "orange" "blue3" "magenta2" "cyan3" "white"])

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-hook 'shell-mode-hook 'pcomplete-shell-setup)

(when (require-soft 'pwdsync)
  (add-hook 'shell-mode-hook (lambda () (pwdsync-mode 1))))

(setq shell-dirtrack-verbose nil)

(defmacro with-buffer-hosting-pid (pid body)
  "*Execute BODY in the buffer associated with the process with id PID.
Return NIL if there is no buffer hosting a process with PID.
Otherwise return the value of the last form in BODY."
  (let ((proc (make-symbol "proc")))
    `(let ((,proc (catch 'found
                    (dolist (,proc (process-list))
                      (let ((pid (process-id ,proc)))
                        (when (and pid
                                   (= ,pid pid))
                          (throw 'found ,proc)))))))
       (and ,proc
            (with-current-buffer (process-buffer ,proc)
              ,body)))))

(when (require-soft 'helm-shell-history)
  (defkey comint-mode-map "C-c C-l" 'comint-helm-input-ring))


;;; http://www.masteringemacs.org/articles/2012/01/16/pcomplete-context-sensitive-completion-emacs/
(defconst pcmpl-svn-commands
  '("add" "blame" "praise" "annotate" "ann"
    "cat" "changelist" "cl" "checkout" "co"
    "cleanup" "commit" "ci" "copy" "cp"
    "delete" "del" "remove" "rm" "diff" "di"
    "export" "help" "h" "import" "info"
    "list" "ls" "lock" "log" "merge" "mergeinfo"
    "mkdir" "move" "mv" "rename" "ren"
    "propdel" "pdel" "pd" "propedit" "pedit" "pe"
    "propget" "pget" "pg" "proplist" "plist" "pl"
    "propset" "pset" "ps"
    "resolve" "resolved" "revert"
    "status" "stat" "st" "switch" "sw"
    "unlock" "update" "up" "upgrade"))

(defun pcomplete/svn ()
  "Completion for `svn'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-svn-commands)
  (cond
   ;; complete files/dirs forever if the command is `add' or `rm'.
   ((pcomplete-match (regexp-opt '("add" "delete" "del" "remove" "rm")) 1)
    (while (pcomplete-here (pcomplete-entries))))
   ((pcomplete-match "ls" 1)
    (let ((current (nth pcomplete-index pcomplete-args)))
      (pcomplete-here* (pcmpl-svn-get-files (or (file-name-directory current) "."))
                       (file-name-nondirectory current))))
   (t
    (while (pcomplete-here (pcomplete-entries))))))

(defun pcmpl-svn-get-files (dir)
  "Return a list of `svn' files in DIR."
  (split-string (pcomplete-process-result "svn" "--non-interactive" "ls" dir)))

(require-soft 'pcmpl-ack)

;;; Comint
(setq-default comint-scroll-to-bottom-on-input 'this)
(setq-default comint-scroll-show-maximum-output t)

;; http://www.emacswiki.org/emacs/ShellMode
(defun comint-clear-buffer ()
  "*Clear the process buffer, deleting its entire contents."
  (interactive "*")
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defkey comint-mode-map "C-S-l" 'comint-clear-buffer)

;; http://www.emacswiki.org/emacs-en/comint-kill-output-to-kill-ring.el
(defun comint-kill-output-to-kill-ring ()
  "Kill output from last command saving it in the kill ring."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
        (inhibit-read-only t))
    (save-excursion
      (let ((pmark (progn (goto-char (process-mark proc))
                          (forward-line 0)
                          (point-marker))))
        (kill-ring-save comint-last-input-end pmark)
        (goto-char (process-mark proc))))))

;; (defun comint-delete-output ()
;;   "Delete all output from interpreter since previous input.
;; Does not delete the prompt."
;;   (interactive)
;;   (let* ((beg (previous-single-char-property-change (point) 'field))
;;          (end (save-excursion
;;                 (goto-char (next-single-char-property-change beg 'field))
;;                 ;; can't use `point-at-bol' here due to field boundaries
;;                 (forward-line 0)
;;                 (point))))
;;     (unless (= beg end)
;;       (delete-region beg end)
;;       (save-excursion
;;         (goto-char beg)
;;         (insert "*** output flushed ***\n")))))
;;
;;
;; (progn
;;   (if (get-text-property (point) 'field) 
;;       (comint-previous-prompt 1)
;;     (goto-char (point-at-bol)))
;;   ())


;; (defun comint-insert-last-word ()
;;   "*Insert the last word from the previous history event at the cursor position."
;;   (interactive)
;;   (let (comint-input-ring-index)
;;     (insert (comint-arguments (comint-previous-input-string 0) nil nil))))

(defun comint-setup ()
  "*Setup for comint-mode."
  (defkey comint-mode-map "M-P" 'comint-previous-matching-input-from-input)
  (defkey comint-mode-map "M-N" 'comint-next-matching-input-from-input)

  (defkey comint-mode-map "C-M-p" 'comint-previous-prompt)
  (defkey comint-mode-map "C-M-n" 'comint-next-prompt)

  (defkey comint-mode-map "M-." 'comint-insert-previous-argument)
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
  (add-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom*)
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer))

(add-hook 'comint-mode-hook 'comint-setup)

(defun comint-postoutput-scroll-to-bottom* (_string)
  "Like `comint-postoutput-scroll-to-bottom' but ignores the minibuffer when considering `comint-move-point-for-output'."
  (let* ((current (current-buffer))
	 (process (get-buffer-process current)))
    (unwind-protect
	(cond
	 ((null process))
	 ((bound-and-true-p follow-mode)
	  (follow-comint-scroll-to-bottom))
	 (t
	  (let* ((this-window (selected-window))
                 ;; `selected' is the selected window, unless we are in
                 ;; the minibuffer, in which case it is the window
                 ;; that was selected as the minibuffer was entered.
                 (selected (if (minibuffer-window-active-p this-window) 
                               (minibuffer-selected-window)
                             this-window))) 
	    (dolist (w (get-buffer-window-list current nil t))
	      (select-window w)
	      (unwind-protect
		  (progn
		    (comint-adjust-point selected)
		    ;; Optionally scroll to the bottom of the window.
		    (and comint-scroll-show-maximum-output
			 (eobp)
			 (recenter (- -1 scroll-margin))))
		(select-window this-window))))))
      (set-buffer current))))

;; added the "for '[^']+'" bits for git
(setq comint-password-prompt-regexp
      (concat
       "\\(^ *\\|"
       (regexp-opt
        '("Enter" "enter" "Enter same" "enter same" "Enter the" "enter the"
          "Old" "old" "New" "new" "'s" "login"
          "Kerberos" "CVS" "UNIX" " SMB" "LDAP" "[sudo]" "Repeat" "Bad") t)
       " +\\)"
       (regexp-opt
        '("password" "Password" "passphrase" "Passphrase"
          "pass phrase" "Pass phrase" "Response"))
       "\\(?:\\(?:, try\\)? *again\\| (empty for no passphrase)\\| (again)\\)?\
\\(?: for [^:]+\\| for '[^']+'\\)?:\\s *\\'"))

(provide 'init-shell)

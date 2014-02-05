;;; init-shell.el --- Shell/Comint initialization

;; Author: Emílio Lopes <eclig@gmx.net>

;; TODO: 
;;
;; * Work through http://snarfed.org/why_i_run_shells_inside_emacs


;;; General setup

(setq comint-use-prompt-regexp nil)
(setq comint-use-prompt-regexp-instead-of-fields nil)

;; Newer versions of comint don't use prompt regexp anymore, but the
;; comint redirection commands (e.g. `comint-redirect-send-command')
;; still need this correctly set.
(setq shell-prompt-pattern "^[^\n]*[$#] ")

(setq comint-process-echoes nil)

(setq comint-buffer-maximum-size (* 8 1024))

;; (add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(defun shell= (name)
  (let* ((process (get-buffer-process (current-buffer)))
         (shell (and process
                     (file-name-sans-extension
                      (file-name-nondirectory
                       (car (process-command process)))))))
    (and (stringp shell) (string= shell name))))

(fset 'sh 'shell)


;;; History and input filters

(setq shell-input-autoexpand nil)
(setq comint-input-autoexpand nil)
(setq comint-input-ring-size 4000)
(setq comint-input-ignoredups t)

(defvar comint-input-ignored-regexps
  (list (rx bos (* (syntax whitespace)) eos)
        (rx bos (or "y" "yes" "n" "no" "j" "ja" "nein") eos)))

(setq comint-input-filter
      (lambda (str)
        (not
         (catch 'matched
           (dolist (rx comint-input-ignored-regexps)
             (and (string-match-p rx str)
                  (throw 'matched t)))))))

(add-hook 'shell-mode-hook
          (lambda ()
            (when (shell= "bash")
              (setq comint-input-ring-file-name
                    (or (getenv "HISTFILE")
                        (if (file-exists-p "~/.bash.d/.bash_history")
                            "~/.bash.d/.bash_history"
                          "~/.bash_history")))
              (comint-read-input-ring 'silent))))

(when (require-soft 'helm-shell-history)
  (defkey comint-mode-map "C-c C-l" 'comint-helm-input-ring))


;;; Output filters

(setq-default comint-scroll-show-maximum-output t)
(setq-default comint-scroll-to-bottom-on-input 'this)

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

(add-hook 'comint-mode-hook
          (lambda ()
            (setq comint-scroll-to-bottom-on-output 'others)
            (when (shell= "bash")
              (add-hook 'comint-preoutput-filter-functions 'shell-filter-ctrl-a-ctrl-b))
            (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
            (add-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom*)
            (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)))


;;; Completion

(setq shell-completion-fignore '("~" "#" "%"))
(setq comint-completion-autolist t)

(add-to-path 'load-path (concat user-emacs-directory "lib/bacom"))

(add-hook 'shell-mode-hook
          (lambda ()
            (when (and (shell= "bash")
                       (require-soft 'bacom))
              (setq completion-at-point-functions '(bacom-dynamic-complete)))))

;; BEWARE: As of Emacs 24.3 `completion-at-point' does not work
;; correctly for non-prefix completion (like "partial-completion" or
;; "substring") in conjunction with completion functions that are
;; non-exclusive.  See comments in `completion--capf-wrapper'.
;;
;; To give an example, suppose you have the symbol `partial-completion'
;; in `completion-styles' and the shell buffer contains the text
;;
;;     somecmd --a-lo-opt
;;
;; Further the string "--a-long-option" can be found in
;; `comint-input-ring'.  With the point at the end of the line,
;; hitting <TAB> won't complete because "--a-lo-opt" is not a prefix
;; of "--a-long-option".  But if the point is right after "--a-" it
;; will complete correctly, since "--a-" is a prefix of "--a-long-option".
(defun comint-complete-from-history ()
  "*Complete symbol at point from history entries."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (and bounds
         (let* ((beg (car bounds))
                (end (cdr bounds))
                (stub (buffer-substring-no-properties beg end))
                (rx (rx (+ (or (syntax whitespace)
                               (syntax punctuation)
                               (syntax open-parenthesis)
                               (syntax close-parenthesis)
                               (syntax string-quote)))))
                candidates) 
           (dotimes (index (1- (ring-size comint-input-ring)))
             (let ((history-entry (ring-ref comint-input-ring index)))
               ;; To allow for fancier completion styles such as
               ;; "partial-completion" and "substring" we do not try
               ;; to filter matching candidates here.  Just return
               ;; everything we find and let the completion engine do
               ;; the work according to `completion-styles'.
               (setq candidates (nconc candidates (split-string history-entry rx 'omit-nulls)))))
           (and candidates
                (list beg end candidates :exclusive 'no))))))

(require-soft 'pcmpl-ack)

;; In a perfect world we would just add the snippet bellow to
;; `shell-mode-hook'.  But as of Emacs 24.3 "hippie-expand" has a bug
;; so that making `hippie-expand-ignore-buffers' and
;; `hippie-expand-only-buffers' buffer-local does not have the desired
;; effect.
;;
;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'hippie-expand-verbose) nil)
;;             (set (make-local-variable 'hippie-expand-dabbrev-as-symbol) t)
;;             (set (make-local-variable 'hippie-expand-only-buffers) '(shell-mode))
;;             (set (make-local-variable 'hippie-expand-try-functions-list)
;;                  '(try-expand-dabbrev
;;                    try-expand-dabbrev-all-buffers
;;                    try-complete-file-name-partially
;;                    try-complete-file-name))))

(defun shell-hippie-expand (arg)
  (interactive "P")
  (require 'hippie-exp)
  (let ((hippie-expand-verbose nil)
        (hippie-expand-dabbrev-as-symbol t)
        (hippie-expand-only-buffers '(shell-mode))
        (hippie-expand-try-functions-list
         '(try-expand-dabbrev
           try-expand-dabbrev-all-buffers
           try-complete-file-name-partially
           try-complete-file-name)))
    (hippie-expand arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map [remap hippie-expand] 'shell-hippie-expand)))


;;; Abbreviations

(define-abbrev shell-mode-abbrev-table ",g" "| grep")
(define-abbrev shell-mode-abbrev-table ",gi" "| grep -i")
(define-abbrev shell-mode-abbrev-table ",gg" "2>&1 | grep")
(define-abbrev shell-mode-abbrev-table ",s" "| sort ")
(define-abbrev shell-mode-abbrev-table ",sn" "| sort --numeric-sort")
(define-abbrev shell-mode-abbrev-table ",wcl" " | wc --lines")
(define-abbrev shell-mode-abbrev-table ",nul" "/dev/null")
(define-abbrev shell-mode-abbrev-table ",nn" "> /dev/null 2>&1")
(define-abbrev shell-mode-abbrev-table ",t" "| tee")
(define-abbrev shell-mode-abbrev-table ",tt" "2>&1| tee")
(define-abbrev shell-mode-abbrev-table ",T" "| tail")
(define-abbrev shell-mode-abbrev-table ",H" "| head")
(define-abbrev shell-mode-abbrev-table ",h" "--help")
(define-abbrev shell-mode-abbrev-table ",v" "--verbose")
(define-abbrev shell-mode-abbrev-table ",V" "--version")
(define-abbrev shell-mode-abbrev-table ",x" "| xargs")

(add-hook 'shell-mode-hook
          (lambda ()
            ;; sanitise the syntax table
            (modify-syntax-entry ?\, ".") ; punctuation
            (modify-syntax-entry ?\= ".") ; punctuation
            (modify-syntax-entry ?\& ".") ; punctuation
            (modify-syntax-entry ?\> ".") ; punctuation
            (modify-syntax-entry ?\< ".") ; punctuation
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


;;; Colors in shell buffers

(ansi-color-map-update 'ansi-color-names-vector
                       ["black" "red2" "green3" "orange" "blue3" "magenta2" "cyan3" "white"])

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;;; Directory tracking

(when (require-soft 'pwdsync)
  (add-hook 'shell-mode-hook (lambda () (pwdsync-mode 1))))

(setq shell-dirtrack-verbose nil)


;;; Interaction between Emacs and the shell

;; emacsclient --eval "(with-buffer-hosting-pid $$ (comint-read-input-ring))"
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


;;; Manipulating the contents of the shell buffer

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


;;; Miscellaneuous keybindings

(add-hook 'comint-mode-hook
          (lambda ()
            (defkey comint-mode-map "M-P" 'comint-previous-matching-input-from-input)
            (defkey comint-mode-map "M-N" 'comint-next-matching-input-from-input)

            (defkey comint-mode-map "C-M-p" 'comint-previous-prompt)
            (defkey comint-mode-map "C-M-n" 'comint-next-prompt)

            (defkey comint-mode-map "M-." 'comint-insert-previous-argument)))



(provide 'init-shell)
;;; init-shell.el ends here

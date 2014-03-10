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
        (rx bos (+ (syntax whitespace)))
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

(defun ido-comint-insert-matching-input ()
  "Insert a previous history item, using `ido' to search through earlier input."
  (interactive)
  (unless (comint-after-pmark-p)
    (user-error "Not at command line"))
  (let ((item (ido-completing-read "History item matching: "
                                   (delete-dups
                                    (ring-elements comint-input-ring))
                                   nil t)))
   (when item
     (unless comint-input-ring-index
       (setq comint-stored-incomplete-input
             (funcall comint-get-old-input)))
     (comint-delete-input)
     (insert item))))

(defkey comint-mode-map "C-c C-l" 'ido-comint-insert-matching-input)

;; Thank you, Joe Bloggs.
;; http://www.emacswiki.org/emacs/ComintMode
(defun comint-end-of-input-history ()
  "Move to the end of the input history.
This corresponds to the line currently being entered."
  (interactive)
  (comint-restore-input))

(defkey comint-mode-map "C-c M->" 'comint-end-of-input-history)

(defun comint-beginning-of-input-history ()
  "Move to the first line in the input history."
  (interactive)
  (comint-next-input (1+ (or comint-input-ring-index 0))))

(defkey comint-mode-map "C-c M-<" 'comint-beginning-of-input-history)


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
            (add-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom*)))


;;; Completion

(setq shell-completion-fignore '("~" "#" "%"))
(setq comint-completion-autolist t)

(add-to-path 'load-path (concat user-emacs-directory "lib/bashcomp"))

(add-hook 'shell-mode-hook
          (lambda ()
            (setq-local completion-cycle-threshold 4)
            (when (and (shell= "bash")
                       (require-soft 'bashcomp))
              (setq completion-at-point-functions '(bashcomp-completion-at-point
                                                    bashcomp-wordbreak-completion-at-point)))))

(defun try-complete-from-comint-history (old)
  "*Complete symbol at point from comint history entries.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (unless old
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (and bounds
           (let* ((beg (car bounds))
                  (end (point))
                  (stub (buffer-substring-no-properties beg end))
                  (rx (rx (+ (or (syntax whitespace)
                                 (syntax punctuation)
                                 (syntax open-parenthesis)
                                 (syntax close-parenthesis)
                                 (syntax string-quote)))))
                  candidates)
             (he-init-string beg end)
             (unless (he-string-member he-search-string he-tried-table)
               (push he-search-string he-tried-table))
             (unless (string= he-search-string "")
               (dotimes (index (1- (ring-size comint-input-ring)))
                 (let* ((history-entry (ring-ref comint-input-ring index))
                        (matches (all-completions stub (split-string history-entry rx 'omit-nulls))))
                   (when matches
                     (setq candidates (nconc candidates matches))))))
             (setq he-expand-list candidates)))))

  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (pop he-expand-list))

  (if (null he-expand-list)
      (and old (he-reset-string) nil)
    (he-substitute-string (pop he-expand-list))
    t))

;; In a perfect world we would just add the snippet bellow to
;; `shell-mode-hook'.  But as of Emacs 24.3 "hippie-expand" has a bug
;; so that making `hippie-expand-ignore-buffers' and
;; `hippie-expand-only-buffers' buffer-local does not have the desired
;; effect.
;;
;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;             (setq-local hippie-expand-verbose nil)
;;             (setq-local hippie-expand-dabbrev-as-symbol t)
;;             (setq-local hippie-expand-only-buffers '(shell-mode))
;;             (setq-local hippie-expand-try-functions-list
;;                         '(try-expand-dabbrev
;;                           try-expand-dabbrev-all-buffers
;;                           try-complete-file-name-partially
;;                           try-complete-file-name))))

(defun shell-hippie-expand (arg)
  (interactive "P")
  (require 'hippie-exp)
  (let ((hippie-expand-verbose nil)
        (hippie-expand-dabbrev-as-symbol nil)
        (hippie-expand-only-buffers '(shell-mode))
        (hippie-expand-try-functions-list
         '(try-complete-from-comint-history
           try-expand-dabbrev
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
(define-abbrev shell-mode-abbrev-table ",p" "| emacspipe")
(define-abbrev shell-mode-abbrev-table ",po" "| emacspipe -o")
(define-abbrev shell-mode-abbrev-table ",t" "| tee")
(define-abbrev shell-mode-abbrev-table ",tt" "2>&1| tee")
(define-abbrev shell-mode-abbrev-table ",T" "| tail")
(define-abbrev shell-mode-abbrev-table ",H" "| head")
(define-abbrev shell-mode-abbrev-table ",h" "--help")
(define-abbrev shell-mode-abbrev-table ",v" "--verbose")
(define-abbrev shell-mode-abbrev-table ",vv" "--version")
(define-abbrev shell-mode-abbrev-table ",x" "| xargs")
(define-abbrev shell-mode-abbrev-table ",x0" "| xargs --null")

(dotimes (i 9)
  (define-abbrev shell-mode-abbrev-table (format ",%d" (1+ i)) (format "~+%d" (1+ i))))

(define-abbrev shell-mode-abbrev-table ",uu" "../..") ; `u' as in `up'
(define-abbrev shell-mode-abbrev-table ",uuu" "../../..")
(define-abbrev shell-mode-abbrev-table ",uuuu" "../../../..")

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
                       ["black" "red3" "green4" "orange" "blue2" "magenta2" "cyan3" "gray90"])

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

;; Zsh's `copy-prev-shell-word'
(defun comint-insert-previous-word ()
  "*Duplicate the word to the left of the cursor, obeying quoting."
  (interactive)
  (insert (comint-arguments
           (buffer-substring-no-properties (comint-line-beginning-position)
                                           (point))
           nil nil)))

(defkey comint-mode-map "C-c ^" 'comint-insert-previous-word)

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

(defun comint-boundaries-of-previous-output ()
  ;; TODO: arg n
  ;; TODO: clean up this mess
  (let* ((pt (point))
         (beg (if (and (eq (get-char-property pt 'field) 'output)
                       (eq (get-char-property (1- pt) 'field) 'boundary))
                  pt
                (while (and
                        (setq prev-pt pt)
                        (setq pt (previous-single-char-property-change pt 'field))
                        (not (or (= pt prev-pt)
                                 (eq (get-char-property pt 'field) 'output)))))
                pt))
         (end (save-excursion
                (goto-char (next-single-char-property-change beg 'field))
                ;; can't use `point-at-bol' here due to field boundaries
                (forward-line 0)
                (if (> (point) 0) (1- (point)) (point)))))
    (list beg end)))

(defun comint-copy-previous-output ()
  ;; TODO: arg n
  (interactive)
  (destructuring-bind (beg end) (comint-boundaries-of-previous-output)
    (when (> end beg)
      (copy-region-as-kill beg end))))

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

(defvar shell-man-function #'woman
  "*Function run to get help for a given command.
This variable should be usually set to `man' or `woman'.")

(defun shell-man ()
  "*Run `woman' for the command in the current command line."
  (interactive)
  (let* ((end (if (search-forward-regexp (regexp-opt-charset comint-delimiter-argument-list) nil t)
                  (match-beginning 0) (point)))
         (cmdline (buffer-substring-no-properties (comint-line-beginning-position) end))
         (this-cmd (or (car (comint-delim-arg cmdline)) ""))
         (prog (comint-arguments this-cmd 0 0)))
    (if (zerop (length prog))
        (error "No command found")
      (funcall shell-man-function prog))))

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

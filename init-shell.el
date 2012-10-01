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

;; (add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(add-hook 'shell-mode-hook
          (lambda ()
            (let ((shell (file-name-sans-extension
                          (file-name-nondirectory
                           (car (process-command (get-buffer-process (current-buffer))))))))
              (cond
               ((string= shell "bash")
                (add-hook 'comint-output-filter-functions 'shell-filter-ctrl-a-ctrl-b)
                (setq comint-input-ring-file-name
                      (or (getenv "HISTFILE")
                          (if (file-exists-p "~/.bash.d/.bash_history")
                              "~/.bash.d/.bash_history"
                            "~/.bash_history"))))))))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; make "," a word constituent so that we can use it in
            ;; abbrevs (and also filenames)
            (modify-syntax-entry ?\, "w")
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
(define-shell-abbrev ",h" "--help")
(define-shell-abbrev ",v" "--verbose")
(define-shell-abbrev ",x" "| xargs")

(fset 'sh 'shell)

(defun shell-setup-keys ()
  "*Key bindings for shell-mode."
  (defkey shell-mode-map "C-x C-r" 'view-file))

(add-hook 'shell-mode-hook 'shell-setup-keys)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-hook 'shell-mode-hook 'pcomplete-shell-setup)

(when (require-soft 'pwdsync)
  (add-hook 'shell-mode-hook (lambda () (pwdsync-mode 1))))

(setq shell-dirtrack-verbose nil)

(defmacro with-buffer-hosting-pid (pid body)
  "*Execute BODY in the buffer associated with the process with id PID.
Return NIL if there is no buffer hosting a process with PID.
Otherwise return the value of the last form in BODY."
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (catch 'found
                      (dolist (,buffer (buffer-list))
                        (let ((process (get-buffer-process ,buffer)))
                          (and process
                               (= ,pid (process-id process))
                               (throw 'found ,buffer)))))))
       (and ,buffer
            (with-current-buffer ,buffer
              ,body)))))

;; (defun pcmpl-bash-complete-command ()
;;   "Completion function for Bash command names.
;; Uses Bash's builtin `compgen' to get a list of possible commands."
;;   (let ((current (nth pcomplete-index pcomplete-args)))
;;     (pcomplete-here* (pcomplete-uniqify-list (comint-redirect-results-list (format "compgen -c %s" (or current "")) "^\\(.+\\)$" 1)))))

;; (setq pcomplete-command-completion-function #'pcmpl-bash-complete-command)


;; (defun pcmpl-bash-environment-variable-completion ()
;;   "Completion data for an environment variable at point, if any."
;;   (let ((var (nth pcomplete-index pcomplete-args)))
;;     (when (and (not (zerop (length var))) (eq (aref var 0) ?$))
;;       (pcomplete-here* (pcomplete-uniqify-list (comint-redirect-results-list (format "compgen -P \\$ -v %s" (substring var 1)) "^\\(.+\\)$" 1))))))

;; (setq pcomplete-default-completion-function pcmpl-bash-environment-variable-completion)

;; (setq comint-dynamic-complete-functions '(shell-environment-variable-completion pcomplete-completions-at-point shell-filename-completion))


;;; http://www.masteringemacs.org/articles/2012/01/16/pcomplete-context-sensitive-completion-emacs/
(add-to-list 'process-coding-system-alist '("svn" . undecided-dos))

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
                       (file-name-nondirectory current))))))

(defun pcmpl-svn-get-files (dir)
  "Return a list of `svn' files in DIR."
  (split-string (pcomplete-process-result "svn" "--non-interactive" "ls" dir)))

;;; Comint
(setq-default comint-scroll-to-bottom-on-input 'this)
(setq-default comint-scroll-to-bottom-on-output nil)
(setq-default comint-scroll-show-maximum-output t)

;; http://www.emacswiki.org/emacs/ShellMode
(defun comint-clear-buffer ()
  "*Clear the process buffer, deleting its entire contents."
  (interactive "*")
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

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
  (add-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom)
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer))

(add-hook 'comint-mode-hook 'comint-setup)

(provide 'init-shell)

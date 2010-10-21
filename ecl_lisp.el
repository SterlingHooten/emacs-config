;;; Lisp/Scheme configuration

;;; Stuff common to all Lisp modes

(mapc (lambda (mode)
        (font-lock-add-keywords mode '(("[()]" 0 '((:foreground "grey60"))))))
      '(emacs-lisp-mode lisp-mode lisp-interaction-mode inferior-lisp-mode scheme-mode scheme48-mode inferior-scheme-mode))


(defun mouse-insert-sexp-at-point (start-event)
  "Insert the sexp under the mouse cursor at point.
This must be bound to a mouse event."
  (interactive "*e")
  (let ((posn (event-start start-event)))
    (let ((sexp-at-mouse-pos
           (with-selected-window (posn-window posn)
             (save-excursion
               (goto-char (posn-point posn))
               (thing-at-point 'sexp)))))
      (if sexp-at-mouse-pos
          (insert sexp-at-mouse-pos)
        (error "Mouse not at a sexp")))))

(global-set-key [S-mouse-3] 'mouse-insert-sexp-at-point)

;; Paredit
(eval-after-load 'paredit
  '(defkey paredit-mode-map "M-/" nil))

(autoload 'paredit-mode "paredit"
  "Turns on pseudo-structural editing of Lisp code." t)

(eval-after-load 'paredit
  '(progn
     (defkey paredit-mode-map "M-/" nil)
     (defkey paredit-mode-map ")" 'paredit-close-parenthesis)
     (defkey paredit-mode-map "M-)" 'paredit-close-parenthesis-and-newline)))

(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme48-mode-hook (lambda () (paredit-mode +1)))

(mapc (lambda (hook)
        (add-hook hook
                  (lambda ()
                    (make-local-variable 'show-paren-mode)
                    (show-paren-mode 1))))
      '(emacs-lisp-mode-hook
        lisp-mode-hook
        lisp-interaction-mode-hook
        inferior-lisp-mode-hook
        scheme-mode-hook
        scheme48-mode-hook
        inferior-scheme-mode-hook))

(defun run-scheme48 ()
  "*Run Scheme48 as an inferior Scheme process."
  (interactive)
  (run-scheme "s48-local"))

(defun run-scsh ()
  "*Run Scsh as an inferior Scheme process."
  (interactive)
  (run-scheme "scsh-local"))

(autoload 's48-mode "s48" "ya major mode for Scheme48 development" t)

(autoload 'scheme48-mode "scheme48" "major mode for Scheme48 development" t)
(add-hook 'scheme48-mode-hook
          (lambda ()
            (setq local-abbrev-table scheme-mode-abbrev-table)))

;; Automode
(add-to-list 'interpreter-mode-alist '("scsh" . scheme-mode))

(defun lisp-indent-definition ()
  "*Indent each line of the current definition."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (indent-sexp)))

;; for SLIB
(unless (getenv "SCHEME_LIBRARY_PATH")
  (setenv "SCHEME_LIBRARY_PATH" "/usr/local/share/slib/"))

;; for MzScheme
(unless (getenv "PLTCOLLECTS")
  (setenv "PLTCOLLECTS" "/usr/local/lib/plt/collects:/usr/lib/plt/collects"))

;; for Scsh
(unless (getenv "SCSH_LIB_DIRS")
  (setenv "SCSH_LIB_DIRS" "\"/usr/local/scsh-packages/0.6\""))

(defun scheme-insert-sexp-and-go (arg)
  "*Copy the sexp following point to the inferior Scheme buffer.
Then switch to the process buffer.
With argument, copy that many sexps after point.
Negative arg -N means copy N sexps before point."
  (interactive "p")
  (let ((sexps
         (buffer-substring (point) (save-excursion (forward-sexp arg) (point)))))
    ;; use this instead of `scheme-buffer' so that a Scheme process will be
    ;; started if none is running.
    (with-current-buffer (process-buffer (scheme-proc))
      (goto-char (max (point) (process-mark (scheme-proc))))
      (insert sexps))
    (switch-to-scheme nil)))

(defun backward-down-list (&optional arg)
  "Move backward down one level of parentheses.
With ARG, do this that many times.
A negative argument means move forward but still down a level."
  (interactive "p")
  (down-list (- (or arg 1))))

(defun scheme-scratch (&optional arg)
  "*Switch to buffer `*Scheme scratch*', creating it if necessary.
The buffer is put in Scheme mode.
With prefix arg clear the buffers content."
  (interactive "P")
  (switch-to-buffer-create "*Scheme scratch*" 'scheme-mode arg))

(defun set-scheme-buffer (buffer)
  "*Set locally the variable `scheme-buffer'.
Useful when running multiple inferior Scheme processes."
  (interactive (list
                (let ((iswitchb-buffer-ignore
                       (list (lambda (b)
                               (with-current-buffer b
                                 (not (eq major-mode 'inferior-scheme-mode)))))))
                  (iswitchb-read-buffer "Scheme buffer: "))))
  (set (make-local-variable 'scheme-buffer) buffer))

(defun prettify-lambda ()
  "*Display lambda forms using the `lambda' character.
Code taken from the \"quack\" package."
  (interactive)
  (let ((lambda-char (make-char 'greek-iso8859-7 107)))
    (font-lock-add-keywords nil
                            `(("[[(]\\(case-\\|match-\\|opt-\\)?\\(lambda\\)\\>"
                               2
                               (progn (compose-region (match-beginning 2)
                                                      (match-end       2)
                                                      ,lambda-char) nil)))))
  (font-lock-fontify-buffer))

(add-hook 'scheme-mode-hook
          (lambda ()
            (local-defkey "C-c <tab>" 'lisp-indent-definition)
            (local-defkey "C-c C-c" 'scheme-send-definition)
            (local-defkey "C-M-S-d" 'backward-down-list)
            (local-defkey "M-<return>" 'scheme-insert-sexp-and-go)
            (modify-syntax-entry ?\| "_") ; fix bug in scheme.el
            ;;(local-defkey "C-M-q" 'lisp-indent-definition)
            ))

;; MIT-Scheme
;; (add-hook 'scheme-mode-hook (lambda ()
;;                               (require 'xscheme)
;;                               (require 'tempo-scheme)
;;                               (make-local-variable 'show-paren-mode)
;;                               (show-paren-mode 1)
;;                               (set (make-local-variable 'comment-add) 1) ;default to `;;' in comment-region
;;                               (setq same-window-buffer-names (delete "*scheme*" same-window-buffer-names))
;;                               (local-defkey "C-c C-s" (lambda () (interactive)
;;                                                         (display-buffer xscheme-buffer-name)))
;;                               (local-unset-key (kbd "M-z"))))

;; MzScheme with ilisp
;; (require-soft 'ilisp-start)
;; (setq scheme-program-name "mzscheme")
;; (add-hook 'scheme-mode-hook (lambda ()
;;                               (require 'tempo-scheme)
;;                               (when (featurep 'ilisp-start)
;;                                 (local-defkey "C-x C-e" 'eval-defun-lisp))
;;                               (make-local-variable 'show-paren-mode)
;;                               (show-paren-mode 1)
;;                               (set (make-local-variable 'comment-add) 1) ;default to `;;' in comment-region
;;                               (setq same-window-buffer-names (delete "*scheme*" same-window-buffer-names))))

;; MzScheme with cmuscheme (comint-based)
;; (setq scheme-program-name "mzscheme")
;; (add-hook 'scheme-mode-hook (lambda ()
;;                               (require 'cmuscheme)
;;                               (require-soft 'mzscheme)
;;                               (require 'tempo-scheme)
;;                               (when (require-soft 'scheme-x)
;;                                 (setq scheme-trace-command "(begin (require (lib \"trace.ss\")) (trace %s))"))
;;                               (make-local-variable 'show-paren-mode)
;;                               (show-paren-mode 1)
;;                               (when (display-graphic-p)
;;                                 (prettify-lambda))
;;                               (set (make-local-variable 'comment-add) 1) ;default to `;;' in comment-region
;;                               (setq same-window-buffer-names (delete "*scheme*" same-window-buffer-names))))
;;
;; (add-hook 'inferior-scheme-mode-hook
;; 	  '(lambda ()
;;              (defun comint-maybe-add-newline-to-output (str)
;;                "Add a newline character to the output if STR is empty."
;;                (if (string-match "\\`\n" str)
;;                    str
;;                  (concat "\n" str)))
;;              (add-hook 'comint-preoutput-filter-functions 'comint-maybe-add-newline-to-output)
;; 	     (split-window)))

;; scsh/scheme 48 setup
(setq scheme-program-name "scsh-local.bat")
(setq scheme-mit-dialect nil)
(add-hook 'scheme-mode-hook (lambda ()
                              (require 'cmuscheme)
                              ;;(require 'tempo-scheme)
                              (setq scheme-trace-command ",trace %s"
                                    scheme-untrace-command ",untrace %s"
                                    scheme-macro-expand-command ",expand %s")
                              (make-local-variable 'show-paren-mode)
                              (show-paren-mode 1)
                              ;; (when (display-graphic-p)
;;                                 (prettify-lambda))
                              (set (make-local-variable 'comment-add) 1) ;default to `;;' in comment-region
                              (setq same-window-buffer-names (delete "*scheme*" same-window-buffer-names))))

;; (setq special-display-buffer-names '("*scheme*"))

(add-hook 'scheme-mode-hook
          (lambda ()
            (mapc (lambda (a)
                    (let ((x (car a))
                          (y (cdr a)))
                      (font-lock-add-keywords
                       nil
                       `((,(format "\\<%s\\>" (regexp-quote (symbol-name x))) . font-lock-keyword-face)))
                      (put x 'scheme-indent-function y)))
                  '((begin0 . 0)
                    (when . 1)
                    (unless . 1)
                    (let-optionals . scheme-let-indent)
                    (let-optionals* . scheme-let-indent)
                    (let*-values . scheme-let-indent)
                    (with-current-input-port . 1)


                    (and-let* . 1)
                    (awk . 3)
                    (call-with-current-noise-port . 1)
                    (close-after . 2)
                    (destructure . 1)
                    (do-in-subdirs . defun)
                    (if-match . 2)
                    (let-optionals . 2)
                    (receive . 2)
                    (run . 1)
                    (syntax-case . 2)
                    (unless . 1)
                    (with-current-input-port . 2)
                    (with-current-output-port . 2)
                    (with-cwd . 1)
                    (with-handler . 1)
                    (with-interrupt-handler . 2)
                    (with-new-proposal . 1)
                         
                    (define-record-type . 2)
                    (make-method . 1)
                    (add-method . defun)
                    ))
            (font-lock-add-keywords
             nil
             '(("^,\\S-+" . font-lock-preprocessor-face))))) ; scheme48 *commands*

;; ;; Karl Pflästerer [http://groups.google.de/group/comp.lang.scheme/msg/56ce4a9d60e26323]
;; (defun my-scheme-eval-last-sexp (&optional insert)
;;   (interactive "P")
;;   (let ((standard-output (if insert (current-buffer) t))
;;         (cmd (buffer-substring (save-excursion (backward-sexp) (point)) (point))))
;;     (with-temp-buffer
;;       (comint-redirect-send-command-to-process cmd (current-buffer) (scheme-proc) t t)
;;       ;; /ECL/ folgende Zeile blockiert Emacs bis output kommt.  Vielleicht
;;       ;; koennte man da was mit einem "Sentinel" machen?
;;       (while (string= (buffer-string) "") (sleep-for 0.01))
;;       (princ (buffer-string)))))
;; (add-hook 'scheme-mode-hook
;;           (lambda ()
;;             (defkey scheme-mode-map "C-c C-e" 'my-scheme-eval-last-sexp)))


;; From http://www.cs.indiana.edu/chezscheme/emacs/iuscheme.el
(defun scheme-return ()
  "Newline and indent, or evaluate the sexp before the prompt.
Complete sexps are evaluated; for incomplete sexps inserts a newline
and indents."
  (interactive)
  (let ((input-start (process-mark (get-buffer-process (current-buffer)))))
    (if (< (point) input-start)
        (comint-send-input)             ; this does magic stuff
      (let ((state (save-excursion
                     (parse-partial-sexp input-start (point)))))
        (if (and (< (car state) 1)      ; depth in parens is zero
                 (not (nth 3 state))    ; not in a string
                 (not (save-excursion   ; nothing after the point
                        (search-forward-regexp "[^ \t\n\r]" nil t))))
            (comint-send-input)         ; then go for it.
          (newline-and-indent))))))

(add-hook 'inferior-scheme-mode-hook
          (lambda ()
            (defkey inferior-scheme-mode-map "C-c <tab>" 'scheme-indent-definition)
            (defkey inferior-scheme-mode-map "<return>" 'scheme-return)
            (defkey inferior-scheme-mode-map "C-j" 'comint-send-input)))


(setq s48-commands '("?"
                     "bound?"
                     "build"
                     "collect"
                     "condition"
                     "config"
                     "config-package-is"
                     "d"
                     "debug"
                     "dis"
                     "dump"
                     "exec"
                     "exit"
                     "exit-when-done"
                     "expand"
                     "flush"
                     "for-syntax"
                     "forget"
                     "go"
                     "help"
                     "in"
                     "inspect"
                     "keep"
                     "load"
                     "load-package"
                     "load-script"
                     "menu"
                     "new-package"
                     "open"
                     "pop"
                     "preview"
                     "proceed"
                     "push"
                     "q"
                     "reload-package"
                     "reset"
                     "run"
                     "set"
                     "structure"
                     "template"
                     "threads"
                     "time"
                     "trace"
                     "translate"
                     "u"
                     "undefine"
                     "unset"
                     "untrace"
                     "user"
                     "user-package-is"
                     "where"))

(defvar s48-command-history '()
  "History list of shortcut command names.")

(defvar s48-command-dispatch-char ?\,
  "Character used to start Scheme48 commands.")

(defun s48-handle-command (&optional arg)
  (interactive "P")
  (if (or arg
          (= (point) (marker-position (process-mark (get-buffer-process (current-buffer))))))
      (let ((cmd (completing-read "Command: "
                                  s48-commands
                                  nil t nil
                                  's48-command-history)))
        (insert (format ",%s " cmd)))
    (insert (string s48-command-dispatch-char))))

;; (add-hook 'inferior-scheme-mode-hook
;;           (lambda ()
;;             (define-key inferior-scheme-mode-map (string s48-command-dispatch-char) 's48-handle-command)))


(provide 'ecl_lisp)

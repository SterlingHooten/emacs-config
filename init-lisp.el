;;; init-lisp: Lisp/Scheme configuration

;;; Stuff common to all Lisp modes

(mapc (lambda (mode)
        (font-lock-add-keywords mode '(("[()]" 0 '((:foreground "grey60"))))))
      '(emacs-lisp-mode
        lisp-mode
        lisp-interaction-mode
        inferior-lisp-mode
        scheme-mode
        inferior-scheme-mode))

;; http://carcaddar.blogspot.com/2011/01/mouse-copy-for-emacs.html
;; http://paste.lisp.org/display/118722
(defun mouse-insert-sexp-at-point (start-event)
  "Insert the sexp under the mouse cursor at point.
This command must be bound to a mouse event."
  (interactive "*e")
  (let ((posn (event-start start-event)))
    (let ((sexp-at-mouse-pos
           (with-selected-window (posn-window posn)
             (save-excursion
               (goto-char (posn-point posn))
               (thing-at-point 'sexp)))))
      (if sexp-at-mouse-pos
          (progn
            (unless (or (bolp)
                        (and (minibufferp)
                             (= (point) (minibuffer-prompt-end)))
                        (save-excursion
                          (backward-char)
                          (looking-at "\\s-\\|\\s\("))))
            (insert sexp-at-mouse-pos)
            (unless (or (eolp)
                        (and (minibufferp)
                             (= (point) (minibuffer-prompt-end)))
                        (looking-at "\\s-\\|\\s\)"))))
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
        inferior-scheme-mode-hook))

(defun run-scheme48 ()
  "*Run Scheme48 as an inferior Scheme process."
  (interactive)
  (run-scheme "s48-local"))

(defun run-scsh ()
  "*Run Scsh as an inferior Scheme process."
  (interactive)
  (run-scheme "scsh-local"))

;; Automode
(add-to-list 'interpreter-mode-alist '("scsh" . scheme-mode))

(defun lisp-indent-definition ()
  "*Indent each line of the current definition."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (indent-sexp)))

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


;; Scheme templates
(require 'tempo)
(defvar scheme-tempo-tags nil)

(tempo-define-template "scheme-define"
               '("(define" p > r ")")
               "define"
               "Insert a `define' expression"
               'scheme-tempo-tags)

(tempo-define-template "scheme-defun"
               '("(define (" p ")" n> r ")")
               "defun"
               "Insert an expression for defining a function"
               'scheme-tempo-tags)

(tempo-define-template "scheme-lambda"
               '("(lambda (" p ")" n> r ")")
               "lambda"
               "Insert a `lambda' expression"
               'scheme-tempo-tags)

(tempo-define-template "scheme-if"
               '("(if " p n> r ")")
               "if"
               "Insert an `if' expression"
               'scheme-tempo-tags)

(tempo-define-template "scheme-when"
               '("(when " p n> p r> ")")
               "when"
               "Insert an `when' expression"
               'scheme-tempo-tags)

(tempo-define-template "scheme-unless"
               '("(unless " p n> p r> ")")
               "unless"
               "Insert an `unless' expression"
               'scheme-tempo-tags)

(tempo-define-template "scheme-cond"
               '("(cond ((" p ")" r "))")
               "cond"
               "Insert a `cond' expression"
               'scheme-tempo-tags)

(tempo-define-template "scheme-let"
               '("(let ((" p "))" r n> ")")
               "cond"
               "Insert a `let' expression"
               'scheme-tempo-tags)

(tempo-define-template "scheme-let*"
               '("(let* ((" p "))" r n> ")")
               "cond"
               "Insert a `let*' expression"
               'scheme-tempo-tags)

(tempo-define-template "scheme-loop"
                '("(let loop ((" p "))" n> r> ")")
                "loop"
                "Insert a loop construction using `let'"
                'scheme-tempo-tags)

(add-hook 'scheme-mode-hook
          (lambda ()

            (define-abbrev scheme-mode-abbrev-table "cond" "" 'tempo-template-scheme-cond)
            (define-abbrev scheme-mode-abbrev-table "define" "" 'tempo-template-scheme-define)
            (define-abbrev scheme-mode-abbrev-table "defun" "" 'tempo-template-scheme-defun)
            (define-abbrev scheme-mode-abbrev-table "if" "" 'tempo-template-scheme-if)
            (define-abbrev scheme-mode-abbrev-table "lambda" "" 'tempo-template-scheme-lambda)
            (define-abbrev scheme-mode-abbrev-table "let" "" 'tempo-template-scheme-let)
            (define-abbrev scheme-mode-abbrev-table "lets" "" 'tempo-template-scheme-let*)
            (define-abbrev scheme-mode-abbrev-table "loop" "" 'tempo-template-scheme-loop)
            (define-abbrev scheme-mode-abbrev-table "unless" "" 'tempo-template-scheme-unless)
            (define-abbrev scheme-mode-abbrev-table "when" "" 'tempo-template-scheme-when)

            (local-defkey "C-c C-f" 'tempo-forward-mark)
            (local-defkey "C-c C-b" 'tempo-backward-mark)

            (local-defkey "C-c t f" 'tempo-template-define)
            (local-defkey "C-c t l" 'tempo-template-lambda)
            (local-defkey "C-c t i" 'tempo-template-if)
            (local-defkey "C-c t c" 'tempo-template-cond)

            (local-defkey "M-S-<tab>" 'tempo-complete-tag)
            (tempo-use-tag-list 'scheme-tempo-tags)))


;; scsh/scheme 48 setup
(setq scheme-mit-dialect nil)
(add-hook 'scheme-mode-hook (lambda ()
                              (require 'cmuscheme)
                              (setq scheme-trace-command ",trace %s"
                                    scheme-untrace-command ",untrace %s"
                                    scheme-macro-expand-command ",expand %s")
                              (make-local-variable 'show-paren-mode)
                              (show-paren-mode 1)
                              (when (display-graphic-p)
                                (prettify-lambda))
                              (set (make-local-variable 'comment-add) 1) ;default to `;;' in comment-region
                              (setq same-window-buffer-names (delete "*scheme*" same-window-buffer-names))))

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




(provide 'init-lisp)

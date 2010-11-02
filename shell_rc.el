;; shell_rc: Shell/Comint initialization
;; Time-stamp: <2010-11-02 16:30:17 Emilio C. Lopes>

;; Newer versions of comint don't use prompt regexp anymore
(if (boundp 'comint-use-prompt-regexp-instead-of-fields)
    (setq comint-use-prompt-regexp-instead-of-fields nil)
  (setq shell-prompt-pattern "~?\\(/+[^/#$%>\n]+\\)*% "))

(setq shell-completion-fignore '("~" "#" "%"))
(setq comint-completion-autolist t)

(setq comint-input-ring-size 4000)
(setq comint-input-autoexpand nil)
(setq comint-input-ignoredups t)
(setq comint-buffer-maximum-size 15000)

(setq comint-process-echoes nil)

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

(fset 'sh 'shell)

(defun shell-setup-keys ()
  "*Key bindings for shell-mode."
  (defkey shell-mode-map "C-<up>" 'comint-previous-prompt)
  (defkey shell-mode-map "C-<down>" 'comint-next-prompt)
  ;; (defkey shell-mode-map "<SPC>" 'comint-magic-space)

  (defkey shell-mode-map "C-x C-r" 'view-file))

(add-hook 'shell-mode-hook 'shell-setup-keys)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-hook 'shell-mode-hook 'pcomplete-shell-setup)

(when (require-soft 'pwdsync)
  (add-hook 'comint-output-filter-functions 'pwdsync-filter 'append))

(setq shell-dirtrack-verbose nil)

;; (defun shell-snarf-aliases ()
;;   "Return a list of all defined shell aliases."
;;   ;; (comint-redirect-results-list "alias" "^alias \\([^=]+\\)=.+\n" 1)
;;   (comint-redirect-results-list "alias -rL" "^alias \\([^=]+\\)=.+\n" 1))

;; (defvar shell-aliases '())

;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;             (setq shell-aliases (shell-snarf-aliases))))

;; (defun shell-complete-alias ()
;;   (let ((cmd (or (comint-match-partial-filename) "")))
;;     (comint-dynamic-simple-complete cmd shell-aliases)))

;;(add-hook 'shell-dynamic-complete-functions 'shell-complete-alias)


;;; Comint
(setq-default comint-scroll-to-bottom-on-input 'this)
;; (setq-default comint-scroll-to-bottom-on-output 'others)
(setq-default comint-scroll-to-bottom-on-output nil)
(setq-default comint-scroll-show-maximum-output t)

(defun comint-clear-buffer ()
  "*Clear the process buffer, deleting its entire contents."
  (interactive "*")
  (delete-region (point-min) (overlay-start comint-last-prompt-overlay)))

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

  ;; (defkey comint-mode-map "M-." 'comint-insert-last-word)
  (defkey comint-mode-map "M-." 'comint-insert-previous-argument)
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
  (add-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom)
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer))

(add-hook 'comint-mode-hook 'comint-setup)

(provide 'shell_rc)

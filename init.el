;;; GNU Emacs initialization file -*- mode: Emacs-Lisp -*-
;;; Emilio C. Lopes

;;; TODO:
;; o Use `add-to-list' and similars for adding things to alists.
;; o Review mode-alist changes.
;; o Use `eval-after-load' for customization of packages.
;; o Review use of "extra" packages. Some are not necessary anymore
;;   while some others could be added.


(unless (boundp 'user-emacs-directory)
  (setq user-emacs-directory "~/.emacs.d/"))

(require 'init-basics (concat user-emacs-directory "init-basics"))

;;; User Interface
(setq running-interactively (not noninteractive))
(setq running-nt (equal system-type 'windows-nt))

;;; Load-path
(add-to-path 'load-path user-emacs-directory)
(add-to-path 'load-path (concat user-emacs-directory "lib"))
(setq custom-theme-directory (concat user-emacs-directory "lib"))

(when running-interactively
  (setq visible-bell t)
  (setq use-dialog-box nil)

  (mapc (lambda (x)
          (when (fboundp (car x))
            (apply 'funcall x)))
        '((menu-bar-mode -1)
          (tool-bar-mode -1)
          (scroll-bar-mode -1)
          (transient-mark-mode -1)
          (blink-cursor-mode -1)
          (show-paren-mode +1)
          (line-number-mode +1)
          (column-number-mode +1)
          (savehist-mode +1)))

  (setq line-number-display-limit-width 512)

  (add-to-list 'default-frame-alist '(cursor-type . box))
  (add-to-list 'default-frame-alist `(font . ,(if running-nt "Consolas 12" "DejaVu Sans Mono-14")))

  (when (display-color-p)
    (load-theme 'tango-plus t))

  (setq-default frame-title-format (list "" "Emacs Macht Alle Computer Sch\366n"))
  (setq-default icon-title-format frame-title-format))


;;; Language settings
(setq edmacro-eight-bits t)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq default-input-method "latin-1-prefix")

(defun activate-default-input-method ()
  "*Activate the input method defined in `default-input-method'."
  (set-input-method default-input-method))

(add-hook 'text-mode-hook 'activate-default-input-method)
(add-hook 'org-mode-hook 'activate-default-input-method)

(setq input-method-highlight-flag nil)
(setq input-method-verbose-flag t)

;; extend the `latin-1-prefix' input method
(eval-after-load "latin-pre"
  '(progn
     (quail-select-package "latin-1-prefix")
     (quail-define-rules
      ((append . t))
      ("\"." #x2026)                    ; ellipses
      ("\"$" #x20ac)                    ; euro sign
      ("\"`" #x201e)                    ; gaensefuesschen links
      ("\"'" #x201c)                    ; gaensefuesschen rechts
      )))

(setq eol-mnemonic-undecided "(?)"    ;; unknown EOL type
      eol-mnemonic-unix      "(unix)" ;; LF
      eol-mnemonic-dos       "(dos)"  ;; CRLF
      eol-mnemonic-mac       "(mac)") ;; CR

;;; Useful defs

(setq hostname (car (split-string system-name "\\." )))

;; Windows sets the environment variable USERNAME, but neither USER
;; nor LOGNAME.  Bash defines only LOGNAME, but not USER.  Make sure
;; the variables USER and LOGNAME are available since some
;; programs/libraries expect one of them to be accordingly set.

(if (and (null (getenv "USER"))
         (getenv "USERNAME"))
    (setenv "USER" (getenv "USERNAME")))

(if (and (getenv "LOGNAME")
         (null (getenv "USER")))
    (setenv "USER" (getenv "LOGNAME")))

(if (and (getenv "USER")
         (null (getenv "LOGNAME")))
    (setenv "LOGNAME" (getenv "USER")))

(setenv "PAGER" "cat")

(let ((tz (current-time-zone)))
  (when tz
    (setenv "TZ" (format "GMT%+d" (* -1(/ (car tz) 3600))))))

(defmacro bind-with-new-map (map binding &rest bindings)
  (let ((%map (make-symbol "map")))
    `(let ((,%map (make-sparse-keymap)))
       ,@(mapcar (lambda (char+command)
                   `(define-key ,%map (read-kbd-macro ,(car char+command)) ,(cdr char+command)))
                 bindings)
       (define-key ,map ,(read-kbd-macro binding) ,%map))))
(put 'bind-with-new-map 'lisp-indent-function 2)

;;; General configuration

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "qx29999")
(setq inhibit-startup-echo-area-message "eclig")
(setq inhibit-startup-echo-area-message "ecl")
(setq initial-scratch-message nil)

;; set up the mode based on the buffer name.  Thanks to `__jim__'.
;; http://www.reddit.com/r/emacs/comments/d2t4q/scratch_buffers_for_emacs/c0x7a68
(setq-default major-mode
              (lambda ()
                (let ((buffer-file-name (or buffer-file-name (buffer-name))))
                  (set-auto-mode))))

(setq view-read-only nil)

(setq disabled-command-function nil)    ; no disabled commands.
(substitute-key-definition 'overwrite-mode nil (current-global-map)) ; unbind `overwrite-mode'

(setq message-log-max 1024)     ; max size of the "*Messages*" buffer.

(setq eval-expression-print-length nil)

(setq scroll-step 0)
(setq scroll-conservatively 0)

(setq scroll-preserve-screen-position t)

(setq window-min-height 2)
(setq window-min-width 10)

(setq enable-recursive-minibuffers t)
(setq history-length 1000)

(setq minibuffer-prompt-properties
      (plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt))

(setq set-mark-command-repeat-pop t)

(setq-default indent-tabs-mode nil)

(setq parens-require-spaces nil)

(setq-default fill-column 70)

(setq adaptive-fill-first-line-regexp "\\`[ \t>]*\\'")

(setq auto-save-default t)
(setq auto-save-file-format t)
(setq auto-save-visited-file-name nil)
(setq auto-save-interval 200)
(setq auto-save-timeout 15)

(setq large-file-warning-threshold 20000000)

(setq make-backup-files t)
(setq backup-by-copying t)
(setq delete-auto-save-files t)
(setq version-control t)                ; numeric backups.
(setq kept-new-versions 4)
(setq kept-old-versions 2)
(setq delete-old-versions t)

(setq-default ctl-arrow t)

(setq echo-keystrokes 0.000001)

(setq-default case-fold-search t)
(setq case-replace t)

;; allow ellipses to end sentences
(setq sentence-end-base "[….?!][]\"'”)}]*")

(setq search-whitespace-regexp "[ \t\r\n]+")

(setq revert-without-query '("."))

(setq search-highlight t)
(setq query-replace-highlight t)

(setq next-line-add-newlines nil)
(setq require-final-newline t)

(setq-default indicate-empty-lines t)

(setq enable-local-eval 'ask)
(setq enable-local-variables t)

(setq find-file-existing-other-name t)

(setq mouse-yank-at-point t)

(setq kill-read-only-ok t)

(setq windmove-wrap-around t)

(setq comment-style 'indent)

(setq list-directory-brief-switches "-BCF")
(setq list-directory-verbose-switches "-Bl")

(add-to-path 'Info-default-directory-list "/usr/info")
(add-to-path 'Info-default-directory-list "/usr/share/info")

;;; Functions

(defvar default-register 0)

(defun set-default-register (register)
  (interactive "*cDefault register: ")
  (setq default-register register))

(defun insert-default-register ()
  "*Insert contents of register `default-register'."
  (interactive)
  (insert-register default-register t))

(defun copy-to-default-register (start end &optional append)
  "*Copy region into register `default-register'.
With prefix arg, append it."
  (interactive "r\nP")
  (funcall (if append 'append-to-register 'copy-to-register) default-register start end))

(defvar zap-to-char-last-char nil
  "Last char given as input to `zap-to-char'.")

(defun zap-up-to-char (arg char)        ; adapted from `zap-to-char'
  "*Kill up to (but not including) ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (if (eq real-last-command this-command)
                         zap-to-char-last-char
                       (setq zap-to-char-last-char (read-char "Zap to char: ")))))
  (kill-region (point) (save-excursion
                         (progn
                           (search-forward (char-to-string char)
                                           nil
                                           nil
                                           (if (looking-at (regexp-quote (char-to-string char)))
                                               (if (> arg 0)
                                                   (1+ arg)
                                                 (1- arg))
                                             arg))
                           (if (> arg 0) (1- (point)) (1+ (point)))))))

(global-defkey "M-z" 'zap-up-to-char)
(global-defkey "M-Z" 'zap-to-char)

;; analogous to dired-copy-filename-as-kill
(defun copy-filename-as-kill (arg)
  "*Copy the value of buffer-file-name into the kill ring.
If current buffer is not visiting any file, use the current
working directory instead.  With a prefix argument ARG use only
the last component of the file's path (its \"basename\")."
  (interactive "P")
  (let* ((fn (or buffer-file-name
                 (directory-file-name default-directory)))
         (kill-string (if arg (file-name-nondirectory fn) fn)))
    (kill-new kill-string)
    (message "%s" kill-string)))

;; show-buffer-file-name
(defun show-buffer-file-name ()
  "*Display the value of buffer-file-name in the echo area."
  (interactive)
  (if buffer-file-name 
      (message "%s" buffer-file-name)
    (message "Buffer \"%s\" is not visiting any file." 
             (buffer-name))))

(defun dos2unix ()
  "*Convert the entire buffer from M$-DOG text file format to UNIX."
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\r+$" "" nil)
    (goto-char (1- (point-max)))
    (when (looking-at "\C-z")
        (delete-char 1))))

(defun revert-buffer-preserve-modes ()
  "*Revert current buffer preserving modes.
Normally they are reinitialized using `normal-mode'.
The read-only status of the buffer is also preserved."
  (interactive)
  (let (buffer-read-only)
    (revert-buffer nil nil t)))

(defun dos-revert ()
  "*Revert current buffer using 'undecided-dos as coding system."
  (interactive)
  (let ((coding-system-for-read 'undecided-dos)
        buffer-read-only)
    (revert-buffer nil t t)))

(defun confirm-exit ()
  (interactive)
  (yes-or-no-p "Really exit Emacs? "))
(add-hook 'kill-emacs-query-functions 'confirm-exit)

(defun mark-backup-buffers-read-only ()
  "*Mark buffers containing backup files as read-only."
  (when (backup-file-name-p buffer-file-name)
    (toggle-read-only 1)))
(add-hook 'find-file-hooks 'mark-backup-buffers-read-only)

;;; other-window-or-other-buffer

(defvar buffer-ignore-regexp '("^ ")
  "*Regexp matching buffer names to be ignored by \\[next-buffer].")

(setq buffer-ignore-regexp
      (concat "^ "
              "\\|\\*Completions\\*"
              "\\|\\*Help\\*"
              "\\|\\*Apropos\\*"
              "\\|\\*Buffer List\\*"
              "\\|\\*Messages\\*"
              "\\|\\*compilation\\*"
              "\\|\\*i?grep\\*"
              "\\|\\*occur\\*"
              "\\|^\\*Score Trace\\*"
              "\\|^\\*ftp"
              "\\|\\*Directory\\*"))

(defun buffer-list-filter (&optional list)
  (let ((bufflist (or list (buffer-list))))
    (mapcar (lambda (buffer)
              (when (string-match buffer-ignore-regexp (buffer-name buffer))
                (delete buffer bufflist))) bufflist)
    bufflist))

(defun other-window-or-other-buffer (arg)
  "*Select next visible window on this frame or, if none, switch to `other-buffer'.
With prefix argument ARG select the ARGth window or buffer.
Subject to `buffer-ignore-regexp'."
  (interactive "p")
  (if (one-window-p)
      (next-buffer arg)
    (other-window arg)))

(defun next-buffer (arg)
  "*Switch to the `next' buffer.
With prefix argument ARG switch to the ARGth buffer in the buffer list.
Subject to `buffer-ignore-regexp'."
  (interactive "p")
  (let ((bufflist (buffer-list-filter)))
    (when (> 0 arg)
      (setq arg (+ (length bufflist) arg)))
    (switch-to-buffer (nth arg bufflist))))

(defun resize-window (&optional arg)    ; Hirose Yuuji and Bob Wiener
  "*Resize window interactively."
  (interactive "P")
  (if (one-window-p) (error "Cannot resize sole window"))
  (setq arg (if arg (prefix-numeric-value arg) 4))
  (let (c)
    (catch 'done
      (while t
	(message
	 "C-n=heighten, C-p=shrink, C-f=widen, C-b=narrow (by %d);  0-9=unit, <RET>=quit"
	 arg)
	(setq c (read-char))
	(condition-case ()
	    (cond
	     ((= c ?\^N) (enlarge-window arg))
	     ((= c ?\^P) (shrink-window arg))
	     ((= c ?\^F) (enlarge-window-horizontally arg))
	     ((= c ?\^B) (shrink-window-horizontally arg))
	     ((= c ?\^G) (keyboard-quit))
	     ((= c ?\^M) (throw 'done t))
             ((= c ?0) (setq arg 10))
	     ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
	     (t (beep)))
	  (error (beep)))))
    (message "Done.")))

(defun change-window ()
  "*Change windows interactively."
  (interactive)
  (cond
   ((one-window-p)
    (error "Sole window in frame"))
   ((<= (count-windows) 3)
    (other-window 1))
   (t
    (let (c)
      (catch 'done
        (while t
          (setq c (read-char-exclusive "C-n=down, C-p=up, C-f=right, C-b=left, 0=delete, <SPC>=next, <BACKSPACE>=previous, <RET>=quit" nil 2))
          (condition-case ()
              (cond
               ((or (null c) (= c ?\^M)) (throw 'done t))
               ((= c ?0)   (delete-window))
               ((= c ?2)   (split-window-vertically))
               ((= c ?3)   (split-window))
               ((= c ? )   (other-window 1))
               ((= c ?\^?) (other-window -1))
               ((= c ?\^N) (windmove-down))
               ((= c ?\^P) (windmove-up))
               ((= c ?\^F) (windmove-right))
               ((= c ?\^B) (windmove-left))
               ((= c ?\^G) (keyboard-quit))
               (t (beep)))
            (error (beep)))))
      (message "Done.")))))

(global-defkey "M-i" 'other-window-or-other-buffer)
;; (display-buffers-matching (lambda (b) (with-current-buffer b (string-match "shell-mode" (symbol-name major-mode)))))
;; (display-buffers-matching (lambda (b) (with-current-buffer b (string-match "org-mode" (symbol-name major-mode)))))

(defun display-buffers-matching (predicate)
  "Displays all buffers matching PREDICATE.
    The function PREDICATE will be called with each buffer as its only argument."
  (let ((buffers
         (let (buffers)
           (dolist (buffer (buffer-list) buffers)
             (when (funcall predicate buffer)
               (setq buffers (append buffers `(,buffer)))))))
        (rows 1)
        (cols 1))
    (if (= 0 (length buffers))
        (message "No matching buffers.")

      (setq rows (floor (sqrt (length buffers))))
      (setq cols (ceiling (/ (length buffers) (float rows))))

      ;; split windows...
      (delete-other-windows)

      ;;create rows
      (dotimes (i (- rows 1))
        (split-window-vertically))
      ;; create cols
      (dotimes (j rows)
        (other-window -1)
        (dotimes (i (- cols 1))
          (split-window-horizontally)))

      ;; display buffers...
      (dolist (buffer buffers)
        (set-window-buffer nil buffer)
        (other-window 1))

      ;; remove empty windows (if cols*rows > length-of-buffers)
      (if (> (* cols rows) (length buffers))
          (dotimes (i (- (* cols rows) (length buffers)))
            (delete-window)))
      (balance-windows))))

(global-defkey "C-x S"
  (lambda ()
    (interactive)
    (display-buffers-matching
     (lambda (b)
       (with-current-buffer b
         (and (string-match "shell-mode" (symbol-name major-mode))
              (not (string-match-p "\\`\\*Async Shell " (buffer-name)))))))))

(defun describe-face-at-point ()
  "*Display the properties of the face at point."
  (interactive)
  (let ((face (or (get-char-property (point) 'face) 'default)))
    (describe-face face)
    (with-current-buffer "*Help*"
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (insert "Face at point: " (propertize (format "%s" face) 'face face) "\n\n")
        (set-buffer-modified-p nil)))))

(defun untabify-buffer ()
  "*Like `untabify', but operate on the whole buffer."
  (interactive "*")
  (untabify (point-min) (point-max)))

(defun delete-horizontal-space-forward () ; adapted from `delete-horizontal-space'
  "*Delete all spaces and tabs after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun backward-delete-char-hungry (arg &optional killp)
  "*Delete characters backward in \"hungry\" mode.
See the documentation of `backward-delete-char-untabify' and
`backward-delete-char-untabify-method' for details."
  (interactive "*p\nP")
  (let ((backward-delete-char-untabify-method 'hungry))
    (backward-delete-char-untabify arg killp)))

(defun indirect-elisp-buffer (arg) ; Erik Naggum
  "*Edit Emacs Lisp code from this buffer in another window.
Optional argument ARG is number of sexps to include in that buffer."
  (interactive "P")
  (let ((buffer-name (generate-new-buffer-name " *elisp*")))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (emacs-lisp-mode)
    (narrow-to-region (point) (save-excursion (forward-sexp arg) (point)))))

;; Noah Friedman
;; http://www.splode.com/users/friedman/software/emacs-lisp/src/buffer-fns.el
(defadvice rename-buffer (before interactive-edit-buffer-name activate)
  "Prompt for buffer name supplying current buffer name for editing."
  (interactive
   (list (let ((minibuffer-local-completion-map
                (copy-keymap minibuffer-local-completion-map)))
           (define-key
             minibuffer-local-completion-map " " 'self-insert-command)
           (completing-read "Rename current buffer to: "
                            (mapcar (function (lambda (buffer)
                                                (list (buffer-name buffer))))
                                    (buffer-list))
                            nil
                            nil
                            (if (string-lessp "19" emacs-version)
                                (cons (buffer-name) 0)
                              (buffer-name))))
         current-prefix-arg)))

(defun switch-to-buffer-create (name mode &optional wipe)
  "*Switch to buffer NAME, creating it if necessary.
Upon creation the buffer is put in major-mode MODE, which must be
a function.  If argument WIPE is non nil, clear the buffer's
contents."
  (if (get-buffer name)
      (progn
        (switch-to-buffer name)
        (when wipe
          (erase-buffer)
          (set-buffer-modified-p nil)))
    (switch-to-buffer (generate-new-buffer name))
    (funcall mode)))

;; Noah Friedman
;; http://www.splode.com/users/friedman/software/emacs-lisp/src/buffer-fns.el
(defun messages ()
  "*Display message log buffer, if it exists."
  (interactive)
  (let* ((buffer-name "*Messages*")
         (buf (get-buffer buffer-name))
         (curbuf (current-buffer))
         (curwin (selected-window))
         winbuf)
    (cond (buf
           (unwind-protect
               (progn
                 (setq winbuf (display-buffer buf))
                 (select-window winbuf)
                 (set-buffer buf)
                 (goto-char (point-max))
                 (recenter -1))
             (select-window curwin)
             (set-buffer curbuf)))
          (t
           (message "Message log is empty.")))))
(global-defkey "C-h M" 'messages)

(defun kill-buffer-and-window-no-confirm ()
  "*Kill the current buffer and delete the selected window, WITHOUT asking."
  (interactive)
  (let ((buffer (current-buffer)))
    (delete-window)
    (kill-buffer buffer)))
(defkey ctl-x-4-map "0" 'kill-buffer-and-window-no-confirm)

(defun quit-buffer-delete-window ()
  "Quit (bury) the current buffer and delete the selected window."
  (interactive)
  (quit-window nil (selected-window)))
(defkey ctl-x-4-map "q" 'quit-buffer-delete-window)

(defun swap-window-positions ()         ; Stephen Gildea
  "*Swap the positions of this window and the next one."
  (interactive)
  (let ((other-window (next-window (selected-window) 'no-minibuf)))
    (let ((other-window-buffer (window-buffer other-window))
	  (other-window-hscroll (window-hscroll other-window))
	  (other-window-point (window-point other-window))
	  (other-window-start (window-start other-window)))
      (set-window-buffer other-window (current-buffer))
      (set-window-hscroll other-window (window-hscroll (selected-window)))
      (set-window-point other-window (point))
      (set-window-start other-window (window-start (selected-window)))
      (set-window-buffer (selected-window) other-window-buffer)
      (set-window-hscroll (selected-window) other-window-hscroll)
      (set-window-point (selected-window) other-window-point)
      (set-window-start (selected-window) other-window-start))
    (select-window other-window)))

;; from http://www.emacswiki.org/cgi-bin/wiki/ToggleWindowSplit
(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (count-windows) 2)
    (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window)                     ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil)))

(define-key ctl-x-4-map "t" 'toggle-frame-split)

(defun move-to-window-top ()
  "*Move the point to the top of the current window."
  (interactive)
  (move-to-window-line 0))

(defun move-to-window-bottom ()
  "*Move the point to the the bottom of the current window."
  (interactive)
  (move-to-window-line -1))

(defun kill-backward-up-list (&optional arg)
  "Kill the form containing the current sexp, leaving the sexp itself.
A prefix argument ARG causes the relevant number of surrounding
forms to be removed."
  (interactive "p")
  (let ((current-sexp (thing-at-point 'sexp)))
    (if current-sexp
        (save-excursion
          (backward-up-list arg)
          (kill-sexp)
          (insert current-sexp))
      (error "Not at a sexp"))))

(global-defkey "C-<backspace>" 'kill-backward-up-list)

(defun copy-sexp (arg)                  ; adapted from `kill-sexp'
  "*Copy the sexp following the cursor to the kill-ring.
With argument, copy that many sexps after the cursor.
Negative arg -N means copy N sexps before the cursor."
  (interactive "p")
  (copy-region-as-kill (point) (save-excursion (forward-sexp arg) (point))))

(defun comment-sexp (arg)               ; adapted from `kill-sexp'
  "*Comment the sexp following the cursor.
With argument, comment that many sexps after the cursor.
Negative arg -N means comment N sexps before the cursor."
  (interactive "p")
  (comment-region (point) (save-excursion (forward-sexp arg) (point))))

(global-defkey "C-;" 'comment-sexp)

(defun copy-line (&optional arg)        ; adapted from `kill-line'
  "*Copy the rest of the current line (or ARG lines) to the kill-ring.
Like \\[kill-line], except that the lines are not really killed, just copied
to the kill-ring. See the documentation of `kill-line' for details."
  (interactive "P")
  (copy-region-as-kill (point)
                       (progn ;save-excursion
                         (if arg
                             (forward-visible-line (prefix-numeric-value arg))
                           (if (eobp) (signal 'end-of-buffer nil))
                           (if (or (looking-at "[ \t]*$") (and kill-whole-line (bolp)))
                               (forward-visible-line 1)
                             (end-of-visible-line)))
                         (point)))
  (setq this-command 'kill-region))

;; Thank you, Mickey Petersen.
;; http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/
(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

;; inspired by Erik Naggum's `recursive-edit-with-single-window'
(defmacro recursive-edit-preserving-window-config (body)
  "*Return a command that enters a recursive edit after executing BODY.
Upon exiting the recursive edit (with\\[exit-recursive-edit] (exit)
or \\[abort-recursive-edit] (abort)), restore window configuration
in current frame."
  `(lambda ()
     "See the documentation for `recursive-edit-preserving-window-config'."
     (interactive)
     (save-window-excursion
       ,body
       (recursive-edit))))

;; (defun recursive-edit-with-single-window (&optional winfunc) ; Erik Naggum
;;   "*Enter a recursive edit with the current window as the single window.
;; The optional argument WINFUNC determines which function will be used
;; to set up the new window configuration.
;; Upon exit, restore window configuration in current frame.
;; Exit with \\[exit-recursive-edit] (exit) or \\[abort-recursive-edit] (abort)."
;;   (interactive)
;;   (when (one-window-p t)
;;     (error "Current window is the only window in its frame"))
;;   (save-window-excursion
;;     (if winfunc (funcall winfunc) (delete-other-windows))
;;     (recursive-edit)))

(defun insert-date (&optional arg)
  "*Insert date in current buffer using German format DD.MM.YYYY.
With optional prefix argument use ISO format instead."
  (interactive "*P")
  (insert (format-time-string
           (if arg "%Y-%m-%d" "%d.%m.%Y")
           (current-time))))

(defun update-date ()
  "*Update the date around point.
Recognise the formats DD.MM.YYYY, YYYY-MM-DD and MM/DD/YYYY."
  (interactive "*")
  (let ((update-date-formats
         '(("[0-3][0-9]\\.[01][0-9]\\.[0-9][0-9][0-9][0-9]"   . "%d.%m.%Y")
           ("[0-3]?[0-9]\\.[01]?[0-9]\\.[0-9][0-9][0-9][0-9]" . "%-d.%-m.%Y")
           ("[0-3]?[0-9]\\.[01]?[0-9]\\.[0-9][0-9]"           . "%-d.%-m.%y")

           ("[01][0-9]/[0-3][0-9]/[0-9][0-9][0-9][0-9]"       . "%m/%d/%Y")
           ("[01]?[0-9]/[0-3]?[0-9]/[0-9][0-9][0-9][0-9]"     . "%-m/%-d/%Y")
           ("[01]?[0-9]/[0-3]?[0-9]/[0-9][0-9]"               . "%-m/%-d/%y")

           ("[0-9][0-9][0-9][0-9]-[01][0-9]-[0-3][0-9]"       . "%Y-%m-%d"))))
    (save-excursion
      ;; (skip-syntax-backward "^-" (point-at-bol))
      (skip-chars-backward "[:digit:]/\.-" (point-at-bol))
      (catch 'done
       (dolist (regexp+format update-date-formats)
         (let ((date-regexp (car regexp+format))
               (date-format (cdr regexp+format)))
           (when (looking-at date-regexp)
             (replace-match (format-time-string date-format (current-time)))
             (throw 'done t))))))))

(defun update-german-date ()
  "*Update a date in German format near point."
  (interactive)
  (error "Use `update-date' instead!"))

(defun show-time-and-date ()
  "*Show current time and date in the echo-area."
  (interactive)
  ;; If one prefers a `date'-like output: "%a %b %d %H:%M:%S  %Y"
  (message (format-time-string "%d. %B %Y  %a (Week %V)   %H:%M:%S" (current-time))))

(defun diff-backup-this-file ()
  "*Diff this file with its backup file or vice versa.
Uses the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'."
  (interactive)
  (if buffer-file-name
      (diff-backup buffer-file-name)
    (error "Buffer \"%s\" is not visiting any file." (buffer-name))))

(defun toggle-variable (var &optional arg) ; adapted from `set-variable'
  "*Toggle the value of VAR.
With argument ARG, set VAR to t if ARG is positive, or to nil otherwise.
Return the new value of VAR."
  (interactive (let* ((default-var (variable-at-point))
                      (var (if (symbolp default-var)
                               (read-variable
                                (format "Toggle variable (default %s): "
                                        default-var) default-var)
                             (read-variable "Toggle variable: "))))
		 (list var current-prefix-arg)))
  (let ((type (get var 'custom-type)))
    (when type
      (unless (eq type 'boolean)
        (error "Not a boolean variable"))))
  (set var (if (null arg)
               (not (symbol-value var))
             (> (prefix-numeric-value arg) 0)))
  (when (interactive-p)
    (message
     (concat "Variable `%S' set to %S"
             (when (local-variable-p var) " in this buffer"))
     var (symbol-value var)))
  (symbol-value var))                   ; return the new value of var

;; See also:
;; http://www.splode.com/users/friedman/software/emacs-lisp/src/win-disp-util.el

(unless (fboundp 'toggle-truncate-lines)
  (defun toggle-truncate-lines (&optional arg)
    "*Toggle the value of the variable `truncate-lines'."
    (interactive "P")
    (setq truncate-partial-width-windows (toggle-variable 'truncate-lines arg))
    ;; If disabling truncation, make sure that window is entirely scrolled
    ;; to the right, otherwise truncation will remain in effect while still
    ;; horizontally scrolled.
    (or truncate-lines
        (scroll-right (window-hscroll)))
    (force-mode-line-update)
    (when (interactive-p)
      (message
       (concat "Line truncation" (if truncate-lines " enabled" " disabled")
               " in this buffer")))))

(defun toggle-stack-trace-on-error (&optional arg)
  (interactive "P")
  (setq stack-trace-on-error (if (null arg)
                                 (not stack-trace-on-error)
                               (> (prefix-numeric-value arg) 0))))

;; Daniel Lundin [http://ftp.codefactory.se/pub/people/daniel/elisp/dot.emacs]
(defun toggle-window-dedicated ()
  "*Toggle whether current window is dedicated or not."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window dedicated to buffer '%s'"
     "Window displaying '%s' is not dedicated anymore.")
   (current-buffer)))

;; http://thread.gmane.org/gmane.emacs.devel/147660/focus=147675
(defun cat-command ()
  "A command for cats."
  (interactive)
  (require 'animate)
  (let ((mouse "
           ___00
        ~~/____'>
          \"  \"")
        (h-pos (floor (/ (window-height) 2)))
        (contents (buffer-substring (window-start) (window-end))))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert contents)
      (setq truncate-lines t)
      (animate-string mouse h-pos 0)
      (dotimes (_ (window-width))
        (sit-for 0.01)
        (dotimes (n 3)
          (goto-line (+ h-pos n 2))
          (move-to-column 0)
          (insert " "))))))


;;; Quick commands for web searches
(setq webjump-sites
      (append
       ;; Thank you, Jorgen Schäfer.
       ;; http://www.emacswiki.org/emacs-fr/JorgenSchaefersEmacsConfig
       '(("google-file" .
          (format "http://www.google.de/search?q=+intitle:\"index+of\" -inurl:htm -inurl:html -inurl:php %s"
                  (read-from-minibuffer "Search for file: ")))
         ("google-message-id" .
          (let ((message-id (read-from-minibuffer "Message-ID: "
                                                  (let ((url (thing-at-point 'url)))
                                                    (when (string-match "mailto:\\(.*\\)" url)
                                                      (match-string 1 url))))))
            (when (string-match "<\\(.*\\)>" message-id)
              (setq message-id (match-string 1 message-id)))
            (format "http://www.google.de/groups?selm=%s" message-id))))

       (mapc
        (lambda (x)
          (setcdr x `(format ,(cdr x) (_init_read_word))))
        '(("leo" . "http://dict.leo.org/?search=%s")
          ("leo.it" . "http://dict.leo.org/itde/?search=%s")
          ("leo.es" . "http://dict.leo.org/esde/?search=%s")
          ("leo.pt" . "http://dict.leo.org/ptde/?search=%s")

          ("wp" . "http://en.wikipedia.org/wiki/Special:Search?search=%s")
          ("wp.de" . "http://de.wikipedia.org/wiki/Special:Search?search=%s")

          ("dwds" . "http://www.dwds.de/?qu=%s")
          ("duden" . "http://www.duden.de/suchen/dudenonline/%s")
          ("synonym" . "http://www.openthesaurus.de/synonyme/%s")

          ("emacswiki" . "http://www.emacswiki.org/cgi-bin/emacs?search=%s")

          ("google" . "http://www.google.de/search?q=%s")))))

(defun _init_read_word ()
  (require 'thingatpt)
  (let ((word (thing-at-point 'word)))
    (url-encode-url
     (if word
         (read-string (format "Word [default \"%s\"]: " word) nil nil word)
       (read-string "Word: ")))))


(defun next-line-not-matching (regexp)
  "*Go to next line *not* matching REGEXP."
  (interactive "sNext line not matching: ")
  (while (search-forward-regexp regexp (point-at-eol) t)
    (forward-line)))

;;; Advices
(defadvice shell-command (before rename-async-buffer activate)
  "Use an unique buffer name for asynchronous commands."
  (when (and (not (ad-get-arg 1))
             (string-match-p "[ \t]*&[ \t]*\\'" (ad-get-arg 0)))
    (require 'cl-macs)
    (cl-do ((i 1 (1+ i))
            (buf "*Async Shell Command*"))
           ((not (get-buffer-process buf)) (ad-set-arg 1 buf))
      (setq buf (format "*Async Shell Command*<%d>" i)))))

;;; Packages
;; sooner or later it will be loaded, so do it now.
(require 'tramp)

;; For all hosts except my local one connect
;; via `ssh' first, and apply `sudo -u root' afterwards:
;; /sudo:lampone:~
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))

;; jka-compr provides transparent access to compressed files.
(require 'jka-compr)
(auto-compression-mode 1)

;; Automatic resizing of help windows.
(temp-buffer-resize-mode +1)

;; enable visiting image files as images.
(if (fboundp 'auto-image-file-mode)
    (auto-image-file-mode 1))

(autoload 'ntcmd-mode "ntcmd"
  "Major mode for editing CMD scripts." t)

;;; imenu
(setq imenu-always-use-completion-buffer-p 'never)

(when (require-soft 'goto-last-change)
  (global-set-key "\C-x\C-\\" 'goto-last-change))

;;; Hippie-expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name))

(global-defkey "M-/" 'hippie-expand)

(setq-default truncate-lines nil)
(setq truncate-partial-width-windows nil)

;;; chop: binary search for a line within a window
(autoload 'chop-move-up "chop")
(autoload 'chop-move-down "chop")
(eval-after-load "chop"
  '(setq chop-lines-near-middle nil))

(global-defkey "S-<up>" 'chop-move-up)
(global-defkey "S-<down>" 'chop-move-down)

(global-defkey "C-," 'chop-move-up)
(global-defkey "C-." 'chop-move-down)

(when (require-soft 'autopair)
  (defun autopair-dont-pair-before-words (action pair pos-before)
    (if (eq action 'opening)
        (let ((char-after-point (char-after (1+ pos-before))))
          (if (and char-after-point
                   (eq (char-syntax char-after-point) ?w))
              t
            (autopair-default-handle-action action pair pos-before)))
      t))
  (setq autopair-handle-action-fns '(autopair-dont-pair-before-words))
  (autopair-global-mode 1)
  (setq autopair-autowrap t))

;;; Gnuplot
(autoload 'gnuplot "gnuplot"
  "Run Gnuplot interactively in a Emacs buffer." t nil)
(autoload 'gnuplot-interaction-mode "gnuplot-interaction"
  "Major mode for editing Gnuplot input files." t nil)

;;; Shell-script
(autoload 'sh-mode "sh-script" 
  "Major mode for editing shell scripts" t nil)
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)
(add-hook 'sh-mode-hook
          (lambda ()
            (setq defun-prompt-regexp "^\\(function[ \t]\\|[^[:space:]]+[ \t]+()[ \t]+\\)")))

;; the-the is a nice thing for text processing:
(autoload 'the-the "the-the"
  "Search forward for for a duplicated word." t nil)

;;; apropos
(defun apropos-function ()
  "*Show functions that match REGEXP."
  (interactive)
  (require 'apropos)
  (let ((apropos-do-all t))
    (call-interactively 'apropos-command)))

;;; ido
(when (and running-interactively
           (require-soft 'ido))
  (ido-mode +1)

  (setq ido-ignore-buffers '("\\` " "\\*\\(?:Quail \\)?Completions\\*"))
  (setq ido-show-dot-for-dired t)

  (add-hook 'ido-setup-hook
            (lambda ()
              (defkey ido-completion-map "<f4>" 'ido-next-match)
              (defkey ido-completion-map "<S-f4>" 'ido-prev-match)))

  (global-defkey "<f4>" 'ido-switch-buffer)

  (global-defkey "<kp-add>" 'ido-switch-buffer)

  (defun ido-bookmark-jump (bname)
    "*Switch to bookmark interactively using `ido'."
    (interactive
     (progn
       (require 'bookmark)
       (list (ido-completing-read "Bookmark: " (bookmark-all-names) nil t))))
    (bookmark-jump bname))

  (substitute-key-definition 'bookmark-jump 'ido-bookmark-jump global-map))

(global-defkey "<kp-subtract>" 'bury-buffer)


(require-soft 'minibuf-isearch)


;;; autoinsert
(setq auto-insert-directory (concat user-emacs-directory "auto-insert/"))
(auto-insert-mode 1)
(add-to-list 'auto-insert-alist '(("/\\.?lib/zsh/" . "ZSH function")
                                  "Short description: "
                                  '(shell-script-mode)
                                  "#!/usr/bin/env zsh
## Emilio Lopes <eclig@gmx.net>

## " (file-name-nondirectory (buffer-file-name)) " --- " str "

## THIS FILE IS IN THE PUBLIC DOMAIN.  USE AT YOUR OWN RISK!

# " (file-name-nondirectory (buffer-file-name)) " () {

emulate -LR zsh

" _ "

# }\n"))
(add-to-list 'auto-insert-alist '(perl-mode . "header.pl"))
(add-to-list 'auto-insert-alist '("\\.pl\\'" . "header.pl"))


;;; winner
(when (require-soft 'winner)
  (winner-mode +1)
  (defkey ctl-x-4-map "u" 'winner-undo)
  (defkey ctl-x-4-map "r" 'winner-redo))


(when (require-soft 'pack-windows)
  (global-defkey "C-x -" 'pack-windows)
  (global-defkey "C-x _" 'shrink-window-if-larger-than-buffer))


;;; completion
(defadvice PC-lisp-complete-symbol (before forward-sexp-before-completion (&optional arg) activate)
  "Do a `forward-sexp' if necessary before trying completion.
With prefix argument ARG behave as usual."
  (interactive "P")
  (unless arg
    (when (looking-at "\\sw\\|\\s_")
      (forward-sexp))))

(if (boundp 'completion-pcm-complete-word-inserts-delimiters)
    (progn
      (setq completion-styles '(partial-completion initials))
      (setq completion-pcm-complete-word-inserts-delimiters t))
  (when (require-soft 'complete)
    (partial-completion-mode 1)))

(setq resize-mini-windows t)

;; Stefan Monnier
;; Press `C-s' at the prompt to search the completion buffer
(defun complete-isearch (regexp)
  "Search in the completions.  If a prefix is given, use REGEXP isearch."
  (interactive "P")
  (unless (and (memq last-command '(minibuffer-complete
        minibuffer-completion-help))
        (window-live-p minibuffer-scroll-window))
    (minibuffer-completion-help))
  (with-current-buffer (window-buffer minibuffer-scroll-window)
    (save-window-excursion
      (select-window minibuffer-scroll-window)
      (if (isearch-forward regexp nil)
   (choose-completion)))))
(defkey minibuffer-local-completion-map "C-s" 'complete-isearch)
(defkey minibuffer-local-must-match-map "C-s" 'complete-isearch)

(defkey minibuffer-local-map "M-N" 'next-complete-history-element)
(defkey minibuffer-local-map "M-P" 'previous-complete-history-element)

(setq completion-ignored-extensions (delete ".pdf" completion-ignored-extensions))


;;; Get the little rodent out of way
(when (and (display-mouse-p)
           (require-soft 'avoid))
  (defun mouse-avoidance-banish-destination ()
    "*The position to which Mouse Avoidance mode `banish' moves the mouse.
Redefined to banish the mouse to the corner of the frame."
    (cons (1+ (frame-width)) (1- (frame-height))))
  (mouse-avoidance-mode 'banish)
  (defun toggle-mouse-avoidance-mode ()
    (interactive)
    (mouse-avoidance-mode)))


;;; Uniquify
(require 'uniquify)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-ignore-buffers-re
      "\\(news\\|mail\\|reply\\|followup\\) message\\*")
;; also uniquify shell-mode buffers
(add-to-list 'uniquify-list-buffers-directory-modes 'shell-mode)


;;; smex
(when (require-soft 'smex)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))


;;; ace-jump
(when (require-soft 'ace-jump-mode)
  (setq ace-jump-mode-scope 'window)

  (setq ace-jump-mode-submode-list
        '(ace-jump-line-mode
          ace-jump-word-mode
          ace-jump-char-mode))

  (define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode))


;;; Time-stamp 
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-active t)
(setq time-stamp-warn-inactive t)
(unless (string-match "^ecl\\(ig\\)?" (user-login-name))
  ;; use full name instead of login name in time-stamps
  (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %U"))

;;; font-lock mode
(when (display-color-p)
  (setq font-lock-maximum-decoration t)
  (global-font-lock-mode 1)
  (setq font-lock-verbose nil)
  (add-hook 'font-lock-mode-hook
            (lambda ()
              (font-lock-add-keywords nil '(("\\*\\(ECL\\|FIXME\\)\\*:?" 0 'show-paren-mismatch-face t))))))

;;; Keybindings
(when running-nt
  (global-defkey "<apps>" 'undo))

;; extra "C-x" for Dvorak keyboard layouts, in the same hand as "s",
;; "f", "w", "v".
(global-defkey "C-z" ctl-x-map)
(or key-translation-map (setq key-translation-map (make-sparse-keymap)))
(define-key key-translation-map "\C-z8" 'iso-transl-ctl-x-8-map)

;; put `previous-line' in the same hand as `next-line' in Dvorak layout
(global-defkey "C-h"     'previous-line)
(global-defkey "C-x C-h" help-map)

(global-defkey "C-M-<backspace>" 'backward-kill-sexp)
(global-defkey "C-M-<delete>" 'backward-kill-sexp)

;; (global-defkey "C-M-<SPC>" 'copy-sexp)
(global-defkey "M-k" 'copy-line)
(global-defkey "M-K" 'kill-sentence)
(global-defkey "M-+" 'delete-horizontal-space-forward)

(defkey esc-map ")" 'up-list)

(global-defkey "C-c 0" (recursive-edit-preserving-window-config (delete-window)))
(global-defkey "C-c 1" (recursive-edit-preserving-window-config
                        (if (one-window-p 'ignore-minibuffer)
                            (error "Current window is the only window in its frame")
                          (delete-other-windows))))
(global-defkey "C-c 2" (recursive-edit-preserving-window-config (split-window-vertically)))
(global-defkey "C-c 3" (recursive-edit-preserving-window-config (split-window-horizontally)))
(global-defkey "C-c 4 b" (recursive-edit-preserving-window-config (ido-switch-buffer-other-window)))
(global-defkey "C-c 4 C-o" (recursive-edit-preserving-window-config (ido-switch-display-buffer)))

(global-defkey "C-c $" 'toggle-truncate-lines)
(global-defkey "C-c \\" 'the-the)
(global-defkey "C-c ;" 'comment-or-uncomment-region)
(global-defkey "C-c ~" 'diff-backup-this-file)

(global-defkey "C-c a" 'show-time-and-date)
;; (global-defkey "C-c b" 'browse-url)

(global-defkey "C-c b" (lambda () (interactive) (mouse-avoidance-banish-mouse)))

(bind-with-new-map (current-global-map) "C-c d"
  ("b" . 'ediff-buffers)
  ("e" . 'eregistry)
  ("f" . 'ediff-files)
  ("d" . 'ediff-directories)
  ("P" . 'ediff-patch-file)
  ("p" . 'ediff-patch-buffer)

  ("m" . 'ediff-merge-buffers)
  ("M" . 'ediff-merge-files)

  ("i" . 'ediff-documentation)
  ("y" . 'ediff-show-registry))

(global-defkey "C-c e" (make-sparse-keymap))
(global-defkey "C-c e b"        'eval-buffer)
(global-defkey "C-c e d"        'byte-recompile-directory)
(global-defkey "C-c e e"        'eval-last-sexp)
(global-defkey "C-c e f"        'byte-compile-file)
(global-defkey "C-c e i"        'indirect-elisp-buffer)
(global-defkey "C-c e r"        'eval-region)
(global-defkey "C-c e t"        'top-level)

(bind-with-new-map (current-global-map) "C-c f"
  ("f" . 'find-function)
  ("k" . 'find-function-on-key)
  ("v" . 'find-variable)
  ("l" . 'find-library))

(bind-with-new-map (current-global-map) "C-c m"
  ("a" . 'abbrev-mode)
;; ("b" . 'toggle-skeleton-pair)         ; "b" as "brackets"
  ("f" . 'auto-fill-mode)
  ("s" . 'flyspell-mode)
  ("p" . 'autopair-mode)
  ("P" . 'paredit-mode)
  ("l" . 'longlines-mode)
  ("m" . 'toggle-mouse-avoidance-mode)
  ("o" . 'orgstruct++-mode)
  ("$" . (lambda ()
           (interactive)
           (setq show-trailing-whitespace (not show-trailing-whitespace))
           (redraw-frame (selected-frame)))))

(global-defkey "C-c j" (make-sparse-keymap))
(global-defkey "C-c j h"        (lambda () (interactive) (dired     "~")))
(global-defkey "C-c j D"        (lambda () (interactive) (dired     (concat (or (getenv "USERPROFILE") "~") "/Downloads"))))
(global-defkey "C-c j p"        (lambda () (interactive) (dired     "e:/qx29999/projs")))
(global-defkey "C-c j d"        (lambda () (interactive) (find-library "init-dired")))
(global-defkey "C-c j e"        (lambda () (interactive) (find-file user-init-file)))
(global-defkey "C-c j m"        (lambda () (interactive) (find-library "message_rc")))
(global-defkey "C-c j g"        (lambda ()
                                  (interactive)
                                  (if (boundp 'gnus-init-file)
                                      (find-file  gnus-init-file)
                                    (find-library  "gnus_rc"))))
(global-defkey "C-c j s"        (lambda () (interactive) (find-library "init-shell")))
(global-defkey "C-c j t"        (lambda () (interactive) (find-file "e:/qx29999/ORG/TODO")))

(global-defkey "C-c j ."        (lambda () (interactive) (find-file "~/.ee.sh")))

(global-defkey "C-c j b"        (lambda () (interactive) (find-file "~/.bash.d/bashrc")))
(global-defkey "C-c j a"        (lambda () (interactive) (find-file "~/.bash.d/aliases")))

(global-defkey "C-c g" 'goto-line)

;; (global-defkey "M-i" 'other-window)

(global-defkey "C-c t" 'insert-date)

(global-defkey "C-c s" 'jump-to-scratch-buffer)
(global-defkey "C-c z" 'jump-to-text-scratch-buffer)

(global-defkey "C-c r" 'rename-buffer)
(global-defkey "C-c u" 'rename-uniquely)
(global-defkey "C-c w" 'copy-filename-as-kill)

(bind-with-new-map help-map "a"
  ("a" . 'apropos)
  ("c" . 'apropos-command)
  ("f" . 'apropos-function)
  ("v" . 'apropos-variable)
  ("d" . 'apropos-documentation)
  ("i" . 'apropos-info)
  ("l" . 'apropos-value))

(defkey help-map "R" 'menu-bar-read-lispref)

(global-defkey "C-x C-q" 'toggle-read-only)
(global-defkey "C-x v q" 'vc-toggle-read-only)

(setq outline-minor-mode-prefix (kbd "C-c C-o"))

(global-defkey "C-M-%"          'query-replace-regexp)
(global-defkey "C-x k"          'kill-this-buffer)
(global-defkey "C-x I"          'insert-buffer)  ; `insert-file' is on "C-x i"

(global-defkey "<home>"         'beginning-of-line)
(global-defkey "<end>"          'end-of-line)

(global-defkey "C-<home>"       'move-to-window-top)
(global-defkey "C-<end>"        'move-to-window-bottom)

(global-defkey "C-<prior>"      'move-to-window-top)
(global-defkey "C-<next>"       'move-to-window-bottom)

(global-defkey "C-x <left>"  'windmove-left)
(global-defkey "C-x <right>" 'windmove-right)
(global-defkey "C-x <up>"    'windmove-up)
(global-defkey "C-x <down>"  'windmove-down)

(global-defkey "S-<backspace>"  'backward-delete-char-hungry)

(global-defkey "<f1>"           'info)
(global-defkey "S-<f1>"         'woman)

(global-defkey "<f2>"           'save-buffer)
(global-defkey "S-<f2>"         'revert-buffer-preserve-modes)

(global-defkey "<f3>"           'dired-jump)
(global-defkey "S-<f3>"         'shell-here)

;; (global-defkey "<f4>"           'anything-switch-buffer)
(global-defkey "S-<f4>"         'bury-buffer)

(global-defkey "<f5>"           'resize-window)
(global-defkey "S-<f5>"         'swap-window-positions)

(global-defkey "<f7>"           'insert-default-register)
(global-defkey "S-<f7>"         'copy-to-default-register)

(global-defkey "<f8>"           'grep)
(global-defkey "S-<f8>"         'grep-find)

(global-defkey "<f9>"           'next-error)
(global-defkey "S-<f9>"         'compile)

(global-defkey "<f10>"           'call-last-kbd-macro)
(global-defkey "S-<f10>"         'apply-macro-to-region-lines)

;; (global-defkey "<f12>"           'toggle-window-dedicated)

;; Make <f12> act like Hyper, for keyboards without it. Just like
;; <f11> acts as Meta on older DEC terminals.
;; Must undef it first for the function-key-map binding to work
;; (global-unset-key [f12])
;; (define-key function-key-map [f12] 'event-apply-hyper-modifier)

(global-defkey "<f12>" 'imenu)

(global-defkey "C-<return>" 'repeat)

(global-defkey "<scroll>" 'toggle-window-dedicated) ; that's `scroll-lock'

(global-defkey "<find>"         'isearch-forward)
(global-defkey "<execute>"      'execute-extended-command)

(global-defkey "<print>"        'ps-spool-buffer-with-faces)
(global-defkey "S-<print>"      'set-default-printer)

;;; Time (and date) display setup.
(display-time-mode 1)
(setq display-time-interval 5)
(setq display-time-day-and-date nil)
(setq display-time-24hr-format t)
(setq display-time-use-mail-icon t)
(set-time-zone-rule nil)

;;; Common modes stuff
;; Add some suffix defs to auto-mode-alist:
(dolist (f (list auto-mode-alist interpreter-mode-alist))
  (while (rassq 'perl-mode f)
    (setcdr (rassq 'perl-mode f) 'cperl-mode)))
(setq auto-mode-alist (append '(("\\.\\([bB][aA][tT]\\|[cC][mM][dD]\\)\\'" . ntcmd-mode)
                                ("[^/]\\.dired\\'" . dired-virtual-mode)
                                ("\\.fi\\'" . fortran-mode)
                                ("\\.bash_\\(functions\\|aliases\\)\\'" . sh-mode)
                                ("\\.\\(SCORE\\|ADAPT\\)\\'" . gnus-score-mode)
                                ("\\.gpt?\\'" . gnuplot-interaction-mode)
                                ("\\.mak\\'" . makefile-mode)
                                ("\\.xslt?\\'" . xml-mode)
                                ("\\.col\\'" . c-mode)
                                ("\\.hol\\'" . c-mode)
                                ("\\.kgs\\'" . c-mode)
                                ("\\.dtx\\'" . latex-mode))
                              auto-mode-alist))

;;; kill-ring
(setq kill-ring-max 1024)
(setq save-interprogram-paste-before-kill t)
(setq x-select-enable-clipboard t)
(setq select-active-regions nil)
(when (eq window-system 'x)
  (setq x-select-enable-primary t))

;; Thank you, Karl Fogel.
;; http://svn.red-bean.com/repos/kfogel/trunk/.emacs
(defun browse-kill-ring ()
  "Browse the kill ring."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Browse Kill Ring*"))
  (widen)
  (delete-region (point-min) (point-max))
  (mapcar
   (lambda (str)
     ;; We could put the full string as a text property on the summary
     ;; text displayed, but with yank-match available, there's no need.
     (insert (substring str 0 (min (length str) 72))
             "\n-*- -*- -*- -*- -*-\n"))
   kill-ring)
  (read-only-mode 1)
  (goto-char (point-min)))

(global-defkey "<f11>" 'browse-kill-ring)

;; From http://lists.gnu.org/archive/html/emacs-devel/2008-03/msg00128.html
;; See also http://svn.red-bean.com/repos/kfogel/trunk/code/yank-match/
(defun insert-yank-from-kill-ring (string)
  "Insert the selected item from the kill-ring in the minibuffer history.
Use minibuffer navigation and search commands to browse the kill-ring
in the minibuffer history."
  (interactive (list (read-string "Yank from kill-ring: " nil 'kill-ring)))
  (insert-for-yank string))

(setq kill-do-not-save-duplicates t)

(defun yank-pop-forward (arg)
"Like `yank-pop' but insert a more recent kill."
  (interactive "p")
  (yank-pop (- arg)))

(global-defkey "M-Y" 'yank-pop-forward)

;;; nxml
;; (setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
;; (defun nxml-kill-element (&optional arg)
;;   "*Kill the following element.
;; With optional argument ARG kill the next ARG elements."
;;   (interactive "*")
;;   (kill-region (point)
;;                (save-excursion 
;;                  (nxml-forward-element arg)
;;                  (point))))
(eval-after-load "nxml-mode"
  '(defun nxml-where ()
     "Display the hierarchy of XML elements the point is on as a path."
     (interactive)
     (let ((path nil))
       (save-excursion
         (save-restriction
           (widen)
           (while (condition-case nil
                      (progn
                        (nxml-backward-up-element) ; always returns nil
                        t)       
                    (error nil))
             (setq path (cons (xmltok-start-tag-local-name) path)))
           (message "/%s" (mapconcat 'identity path "/")))))))

;; (add-hook 'nxml-mode-hook
;;           (lambda ()
;;             (defkey nxml-mode-map "C-c M-k" 'nxml-kill-element)))



;;; TeX
(setq tex-dvi-view-command
      (if (eq window-system 'x) "xdvi" "dvi2tty -q * | cat -s"))
(setq tex-dvi-print-command "dvips")
(setq tex-alt-dvi-print-command
      '(format "dvips -P%s" (read-string "Use printer: " ps-printer-name)))
(setq tex-open-quote "\"")              ; disable "smart quoting".
(setq tex-close-quote "\"")


;;; AUCTeX
(when (require-soft 'tex-site)
  (setq LaTeX-math-abbrev-prefix "#")
  (setq TeX-open-quote "\"")              ; disable "smart quoting".
  (setq TeX-close-quote "\"")
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (TeX-global-PDF-mode 1)
              (setq  TeX-show-compilation nil)
              (setq LaTeX-default-environment "equation")
              ;;             (local-defkey "{" 'TeX-insert-braces)
              ;;             (local-defkey "}" 'up-list)
              (local-defkey "S-<f9>" (lambda ()(interactive)
                                       (let ((TeX-show-compilation nil))
                                         (TeX-command "LaTeX" 'TeX-master-file))))
              (local-defkey "<f9>" 'TeX-next-error))))


;;; reftex
(setq reftex-insert-label-flags '("s" "sfte"))
(setq reftex-label-alist
      '(("equation" ?e "eq:" "~\\eqref{%s}" t (regexp "equations?" "eqs?\\." "eqn\\." "Gleichung\\(en\\)?"  "Gl\\."))))


;;; BBDB
(when (require-soft 'bbdb)
  (bbdb-initialize 'gnus 'message)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
  (setq bbdb-dwim-net-address-allow-redundancy t)
  (setq bbdb-use-pop-up t)
  (setq bbdb-display-layout 'one-line)
  (setq bbdb-pop-up-display-layout 'one-line)
  (setq bbdb-complete-name-allow-cycling t)
  (setq bbdb-offer-save 'save)
  (setq bbdb-quiet-about-name-mismatches 0)
  (setq bbdb-user-mail-names "\\(ecl\\(ig\\)?\\|[Ee]milio[_.][Ll]opes\\)@.+")
  ;;(setq bbdb/mail-auto-create-p t)
  ;;(setq bbdb/news-auto-create-p t)
  (setq bbdb-north-american-phone-numbers-p nil)
  (setq bbdb-check-zip-codes-p nil))

;; BrYan P. Johnson (bilko@onebabyzebra.com)
(defadvice bbdb-complete-name (after bbdb-complete-name-default-domain activate)
  (let* ((completed ad-return-value))
    (when (null completed)
      (expand-abbrev))))


;;; SES
(eval-after-load "ses" '(require-soft 'ses-formulas))


;;; Scheme/Lisp modes

(require 'init-lisp)

(defun jump-to-scratch-buffer (&optional arg)
  "*Switch to buffer `*scratch*', creating it if necessary.
With prefix arg clear the buffers content."
  (interactive "P")
  (switch-to-buffer-create "*scratch*" 'lisp-interaction-mode arg))


;;; Perl mode
;; use cperl-mode as default
;; (defalias 'perl-mode 'cperl-mode)
(setq cperl-hairy nil)
(setq cperl-font-lock t)
(setq cperl-clobber-lisp-bindings nil)
(setq cperl-lazy-help-time 1)
(setq cperl-info-page "Perl")

(setq perl-indent-level 2)

(defun perl-insert-dumper (var)
  "Insert a Perl print statement to print out variable VAR.
If VAR begins with one of `@%$' use `Data::Dumper'."
  (interactive "*MVariable: ")
  (insert
   (format (if (string-match "^[@%$].+" var)
               "print \"+++ \\%s: \", Dumper(\\%s), \"\\n\";"
             "print \"+++ %s: $%s\\n\";") var var)))
(add-hook 'cperl-mode-hook
	  (lambda ()
            (cperl-lazy-install)
            (when (fboundp 'skeleton-pair-insert-maybe)
              (fset 'cperl-electric-paren 'skeleton-pair-insert-maybe)
              (fset 'cperl-electric-rparen 'self-insert-command))
	    (set-compile-command "perl -cw %s")))


;;; PHP
(require-soft 'php-mode)

(defun php (symbol)
  (require 'thingatpt)
  (interactive (list 
                (let ((symbol (thing-at-point 'symbol)))
                  (if symbol
                      (read-string (format "Symbol [default \"%s\"]: " symbol) nil nil symbol)
                    (read-string "Symbol: ")))))
  
  (browse-url (format "http://www.php.net/%s" (string-make-unibyte symbol))))


;;; Compile
(setq compile-command "make ")
(setq compilation-read-command nil)
(setq compilation-ask-about-save t)
(setq compilation-scroll-output t)

(defun set-compile-command (command)
  "*Locally set `compile-command' to COMMAND.
Look for a file named `GNUmakefile', `Makefile' or `makefile' in the buffer's
directory. If it can not be found, set `compile-command' locally to COMMAND.
An occurence of \"%s\" in COMMAND is substituted by the filename."
  (unless (or (file-exists-p "GNUmakefile")
              (file-exists-p "Makefile")
              (file-exists-p "makefile")
              (null buffer-file-name))
    (set (make-local-variable 'compile-command)
         (format command (file-name-nondirectory buffer-file-name)))))


;;; Makefile mode
;; (setq makefile-electric-keys t)
(add-hook 'makefile-mode-hook
          (lambda ()
            (modify-syntax-entry ?. "_"  makefile-mode-syntax-table)))


;;; Sendmail configuration
;;(setq mail-user-agent 'gnus-user-agent)
(setq mail-user-agent 'message-user-agent)

(setq mail-archive-file-name "~/Mail/Outgoing")

(setq user-full-name "Emilio C. Lopes")
(setq user-mail-address "eclig@gmx.net")
(setq mail-default-reply-to "eclig@gmx.net")

(setq mail-from-style nil)

(setq mail-aliases t)
(setq mail-personal-alias-file "~/.mailrc")

(setq mail-yank-prefix "> ")

(add-hook 'mail-setup-hook 'mail-abbrevs-setup)


;;; Message
(add-hook 'message-load-hook (lambda () (require-soft 'message_rc)))


;;; Resume/Server configuration.
;; With these hooks and using emacs.bash (or emacs.csh), both from
;; "etc" dir, it is possible to specify arguments when resuming emacs
;; after a suspension.
(when running-interactively
  (add-hook 'suspend-hook 'resume-suspend-hook)
  (add-hook 'suspend-resume-hook 'resume-process-args)
  (server-start))


;;; Abbrevs
(setq save-abbrevs 'silently)
(quietly-read-abbrev-file)


;;; Bookmarks
(setq bookmark-default-file (locate-user-emacs-file "bookmarks" ".emacs.bookmarks"))
(setq bookmark-save-flag 1)

(defadvice bookmark-load (before prefix-arg-revert activate)
  "A prefix arg is interpreted to specify (non-nil) REVERT."
  (if (interactive-p)
      (ad-set-arg 1 current-prefix-arg)))

(defun bookmark-location-as-kill ()
  "*Copy location of this bookmark to the kill ring."
  (interactive)
  (if (bookmark-bmenu-check-position)
      (let ((location (bookmark-location (bookmark-bmenu-bookmark))))
        (kill-new location)
        (message location))))

(defun my-bookmark-load-hook ()
  (defkey bookmark-bmenu-mode-map "<RET>" 'bookmark-bmenu-this-window)
  (defkey bookmark-bmenu-mode-map "w" 'bookmark-location-as-kill))

(add-hook 'bookmark-load-hook 'my-bookmark-load-hook)


;;; folding
(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)


;;; Buffer selection
(setq bs-configurations
  '(("files" nil nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)
    ("dired" nil nil nil (lambda (b)
                           (with-current-buffer b
                             (not (eq major-mode 'dired-mode)))) bs-sort-buffer-interns-are-last)
    ("shell" nil nil nil (lambda (b)
                           (with-current-buffer b
                             (not (eq major-mode 'shell-mode)))) bs-sort-buffer-interns-are-last)))

(global-defkey "C-x C-b" 'bs-show)

;;; Dired
(eval-after-load "dired" '(require-soft 'init-dired))
(autoload 'dired-jump "dired"
  "Jump to Dired buffer corresponding to current buffer." t)


;;; Text-mode
(require-soft 'init-text)


;;; Org-mode
(setq org-startup-folded nil)           ; no folding on startup

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "DEFERRED(z)"))))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("STARTED" :foreground "blue" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("HOLD" :foreground "magenta" :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)
        ("DEFERRED" :foreground "blue" :weight bold)))

(setq org-use-fast-todo-selection t)


;;; Shell and Comint
(defun shell-dwim (&optional create)
  "Start or switch to an inferior shell process, in a smart way.
If a buffer with a running shell process exists, simply switch to
that buffer.
If a shell buffer exists, but the shell process is not running,
restart the shell.
If already in an active shell buffer, switch to the next one, if
any.

With prefix argument CREATE always start a new shell."
  (interactive "P")
  (let* ((next-shell-buffer
          (catch 'found
            (dolist (buffer (reverse (buffer-list)))
              (when (string-match "^\\*shell\\*" (buffer-name buffer))
                (throw 'found buffer)))))
         (buffer (if create
                     (generate-new-buffer-name "*shell*")
                   next-shell-buffer)))
    (shell buffer)))

(defun shell-here (&optional dir)
  (interactive)
  (let* ((dir (or dir
                  (and (eq major-mode 'dired-mode)
                       (save-excursion
                         (dired-next-subdir 0 'noerror)
                         (dired-get-subdir)))
                  default-directory))
         (buffs (mapcar (lambda (buffer)
                          (with-current-buffer buffer
                            (and (eq major-mode 'shell-mode)
                                 (string= (expand-file-name dir) (expand-file-name default-directory))
                                 buffer)))
                        (buffer-list))))
    (setq buffs (delq nil buffs))
    (if (null buffs)
        (let ((default-directory dir))
          (shell (generate-new-buffer-name "*shell*")))
      (shell (car buffs)))))

(eval-after-load "shell" '(require-soft 'init-shell))


;;; view-file
(eval-after-load "view"
  '(progn
     (substitute-key-definition 'View-quit 'View-exit-no-restore view-mode-map)
     (defkey view-mode-map "e" 'View-exit-and-edit)
     (defkey view-mode-map "b" 'View-scroll-page-backward)))
(defun View-exit-no-restore ()
  "*Quit View mode, without trying to restore window or buffer to previous state."
  (interactive)
  (let ((view-return-to-alist nil))
    (view-mode-exit nil view-exit-action)))
(setq-default view-exit-action 'kill-buffer)



;;; grep
(setq grep-command "grep -s -n ")

(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-directories "_darcs")
     (define-key grep-mode-map "F"
       (lambda (regexp) (interactive "sFlush lines matching: ")
         "Delete lines containing matches for REGEXP."
         (let ((buffer-read-only nil))
           (flush-lines regexp))))
     (define-key grep-mode-map "K"
       (lambda (regexp) (interactive "sKeep only lines matching: ")
         "Delete all lines except those containing matches for REGEXP."
         (let ((buffer-read-only nil))
           (keep-lines regexp))))))

(when (require-soft 'gack)
 (global-defkey "<f8>" 'ack))


;;; calendar
(add-hook 'calendar-load-hook
          (lambda ()
            (require-soft 'init-calendar)))


;;; Occur
(defun my-occur-mode-hook ()
  (defkey occur-mode-map "n" 'occur-next)
  (defkey occur-mode-map "<down>" 'occur-next)
  (defkey occur-mode-map "p" 'occur-prev)
  (defkey occur-mode-map "<up>" 'occur-prev)
  ;;Occur - Kin Cho <kin@dynarc.com>
  (define-key occur-mode-map "F"
    (lambda (regexp) (interactive "sFlush lines matching: ")
      "Delete lines containing matches for REGEXP."
      (let ((buffer-read-only nil))
        (flush-lines regexp))))
  (define-key occur-mode-map "K"
    (lambda (regexp) (interactive "sKeep only lines matching: ")
      "Delete all lines except those containing matches for REGEXP."
      (let ((buffer-read-only nil))
        (keep-lines regexp)))))
(add-hook 'occur-mode-hook 'my-occur-mode-hook)

(defun occur-shrink-window ()
  "*Shrink the \"*Occur*\" window as much as possible to display its contents."
  (let ((win (get-buffer-window "*Occur*")))
    (when (windowp win)
      (shrink-window-if-larger-than-buffer win))))
(add-hook 'occur-hook 'occur-shrink-window)



;;; Diff/Ediff

(setq diff-switches "--unified")
(setq diff-default-read-only t)
(setq diff-advance-after-apply-hunk nil)

(eval-after-load "diff-mode"
  '(progn
     (add-hook 'diff-mode-hook
               (lambda ()
                 (diff-auto-refine-mode -1)
                 (defkey diff-mode-map "M-k" nil) ; don't override global binding
                 (defkey diff-mode-map "C-c C-k" 'diff-hunk-kill)))))

(defun vc-diff-dwim (file &optional historic)
  "*Display diffs between file revisions.
If FILE was changed with respect to the latest version in the
version control system, show the differences to its current
state.  If FILE has not been edited by the user, ask if we should
compare the current revision of FILE with its predecessor.

With a prefix argument HISTORIC, prompt for two revision
designators specifying the revisions to compare."
  (interactive (list (buffer-file-name) current-prefix-arg))
  (if historic
      (call-interactively 'vc-version-diff)
    (let ((backend (vc-backend file)))
      (vc-state-refresh file backend)
      (if (and (memq (vc-state file) '(up-to-date needs-update))
               (called-interactively-p)
               (y-or-n-p "This working file is unmodified.  Compare this revision with its predecessor? "))
          (let* ((current (vc-working-revision file))
                 (previous (vc-call-backend backend 'previous-revision file current)))
            (vc-version-diff file previous current))
        (vc-diff)))))

(global-defkey "C-x v -" 'vc-diff-dwim)

(setq ediff-keep-variants nil)

(add-hook 'ediff-load-hook
          (lambda ()
            (setq ediff-custom-diff-options "--unified")

            (setq ediff-window-setup-function 'ediff-setup-windows-plain)

            (add-hook 'ediff-keymap-setup-hook
                      (lambda ()
                        (defkey ediff-mode-map "q" 'ediff-quit-no-questions)))
            
            (add-hook 'ediff-before-setup-hook
                      (lambda ()
                        (setq ediff-saved-window-configuration (current-window-configuration))))

            (let ((restore-window-configuration
                   (lambda ()
                     (set-window-configuration ediff-saved-window-configuration))))
              (add-hook 'ediff-quit-hook restore-window-configuration 'append)
              (add-hook 'ediff-suspend-hook restore-window-configuration 'append))

            (setq ediff-split-window-function (lambda (&optional arg)
                                                (if (> (frame-width) 150)
                                                    (split-window-horizontally arg)
                                                  (split-window-vertically arg))))

            (setq-default ediff-forward-word-function 'forward-char)

            (defadvice ediff-windows-wordwise (before invert-prefix-arg activate)
              "Invert the sense of the prefix argument, when run interactively."
              (when (interactive-p)
                (ad-set-arg 0 (not (ad-get-arg 0)))))))

(defun ediff-quit-no-questions (reverse-default-keep-variants)
  "Quit ediff without prompting."
  (interactive "P")
  (ediff-really-quit reverse-default-keep-variants))

(defun vc-ediff-dwim (file &optional historic)
  "*Compare revisions of FILE using Ediff.
If FILE was changed with respect to the latest version in the
version control system, show the differences to its current
state.  If FILE has not been edited by the user, ask if we should
compare the current revision of FILE with its predecessor.

With a prefix argument HISTORIC, prompt for two revision
designators specifying the revisions to compare."
  (interactive (list (buffer-file-name) current-prefix-arg))
  (require 'ediff)
  (ediff-load-version-control)
  (cond
   (historic
    (ediff-revision file))
   ((memq (vc-state file) '(up-to-date needs-update))
    (unless (and (called-interactively-p)
                 (not (y-or-n-p "This working file is unmodified.  Compare this revision with its predecessor? ")))
      (let* ((backend (vc-backend file))
             (current (ediff-vc-working-revision file))
             (previous (vc-call-backend backend 'previous-revision file current)))
        (ediff-vc-internal previous "" nil))))
   (t (ediff-vc-internal "" "" nil))))

(global-defkey "C-x v =" 'vc-ediff-dwim)

;;; Adapted from Dave Love's "fx-misc.el", http://www.loveshack.ukfsn.org/emacs
(defun ediff-diff-buffer-with-saved ()
  "*Run Ediff between the (modified) current buffer and the buffer's file.

A new buffer is created containing the disc file's contents and
`ediff-buffers' is run to compare that with the current buffer."
  (interactive)
  (unless (buffer-modified-p)
    (error "Buffer isn't modified"))
  (let ((current (buffer-name))
        (file (or (buffer-file-name)
                  (error "Current buffer isn't visiting a file")))
        (mode major-mode))
    (with-current-buffer (get-buffer-create (format "*%s-on-disc*" current))
      (buffer-disable-undo)
      (erase-buffer)
      (insert-file-contents file)
      (set-buffer-modified-p nil)
      (funcall mode)

      (ediff-buffers (buffer-name) current))))


;;; Printing
(when (require-soft 'printing)
  (pr-update-menus t))

(defun set-default-printer (printer)
  "*Change the default printer."
  (interactive (list (read-from-minibuffer "Printer: " printer-name)))
  (setq printer-name printer)
  (setq ps-printer-name printer-name))


;; PS printing
(setq ps-paper-type 'a4)
(setq ps-print-header t)
(setq ps-print-header-frame t)
(setq ps-print-color-p nil)

;; (setq ps-font-family 'Times)
;; (setq ps-font-size '(8.5 . 11))

(setq ps-font-family 'Courier)
(setq ps-font-size   '(7 . 8.5))


(setq ps-right-header
      (list "/pagenumberstring load"
            'ps-time-stamp-yyyy-mm-dd 'ps-time-stamp-hh:mm:ss))

(defun ps-time-stamp-yyyy-mm-dd ()
  "*Return the date as, for example, \"2003-02-18\"."
  (format-time-string "%Y-%m-%d"))

;; One page per sheet
(setq ps-landscape-mode nil)
(setq ps-number-of-columns 1)

;; Two pages per sheet
;; (setq ps-landscape-mode t)
;; (setq ps-number-of-columns 2)

(defun ps-print-buffer-no-header ()
  (interactive)
  (let ((ps-print-header nil)
        (ps-print-header-frame nil))
    (funcall (if window-system 'ps-print-buffer-with-faces 'ps-print-buffer))))

(defun ps-print-buffer-maybe-with-faces-landscape ()
  (interactive)
  (let ((ps-landscape-mode nil)
        (ps-number-of-columns 1))
    (funcall (if window-system 'ps-print-buffer-with-faces 'ps-print-buffer))))

(defun ps-print-buffer-maybe-with-faces-n-up (&optional n)
  (interactive)
  (let ((ps-landscape-mode t)
        (ps-number-of-columns (or n 2)))
    (funcall (if window-system 'ps-print-buffer-with-faces 'ps-print-buffer))))

(global-defkey "C-c p" (make-sparse-keymap))

(global-defkey "C-c p p" (if window-system 'ps-print-buffer-with-faces 'ps-print-buffer))

(global-defkey "C-c p s" (if window-system 'ps-spool-buffer-with-faces 'ps-spool-buffer))

(global-defkey "C-c p b" (if window-system 'ps-print-buffer-with-faces 'ps-print-buffer))
(global-defkey "C-c p B" (if window-system 'ps-spool-buffer-with-faces 'ps-spool-buffer))
(global-defkey "C-c p r" (if window-system 'ps-print-region-with-faces 'ps-print-region))
(global-defkey "C-c p R" (if window-system 'ps-spool-region-with-faces 'ps-spool-region))

(global-defkey "C-c p l" 'ps-print-buffer-maybe-with-faces-landscape)
(global-defkey "C-c p n" 'ps-print-buffer-maybe-with-faces-n-up)

(global-defkey "C-c p S" 'set-default-printer)


;;; Calc
(setq calc-full-mode t)
(setq calc-display-trail nil)



;;; Man
(eval-after-load "man"
  '(progn
     (setq Man-notify-method 'friendly)
     (defkey Man-mode-map "q" 'Man-kill)))



;;; Woman
(setq woman-use-own-frame nil)



;;; Browse URL
(setq browse-url-new-window-flag nil)
(setq browse-url-mozilla-new-window-is-tab t)


;;; Custom
(setq custom-file (locate-user-emacs-file "custom.el" ".custom"))
(when (file-readable-p custom-file)
  (load-file custom-file))



;;; isearch
(setq isearch-allow-scroll t)

(defun isearch-recenter ()
  (interactive)
  (recenter-top-bottom)
  (isearch-update))
(defkey isearch-mode-map "C-l" 'isearch-recenter)

(defun isearch-yank-sexp ()
  "*Pull next expression from buffer into search string."
  (interactive)
  (isearch-yank-internal (lambda () (forward-sexp 1) (point))))
(defkey isearch-mode-map "C-v" 'isearch-yank-sexp)

(defun isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))
(defkey isearch-mode-map "C-y" 'isearch-yank-symbol)

;; Alex Schroeder [http://www.emacswiki.org/cgi-bin/wiki/OccurBuffer]
(defun isearch-occur ()
  "*Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(defkey isearch-mode-map "<mouse-2>" 'isearch-yank-kill)
(defkey isearch-mode-map "<down-mouse-2>" nil)
(defkey isearch-mode-map "C-c" 'isearch-toggle-case-fold)
(defkey isearch-mode-map "C-t" 'isearch-toggle-regexp)
(defkey isearch-mode-map "C-o" 'isearch-occur)
(defkey isearch-mode-map "M-k" 'isearch-yank-line)
(defkey isearch-mode-map "C-h" 'isearch-mode-help)

;; Juri Linkov [http://www.jurta.org/emacs/dotemacs.el]
(defun isearch-beginning-of-buffer ()
  "Move isearch point to the beginning of the buffer."
  (interactive)
  (goto-char (point-min))
  (isearch-repeat-forward))

(defun isearch-end-of-buffer ()
  "Move isearch point to the end of the buffer."
  (interactive)
  (goto-char (point-max))
  (isearch-repeat-backward))

(defkey isearch-mode-map "M-<" 'isearch-beginning-of-buffer)
(defkey isearch-mode-map "M->" 'isearch-end-of-buffer)


;;; Misc

;; see http://angg.twu.net/eev-article.html
(defun ee (s e)
  "Save the region in a temporary script"
  (interactive "r")
  (write-region s e "~/.ee.sh"))

(global-defkey "<down-mouse-3>" 'mouse-major-mode-menu)

;;; Misc history
;; Add *all* visited files to `file-name-history', no matter if they
;; are visited through Dired or gnuclient or whatever.
(defun add-file-name-to-history ()
  "*Add the name of the visited file to `file-name-history'."
  (let ((fn (or buffer-file-name
                (and (eq major-mode 'dired-mode) default-directory))))
    (when fn
      (setq file-name-history (cons fn (delete fn file-name-history)))))
  nil)

(add-hook 'find-file-hook 'add-file-name-to-history)
(add-hook 'write-file-functions 'add-file-name-to-history)
(add-hook 'dired-after-readin-hook 'add-file-name-to-history)

;;; periodically kill old buffers
(require 'midnight)


;;; Local Configuration

(when (equal system-type 'windows-nt)
  (require-soft 'init-w32))

(when (getenv "BMW")
  (require-soft 'init-bmw))

;;; EOF

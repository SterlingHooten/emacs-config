;;; GNU Emacs initialization file ("~/.emacs"). -*- mode: Emacs-Lisp -*-
;;; Emilio C. Lopes
;;; Time-stamp: <2010-10-21 12:33:48 Emilio C. Lopes>

;;; TODO:
;; o Use `add-to-list' and similars for adding things to alists.
;; o Review mode-alist changes.
;; o Use `eval-after-load' for customization of packages.
;; o Review use of "extra" packages. Some are not necessary anymore
;;   while some others could be added.
;; o Cleanup.
;; o Index, with page breaks between the "sections".
;; o Other things. Search for "TODO".

;;{{{ Language things
;;(setq default-enable-multibyte-characters nil)
(setq edmacro-eight-bits t)

;; (add-hook 'set-language-environment-hook
;;           (lambda ()
;;             (when (equal current-language-environment "German")
;;               (setq default-input-method "german-prefix"))))

;; (set-language-environment "German")

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
;;}}}
;;{{{ Useful defs

(setq hostname (substring system-name 0 (string-match "\\..+" system-name)))

(setq running-interactively (not noninteractive))
(setq running-nt (equal system-type 'windows-nt))


(setq at-home (string-match "\\`manaira\\b" system-name))
(setq at-bmw (equal (getenv "BMW") "BMW"))

(setq user-emacs-directory "~/.emacs.d/")

(defun add-to-path (path dir &optional append)
  "*Add directory DIR to path PATH.
If optional argument APPEND is non-nil, DIR is added at the end."
  (setq dir (expand-file-name dir))
  (and (file-directory-p dir) (file-accessible-directory-p dir)
       (add-to-list path dir append)))

(defmacro global-defkey (key def)
  "*Bind KEY globally to DEF.
KEY should be a string constant in the format used for
saving keyboard macros (cf. `insert-kbd-macro')."
  `(global-set-key (kbd ,key) ,def))

(defmacro local-defkey (key def)
  "*Bind KEY locally to DEF.
KEY should be a string constant in the format used for
saving keyboard macros (cf. `insert-kbd-macro')."
  `(local-set-key (kbd ,key) ,def))

(defmacro defkey (keymap key def)
  "*Define KEY to DEF in KEYMAP.
KEY should be a string constant in the format used for
saving keyboard macros (cf. `insert-kbd-macro')."
  `(define-key ,keymap (kbd ,key) ,def))

(defmacro bind-with-new-map (map binding &rest bindings)
  (let ((%map (make-symbol "mapu")))
    `(let ((,%map (make-sparse-keymap)))
       ,@(mapcar (lambda (char+command)
                   `(define-key ,%map (read-kbd-macro ,(car char+command)) ,(cdr char+command)))
                 bindings)
       (define-key ,map ,(read-kbd-macro binding) ,%map))))
(put 'bind-with-new-map 'lisp-indent-function 2)

(defmacro require-soft (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror))

(defmacro mapc-pair (proc seq)
  "Apply PROC to each element of SEQ, a sequence of pairs.
PROC should accept two arguments: the car and the cdr of each
pair. PROC is called for side effects only, don't accumulate the
results "
  `(mapc (lambda (x)
           (funcall ,proc (car x) (cdr x))) ,seq))

;;}}}
;;{{{ Load-path
(add-to-path 'load-path "~/.emacs.d")
(add-to-path 'load-path "~/.emacs.d/lib")
(add-to-path 'load-path "~/.lib/emacs/elisp")
(add-to-path 'load-path "~/.lib/emacs/rc")

;;}}}
;;{{{ System-dependent configuration

(when running-nt

  (setq user-login-name (downcase user-login-name))

  (setq focus-follows-mouse nil)
  (auto-raise-mode -1)

  ;;(set-selection-coding-system 'cp1252)
  (setq w32-enable-synthesized-fonts nil)

  ;;(setq w32-enable-caps-lock nil)
  (setq w32-alt-is-meta t)
  (setq w32-pass-alt-to-system nil)

  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'hyper)
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'hyper)

  ;; Make `C-Tab' an alias for `M-Tab'
  ;;(define-key function-key-map [(control tab)] [?\M-\t])

  ;; Don't pass `M-Tab´ to the system
  ;;(w32-register-hot-key [(alt tab)])

  (defun w32-toggle-meta-tab ()
    "*Toggle passing of the key combination M-TAB to the system."
    (interactive)
    (unless (fboundp 'w32-registered-hot-keys)
      (error "This command is not available on your system"))
    (let ((hotkeys (w32-registered-hot-keys))
          hotkey found)
      (while (and (setq hotkey (pop hotkeys)) (not found))
        (when (equal '(meta tab) (w32-reconstruct-hot-key hotkey))
          (w32-unregister-hot-key hotkey)
          (setq found t)))
      (if (not found)
          (w32-register-hot-key [(alt tab)]))))

  (setq w32-enable-num-lock nil)
  (global-defkey "<kp-numlock>" 'w32-toggle-meta-tab)
  ;;(global-defkey "<apps>" 'w32-toggle-meta-tab)

  (defun normalize-file-path-on-kill-ring ()
    "*Substitute the filename on the kill-ring with its canonical form.
The canonical form is the result of applying `expand-file-name'
to the filename."
    (interactive)
    (kill-new (replace-regexp-in-string "/"
                                        "\\\\"
                                        (expand-file-name (current-kill 0 'do-not-move)))
              'replace))
  (global-defkey "C-c k" 'normalize-file-path-on-kill-ring)

  ;; handle cygwin mount points
  (when (require-soft 'cygwin-mount)
    (cygwin-mount-activate))

  (let ((bg-mode 'light)
        (bg "white")                    ; or "tan"
        (fg "black"))
;;     (set-background-color bg)
;;     (set-foreground-color fg)
    (add-to-list 'initial-frame-alist `(background-mode  . ,bg-mode))
    (add-to-list 'initial-frame-alist `(background-color . ,bg))
    (add-to-list 'initial-frame-alist `(foreground-color . ,fg))
    (add-to-list 'default-frame-alist `(background-mode  . ,bg-mode))
    (add-to-list 'default-frame-alist `(background-color . ,bg))
    (add-to-list 'default-frame-alist `(foreground-color . ,fg)))
  (if (display-color-p)
      (progn
        (set-face-foreground 'mode-line "white")
        (set-face-background 'mode-line "royalblue")
;;         (set-face-foreground 'fringe "white")
;;         (set-face-background 'fringe "deepskyblue")
        (set-face-foreground 'fringe "slategray")
        (set-face-background 'fringe "white")
        ;; (set-cursor-color "blue")
        (set-cursor-color "MediumSeaGreen")
        (set-mouse-color "red"))
    (progn
      (set-face-foreground 'mode-line "white")
      (set-face-background 'mode-line "DimGray")
      (set-face-foreground 'fringe "black")
      (set-face-background 'fringe "gray")))
  ;; Fontname comes from (insert (prin1-to-string (w32-select-font)))
  ;; for the small laptop display
  (progn
;;    (set-default-font "-outline-Andale Mono-normal-r-normal-normal-13-97-96-96-c-*-iso10646-1")
;;    (set-face-font 'mode-line "-outline-Andale Mono-normal-r-normal-normal-12-90-96-96-c-*-iso10646-1")
;;    (setq-default line-spacing 1)
;;    (set-default-font "-raster-ProggyClean-normal-r-normal-normal-11-82-96-96-c-*-iso10646-1")
;;    (set-default-font "-raster-SPEEDY-normal-r-normal-normal-12-90-96-96-c-*-ms-oemlatin")
;;    (set-default-font "-raster-Sheldon Narrow-normal-r-normal-normal-12-90-96-96-c-*-iso8859-1")
    ;; (set-default-font "-outline-Terminus-medium-r-normal-normal-16-120-96-96-c-*-iso8859-1")
    ;; (set-default-font "-outline-Andale Mono-normal-r-normal-normal-19-142-96-96-c-*-iso8859-1")
    ;; (set-face-font 'mode-line "-outline-Andale Mono-normal-r-normal-normal-16-120-96-96-c-*-iso8859-1")
    (set-default-font "-outline-Consolas-normal-r-normal-normal-19-142-96-96-c-*-iso8859-1")
    )
  ;; for the large monitor
;;   (progn
;;     (set-default-font "-outline-Andale Mono-normal-r-normal-normal-11-82-96-96-c-70-iso8859-1")
;;     (set-face-font 'mode-line "-outline-Andale Mono-normal-r-normal-normal-10-90-96-96-c-*-iso10646-1")
;;     (setq-default line-spacing 1))
  ;; (setq w32-use-w32-font-dialog nil)    ; Unix-like font-dialog

  (setq archive-zip-use-pkzip nil)
  (setq w32-quote-process-args t)
  ;; needed for running M$-DOG batches under ZSH
;;   (let ((cygwin (getenv "CYGWIN")))
;;     (unless (and cygwin (string-match "\\<tty\\>" cygwin))
;;       (setenv "CYGWIN" (concat "tty " cygwin))))
  (setq process-coding-system-alist '(("bash" . undecided-unix) ("zsh" . undecided-unix)))
;;  (setq shell-file-name "zsh")
   (setq shell-file-name "bash")
 ;; (setq shell-file-name "e:/tools/git-1.6.5.1/bin/bash.exe")
  (setq explicit-bash-args '("-i"))
  (setenv "SHELL" shell-file-name)
  (setq explicit-shell-file-name shell-file-name))

;;}}}
;;{{{ General configuration

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "qx29999")
(setq inhibit-startup-echo-area-message "eclig")
(setq inhibit-startup-echo-area-message "ecl")
(setq initial-scratch-message nil)

(setq visible-bell t)

(setq use-dialog-box nil)

(setq initial-major-mode 'lisp-interaction-mode)

;; set up the mode based on the buffer name.  Thanks to `__jim__'.
;; http://www.reddit.com/r/emacs/comments/d2t4q/scratch_buffers_for_emacs/c0x7a68
(setq default-major-mode
      (lambda ()
        (let ((buffer-file-name (or buffer-file-name (buffer-name))))
          (set-auto-mode))))

(setq view-read-only nil)

;; (setq colon-double-space nil)
;; (setq sentence-end-double-space nil)
;; Setting this should not be necessary anymore
;; (setq sentence-end "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*")

;; (defadvice forward-sentence (around skip-period activate)
;;   "Position point before the period.
;; More exactly, position the point before `sentence-end'."
;;   (when (and (looking-at (sentence-end))
;;              (eq last-command this-command))
;;     (goto-char (match-end 0)))
;;   ad-do-it
;;   (goto-char (match-beginning 0)))

(setq disabled-command-function nil)    ; no disabled commands.
(put 'rmail 'disabled t)                ; avoid mbox destruction


(setq message-log-max 1024)     ; max size of the "*Messages*" buffer.

(setq eval-expression-print-length nil)

;; (setq scroll-step 1)
;; (setq scroll-conservatively 10000000)

(setq scroll-step 0)
(setq scroll-conservatively 0)

(setq scroll-preserve-screen-position t)

(setq window-min-height 2)
(setq window-min-width 10)

(setq enable-recursive-minibuffers t)
(setq history-length 512)

(setq set-mark-command-repeat-pop t)

(setq-default indent-tabs-mode nil)

(setq parens-require-spaces nil)

(setq-default fill-column 70)

;; (setq split-width-threshold nil)

(setq longlines-show-hard-newlines t)

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

;;(setq inhibit-eol-conversion t)

(setq-default ctl-arrow t)
(setq meta-flag t)

;;(setq echo-keystrokes 1)
(setq echo-keystrokes 0.000001)

(setq-default case-fold-search t)
(setq case-replace t)

(setq search-whitespace-regexp "[ \t\r\n]+")

(setq revert-without-query '("."))

(setq search-highlight t)
(setq query-replace-highlight t)

(setq next-line-add-newlines nil)
(setq require-final-newline t)

(setq default-indicate-empty-lines t)
;; (setq-default show-trailing-whitespace t)

(setq enable-local-eval 'ask)
(setq enable-local-variables t)

(setq find-file-existing-other-name t)

(setq mouse-yank-at-point t)

;; (setq kill-whole-line t)
(setq kill-read-only-ok t)

(setq windmove-wrap-around t)

(when (eq window-system 'x)
  (setq x-pointer-shape x-pointer-left-ptr)
  (when (x-display-color-p)
    (set-mouse-color "RoyalBlue")))

(setq comment-style 'indent)

(setq list-directory-brief-switches "-BCF")
(setq list-directory-verbose-switches "-Bl")

(add-to-path 'Info-default-directory-list "/usr/info")
(add-to-path 'Info-default-directory-list "/usr/share/info")

;;}}}
;;{{{ Functions

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

;; (defun zap-up-to-char (arg char)        ; adapted from `zap-to-char'
;;   "*Kill up to (but not including) ARG'th occurrence of CHAR.
;; Case is ignored if `case-fold-search' is non-nil in the current buffer.
;; Goes backward if ARG is negative; error if CHAR not found."
;;   (interactive "p\ncZap to char: ")
;;   (kill-region (point) (progn
;; 			 (search-forward (char-to-string char) nil nil arg)
;; 			 (goto-char (if (> arg 0) (1- (point)) (1+ (point))))
;; 			 (point))))

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

(defun x11-maximize-frame-vertically ()
  "*Maximize the selected frame vertically.
Works only for X11."
  (interactive)
  (when (eq window-system 'x)
    (set-frame-height (selected-frame)
                      (/ (- (x-display-pixel-height) 50) (frame-char-height)))
    (set-frame-position (selected-frame)
                        (cdr (assq 'left (frame-parameters))) 30)))

(defun w32-maximize-frame ()
  "*Maximize the selected frame.
Works only on Windows."
  (interactive)
  (unless (fboundp 'w32-send-sys-command)
    (error "This command is not available on this system"))
  (w32-send-sys-command #xf030))

;;{{{ other-window-or-other-buffer

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

;;}}}

(defun resize-window (&optional arg)    ; Hirose Yuuji and Bob Wiener
  "*Resize window interactively."
  (interactive "p")
  (if (one-window-p) (error "Cannot resize sole window"))
  (or arg (setq arg 1))
  (let (c)
    (catch 'done
      (while t
	(message
	 "h=heighten, s=shrink, w=widen, n=narrow (by %d);  0-9=unit, q=quit"
	 arg)
	(setq c (read-char))
	(condition-case ()
	    (cond
	     ((= c ?h) (enlarge-window arg))
	     ((= c ?s) (shrink-window arg))
	     ((= c ?w) (enlarge-window-horizontally arg))
	     ((= c ?n) (shrink-window-horizontally arg))
	     ((= c ?\^G) (keyboard-quit))
	     ((= c ?q) (throw 'done t))
             ((= c ?0) (setq arg 10))
	     ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
	     (t (beep)))
	  (error (beep)))))
    (message "Done.")))


;; (defadvice recenter (before recenter-at-line-10 activate)
;;   "When recentering place point on eyes level."
;;   (when (and (interactive-p) (not (ad-get-arg 0)))
;;     (ad-set-arg 0 20)))

;; The following two functions written by Noah Friedman
;; http://www.splode.com/users/friedman/software/emacs-lisp/src/buffer-fns.el
(defun toggle-mode-line-inverse-video (&optional current-only)
  (interactive)
  (cond ((fboundp 'set-face-attribute)
         (let ((onp (face-attribute 'modeline :inverse-video))
               (dt (cdr (assq 'display-type (frame-parameters)))))
           (when (equal onp 'unspecified)
             (setq onp nil))
           (set-face-attribute 'modeline nil :inverse-video (not onp))
           ;; This should be toggled on mono frames; in color frames, this
           ;; must always be t to use the face attribute.
           (setq mode-line-inverse-video (or (eq dt 'color) (not onp)))
           (force-mode-line-update (not current-only))))
        (t
         (setq mode-line-inverse-video (not mode-line-inverse-video))
         (force-mode-line-update (not current-only)))))

(defun bell-flash-mode-line ()
  "*Effect ringing bell by flashing mode line momentarily.
In emacs 20.1 or later, you can use the variable `ring-bell-function'
to declare a function to run in order to ring the emacs bell."
  (let ((localp (local-variable-p 'mode-line-inverse-video)))
    (or localp
        (make-local-variable 'mode-line-inverse-video))
    (toggle-mode-line-inverse-video t)
    (sit-for 0 100)
    ;; Set it back because it may be a permanently local variable.
    (toggle-mode-line-inverse-video t)
    (or localp
        (kill-local-variable 'mode-line-inverse-video))))
(setq ring-bell-function 'bell-flash-mode-line)


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

;; Emacs 21.1 has `delete-trailing-whitespace'
;; (defun remove-trailing-blanks (beg end)
;;   "*Remove the trailing blanks from all lines in region."
;;   (interactive "*r")
;;   (save-excursion
;;     (goto-char beg)
;;     (while (re-search-forward "[ \t\r]+$" end t)
;;       (replace-match "" nil nil))))

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
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)

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

(defun comment-sexp (arg)                  ; adapted from `kill-sexp'
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

;; (defun insert-date (&optional arg)
;;   "*Insert date in current buffer.
;; With optional prefix argument also insert time."
;;   (interactive "*P")
;;   (insert (format-time-string
;;            (concat "%-d %b %Y" (when arg ", %H:%M (%Z)")) 
;;            (current-time))))

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

;; (defun zsh-forward-word (arg)
;;   ;; TODO: if ARG 0 move to end of current word
;;   "Move point forward to the beginning of the ARG-th word."
;;   (interactive "p")
;;   (forward-word (if (looking-at "\\W") arg (1+ arg)))
;;   (forward-word -1))

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

(defun leo (word)
  (require 'thingatpt)
  (interactive (list 
                (let ((word (thing-at-point 'word)))
                  (if word
                      (read-string (format "Word [default \"%s\"]: " word) nil nil word)
                    (read-string "Word: ")))))
  
  (browse-url (format "http://dict.leo.org/?search=%s" (string-make-unibyte word))))

(defun wp (word)
  (require 'thingatpt)
  (interactive (list 
                (let ((word (thing-at-point 'word)))
                  (if word
                      (read-string (format "Word [default \"%s\"]: " word) nil nil word)
                    (read-string "Word: ")))))
  
  (browse-url (format "http://de.wikipedia.org/wiki/Special:Search?search=%s" word)))

(defvar spelling-alphabet
  "Buchstabe  Deutschland      ITU/ICAO/NATO
A          Anton            Alfa
Ä          Ärger            –
B          Berta            Bravo
C          Cäsar            Charlie
Ch         Charlotte        –
D          Dora             Delta
E          Emil             Echo
F          Friedrich        Foxtrot
G          Gustav           Golf
H          Heinrich         Hotel
I          Ida              India
J          Julius           Juliett
K          Kaufmann         Kilo
L          Ludwig           Lima
M          Martha           Mike
N          Nordpol          November
O          Otto             Oscar
Ö          Ökonom           –
P          Paula            Papa
Q          Quelle           Quebec
R          Richard          Romeo
S          Siegfried        Sierra
Sch        Schule           –
ß          Eszett           –
T          Theodor          Tango
U          Ulrich           Uniform
Ü          Übermut          –
V          Viktor           Victor
W          Wilhelm          Whiskey
X          Xanthippe        X-Ray
Y          Ypsilon          Yankee
Z          Zeppelin         Zulu
")
;;}}}
;;{{{ Packages

;; sooner or later it will be loaded, so do it now.
(require 'tramp)
(setq tramp-default-method "sshx")

;; /sudo:eas254.muc:/etc/fstab
(add-to-list 'tramp-default-proxies-alist
             '("eas254\\.muc\\'" "\\`root\\'" "/sshx:eas254@%h:"))

;; jka-compr provides transparent access to compressed files.
(require 'jka-compr)
(auto-compression-mode 1)

;; Automatic resizing of help windows.
(temp-buffer-resize-mode +1)

;; enable visiting image files as images.
(if (fboundp 'auto-image-file-mode)
    (auto-image-file-mode 1))

;; (autoload 'bat-mode "bat-mode"
;;   "Major-mode for editing DOS and Windows BAT files." t)
;; (eval-after-load "bat-mode"
;;   '(modify-syntax-entry ?% "$"  bat-mode-syntax-table))
(autoload 'ntcmd-mode "ntcmd"
  "Major mode for editing CMD scripts." t)

;; find-library-file
;; (autoload 'find-library-file "find-library-file"
;;   "Visit file LIBRARY in `load-path'." t)

;; imenu
(setq imenu-always-use-completion-buffer-p 'never)
;; (global-defkey "<apps>" 'imenu)


;; Kyle Jones' redo
;; (when (require-soft 'redo)
;;   (global-defkey "M-_" 'redo))
                                        ; `undo' is on "C-_"

(when (require-soft 'goto-last-change)
  (global-set-key "\C-x\C-\\" 'goto-last-change))

(when running-nt
  (global-defkey "<apps>" 'undo)
  ;; (global-defkey "S-<apps>" 'redo)
  )

;; Hippie-expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-all-abbrevs
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-list
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev-from-kill))

(global-defkey "M-/" 'hippie-expand)
(global-defkey "C-M-/" 'dabbrev-completion)

;; I like to have truncated lines and use `hscroll' to automatically
;; scroll horizontally:
;; (setq-default truncate-lines t)
;; (setq truncate-partial-width-windows t)
;; (setq hscroll-mode-name nil)
;; (setq hscroll-margin 1)
;; (hscroll-global-mode 1)

(setq-default truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; (when window-system
;;   (let ((truncation-glyph (+ ?$ (lsh (face-id 'modeline) 19)))
;;         (wrap-glyph       (+ ?\\ (lsh (face-id 'modeline) 19))) )
;;     (set-display-table-slot standard-display-table 'truncation truncation-glyph)
;;     (set-display-table-slot standard-display-table 'wrap       wrap-glyph)))


;; chop: binary search for a line within a window
(autoload 'chop-move-up "chop")
(autoload 'chop-move-down "chop")
(eval-after-load "chop"
  '(setq chop-lines-near-middle nil))

;; (global-defkey "C-S-p" 'chop-move-up)
;; (global-defkey "C-S-n" 'chop-move-down)

;; (global-defkey "M-[" 'chop-move-up)
;; (global-defkey "M-]" 'chop-move-down)

(global-defkey "S-<up>" 'chop-move-up)
(global-defkey "S-<down>" 'chop-move-down)

(global-defkey "C-," 'chop-move-up)
(global-defkey "C-." 'chop-move-down)

;; Insert paired delimiters
;; (when (require-soft 'insert-delims)
;;   (mapcar
;;    (lambda (char)
;;      (global-set-key (read-kbd-macro (concat "H-" (char-to-string char))) 'insert-delimiter-pair))
;;    "\"()[]{}<>`'"))

;; (when (require-soft 'skeleton)
;;   (mapcar
;;    (lambda (char)
;;      (global-set-key (read-kbd-macro (char-to-string char)) 'skeleton-pair-insert-maybe))
;;    "\"([{<`'")

;;   (defun toggle-skeleton-pair (&optional arg)
;;     "*Toggle insertion of matching pairs (parentheses, brackets, etc.)"
;;     (interactive "P")
;;     (toggle-variable 'skeleton-pair arg))

;;   ;; (toggle-skeleton-pair +1)
;;   )

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

;; Find file at point
(require 'ffap)
(setq ffap-require-prefix t)
(setq ffap-highlight nil)
(when running-nt
  (setq ffap-url-regexp nil))

(defun ffap-read-only ()
  "Like \\[find-file] but marks buffer as read-only.
Only intended for interactive use."
  (interactive)
  (let ((ffap-file-finder 'find-file-read-only))
    (call-interactively 'ffap)))

(defun ffap-read-only-other-window ()
  "Like \\[ffap-other-window] but marks buffer as read-only.
Only intended for interactive use."
  (interactive)
  (let ((ffap-file-finder 'find-file-read-only-other-window))
    (call-interactively 'ffap)))

(defun ffap-read-only-other-window-noselect ()
  "Like \\[ffap-read-only-other-window] but don't select buffer.
Only intended for interactive use."
  (interactive)
  (let ((ffap-file-finder 'find-file-read-only-other-window))
    (save-selected-window
      (call-interactively 'ffap))))

;;(ffap-bindings)
(global-defkey "C-x C-f"   'find-file-at-point)
(global-defkey "C-x 4 f"   'ffap-other-window)
(global-defkey "C-x 4 C-f" 'ffap-other-window)
;;(global-defkey "C-x d"     'dired-at-point)
(global-defkey "C-x C-r"   'ffap-read-only)
(global-defkey "C-x 4 r"   'ffap-read-only-other-window)
(global-defkey "C-x 4 R"   'ffap-read-only-other-window-noselect)


;; Gnuplot
(autoload 'gnuplot "gnuplot"
  "Run Gnuplot interactively in a Emacs buffer." t nil)
(autoload 'gnuplot-interaction-mode "gnuplot-interaction"
  "Major mode for editing Gnuplot input files." t nil)

;; Shell-script
(autoload 'sh-mode "sh-script" 
  "Major mode for editing shell scripts" t nil)
(eval-after-load "sh-script"
  '(set-face-foreground 'sh-heredoc-face (face-foreground 'font-lock-constant-face)))

;; Message
(eval-after-load "message" '(load (expand-file-name "~/.message")))


;; the-the is a nice thing for text processing:
(autoload 'the-the "the-the"
  "Search forward for for a duplicated word." t nil)

;; apropos
(defun apropos-function ()
  "*Show functions that match REGEXP."
  (interactive)
  (require 'apropos)
  (let ((apropos-do-all t))
    (call-interactively 'apropos-command)))


;; iswitchb
(if (fboundp 'iswitchb-mode)
    (iswitchb-mode)
  (iswitchb-default-keybindings))
(setq read-buffer-function 'iswitchb-read-buffer)
(setq iswitchb-case t)
(setq iswitchb-regexp nil)
(setq iswitchb-prompt-newbuffer nil)
(setq iswitchb-default-method 'samewindow)
(setq iswitchb-all-frames 'no)

(defun major-mode-matches (buffer regexp)
  "*Return t if `mode-name' in  BUFFER matches REGEXP.
To be used mainly as a filter in iswitchb to select only buffers
whose major-mode matches REGEXP."
  (with-current-buffer buffer
    (string-match regexp (format-mode-line mode-name))))

(defun iswitchb-only-dired-buffers (buffer)
  "*Ignore all buffers not in dired-mode."
  (not (major-mode-matches buffer "\\`Dired\\>")))

(defun iswitchb-dired-buffers ()
  "*Switch to a Dired buffer."
  (interactive)
  (let ((iswitchb-buffer-ignore '(iswitchb-only-dired-buffers)))
    (call-interactively 'iswitchb-buffer)))

(global-defkey "C-x D" 'iswitchb-dired-buffers)

(defun iswitchb-only-shell-buffers (buffer)
  "*Ignore all buffers not in shell-mode."
  (not (major-mode-matches buffer "\\`Shell\\'")))

(defun iswitchb-shell-buffers ()
  "*Switch to a Shell buffer."
  (interactive)
  (let ((iswitchb-buffer-ignore '(iswitchb-only-shell-buffers)))
    (call-interactively 'iswitchb-buffer)))

(global-defkey "C-x S" 'iswitchb-shell-buffers)


;; (require-soft 'iswitchb-filters)

;; Kin Cho
(defun iswitchb-exclude-nonmatching ()
  "*Exclude non matching buffer names."
  (interactive)
  (setq iswitchb-buflist iswitchb-matches)
  (setq iswitchb-rescan t)
  (delete-minibuffer-contents))

(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-my-keys)
(defun iswitchb-my-keys ()
 "*Add custom keybindings for iswitchb."
 (defkey iswitchb-mode-map "C-o" 'iswitchb-exclude-nonmatching)
 (defkey iswitchb-mode-map "C-M-l" 'iswitchb-rescan)
;; Cause problem in conjunction with minibuffer-complete-cycle!
;; (defkey iswitchb-mode-map "ESC" 'keyboard-escape-quit)
 (defkey iswitchb-mode-map "<f4>" 'iswitchb-next-match)
 (defkey iswitchb-mode-map "<S-f4>" 'iswitchb-prev-match)
 (defkey iswitchb-mode-map "<kp-add>" 'iswitchb-next-match)
 (defkey iswitchb-mode-map "<kp-subtract>" 'iswitchb-prev-match)
 (defkey iswitchb-mode-map "C-a" 'iswitchb-toggle-ignore)
 (defkey iswitchb-mode-map "C-z C-f" 'iswitchb-find-file))

(global-defkey "<kp-subtract>" 'bury-buffer)
(global-defkey "<kp-add>"      'iswitchb-buffer)

(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
set to non-nil."
  (setq iswitchb-buflist iswitchb-matches)
  (iswitchb-rescan))


;; minibuffer-complete-cycle
;; (when (require-soft 'minibuffer-complete-cycle)
;;   (setq minibuffer-complete-cycle t))

(require-soft 'minibuf-isearch)


;;; autoinsert
(setq auto-insert-directory (concat user-emacs-directory "auto-insert/"))
(auto-insert-mode 1)
(add-to-list 'auto-insert-alist '(("/\\.?lib/zsh/" . "ZSH function")
                                  "Short description: "
                                  '(shell-script-mode)
                                  "#!/usr/bin/env zsh
## Time-stamp: <>
## Emilio Lopes <eclig@gmx.net>

## " (file-name-nondirectory (buffer-file-name)) " --- " str "

## THIS FILE IS IN THE PUBLIC DOMAIN.  USE AT YOUR OWN RISK!

# " (file-name-nondirectory (buffer-file-name)) " () {

emulate -LR zsh

" _ "

# }\n"))
(add-to-list 'auto-insert-alist '(perl-mode . "header.pl"))
(add-to-list 'auto-insert-alist '("\\.pl\\'" . "header.pl"))


;;; escreen
;; (setq escreen-install-number-mode-format t)
;; (setq escreen-number-mode t)
;; (setq escreen-prefix-char "\C-q")
;; (when (require-soft 'escreen)
;;   ;; (require-soft 'escreen-addons)
;;   (escreen-install)
;;   (defkey escreen-map "q" 'quoted-insert)
;;   (defkey escreen-map "SPC" 'escreen-goto-next-screen))


;;; winring
;; (when (require-soft 'winring)
;;   (setq winring-show-names t)
;;   (setq winring-prompt-on-create nil)
;;   (setq winring-keymap-prefix (kbd "C-q"))
;;   (defkey winring-map "<SPC>" 'winring-next-configuration)
;;   (defkey winring-map "q" 'quoted-insert)
;;   (defkey winring-map "c" 'winring-new-configuration)
;;   (winring-initialize))


;;; winner
(when (require-soft 'winner)
  (winner-mode +1))


;; Overload completion commands through the ones from `complete':
;; (require 'complete)
;; (fset 'lisp-complete-symbol 'PC-lisp-complete-symbol)
;; (fset 'minibuffer-complete 'PC-complete)
;; (fset 'minibuffer-complete-word 'PC-complete-word)
;; (fset 'minibuffer-complete-and-exit 'PC-complete-and-exit)
;; (fset 'minibuffer-completion-help 'PC-completion-help)

(defadvice PC-lisp-complete-symbol (before forward-sexp-before-completion (&optional arg) activate)
  "Do a `forward-sexp' if necessary before trying completion.
With prefix argument ARG behave as usual."
  (interactive "P")
  (unless arg
    (when (looking-at "\\sw\\|\\s_")
      (forward-sexp))))

(partial-completion-mode 1)

;; Resizing the minibuffer:
;; (require 'rsz-mini)
;; (resize-minibuffer-mode +1)
;; (setq resize-minibuffer-window-exactly t)
;; (setq resize-minibuffer-frame t)
;; (setq resize-minibuffer-frame-exactly t)
;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
;; (defun my-minibuffer-setup-hook ()
;;   "*Setup the minibuffer."
;;   (setq truncate-lines nil))
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


;; Get the little rodent out of way
(when (and (display-mouse-p)
           (require-soft 'avoid))
  ;; (mouse-avoidance-mode 'banish)
  (defun toggle-mouse-avoidance-mode ()
    (interactive)
    (mouse-avoidance-mode)))


;; Filladapt
(when (require-soft 'filladapt)
  (setq filladapt-fill-column-tolerance 6)
  (setq filladapt-mode-line-string nil)
;;  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'text-mode-hook 'turn-on-filladapt-mode)
  )

;;; Uniquify
(require 'uniquify)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re
      "\\(news\\|mail\\|reply\\|followup\\) message\\*")
;; (add-to-list 'uniquify-list-buffers-directory-modes 'shell-mode)

;;; Time-stamp 
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-active t)
(setq time-stamp-warn-inactive t)
(unless (string-match "^ecl\\(ig\\)?" (user-login-name))
  ;; use full name instead of login name in time-stamps
  (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %U"))


;;; Turn on font-lock mode just for color displays
(when (display-color-p)
  (require-soft 'font-latex)
  (setq font-lock-support-mode 'jit-lock-mode)
  (setq font-lock-maximum-decoration t)
  (global-font-lock-mode 1)
  (set-face-foreground 'font-lock-comment-face "red")
  ;; (set-face-foreground 'font-lock-string-face "firebrick")
  (set-face-foreground 'font-lock-string-face "indianred")
  (set-face-foreground 'font-lock-type-face "darkgreen")
  ;; (set-face-foreground 'font-lock-variable-name-face "cyan4")
  (set-face-foreground 'font-lock-variable-name-face "DodgerBlue")
  (set-face-foreground 'font-lock-constant-face "blue2")
;;  (set-face-foreground 'font-lock-variable-name-face "orangered")
;;  (set-face-foreground 'font-lock-variable-name-face "cyan3")
  (set-face-foreground 'font-lock-variable-name-face "#008b8b")
  (add-hook 'font-lock-mode-hook
            (lambda ()
              (font-lock-add-keywords nil '(("\\*\\(ECL\\|FIXME\\)\\*:?" 0 'show-paren-mismatch-face t)))))
  ;; (font-lock-add-keywords 'fortran-mode '(("\\<ECL:?" 0 'show-paren-mismatch-face t)))
  )

;;}}}

;;{{{ Keybindings

;; extra "C-x" for Dvorak keyboard layouts, in the same hand as "s",
;; "f", "w", "v".
(global-defkey "C-z" ctl-x-map)
(or key-translation-map (setq key-translation-map (make-sparse-keymap)))
(define-key key-translation-map "\C-z8" 'iso-transl-ctl-x-8-map)

;; put `previous-line' in the same hand as `next-line' in Dvorak layout
(global-defkey "C-h"     'previous-line)
(global-defkey "C-x C-h" help-map)

(global-defkey "C-M-<backspace>" 'backward-kill-sexp)

;; (global-defkey "C-M-<SPC>" 'copy-sexp)
(global-defkey "M-k" 'copy-line)
(global-defkey "M-K" 'kill-sentence)
(global-defkey "M-+" 'delete-horizontal-space-forward)

(defkey esc-map ")" 'up-list)

;; easy cursor movement with a Dvorak layout
(global-defkey "M-H" 'backward-char)
(global-defkey "M-N" 'forward-char)
(global-defkey "M-C" 'previous-line)
(global-defkey "M-T" 'next-line)


(global-defkey "C-c 0" (recursive-edit-preserving-window-config (delete-window)))
(global-defkey "C-c 1" (recursive-edit-preserving-window-config
                        (if (one-window-p 'ignore-minibuffer)
                            (error "Current window is the only window in its frame")
                          (delete-other-windows))))
(global-defkey "C-c 2" (recursive-edit-preserving-window-config (split-window-vertically)))
(global-defkey "C-c 3" (recursive-edit-preserving-window-config (split-window-horizontally)))
(global-defkey "C-c 4 b" (recursive-edit-preserving-window-config (iswitchb-buffer-other-window)))
(global-defkey "C-c 4 C-o" (recursive-edit-preserving-window-config (iswitchb-display-buffer)))

;; (global-defkey "C-c 1" 'recursive-edit-with-single-window)
;; (global-defkey "C-c 0"
;;                (lambda () (interactive)
;;                  (recursive-edit-with-single-window 'delete-window)))

(global-defkey "C-c $" 'toggle-truncate-lines)
(global-defkey "C-c \\" 'the-the)
(global-defkey "C-c ;" 'comment-or-uncomment-region)
(global-defkey "C-c ~" 'diff-backup-this-file)

(global-defkey "C-c a" 'show-time-and-date)
;; (global-defkey "C-c b" 'browse-url)

(global-defkey "C-c b" (lambda () (interactive) (mouse-avoidance-banish-mouse)))

(bind-with-new-map (current-global-map) "C-c d"
  ("b" . 'ediff-buffers)
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
  ("$" . (lambda ()
           (interactive)
           (setq show-trailing-whitespace (not show-trailing-whitespace))
           (redraw-frame (selected-frame)))))

(global-defkey "C-c j" (make-sparse-keymap))
(global-defkey "C-c j h"        (lambda () (interactive) (dired     "~")))
(global-defkey "C-c j D"        (lambda () (interactive) (dired     "~/Downloads")))
(global-defkey "C-c j d"        (lambda () (interactive) (find-file "~/.lib/emacs/rc/dired_rc.el")))
(global-defkey "C-c j e"        (lambda () (interactive) (find-file user-init-file)))
(global-defkey "C-c j m"        (lambda () (interactive) (find-file "~/.message")))
(global-defkey "C-c j g"        (lambda () (interactive) (find-file "~/.gnus")))
(global-defkey "C-c j s"        (lambda () (interactive) (find-file "~/.lib/emacs/rc/shell_rc.el")))
(global-defkey "C-c j t"        (lambda () (interactive) (find-file "u:/ORG/TODO")))

(global-defkey "C-c j ."        (lambda () (interactive) (find-file "~/.ee.sh")))

(global-defkey "C-c j z"        (lambda () (interactive) (find-file "~/.zshrc")))
(global-defkey "C-c j a"        (lambda () (interactive) (find-file "~/.zaliases")))
(global-defkey "C-c j l"        (lambda () (interactive) (find-file "~/.zprofile")))
(global-defkey "C-c j v"        (lambda () (interactive) (find-file "~/.zshenv")))

(global-defkey "C-c g" 'goto-line)
(global-defkey "M-g" 'goto-line)

(global-defkey "M-i" 'other-window)

(global-defkey "C-c t" 'insert-date)

;; (global-defkey "C-c m" 'man)
;; (global-defkey "C-c r" 'revert-buffer)
(global-defkey "C-c s" 'jump-to-scratch-buffer)
(global-defkey "C-c z" 'jump-to-text-scratch-buffer)

(global-defkey "C-c u" 'rename-uniquely)
;; (global-defkey "C-c v" 'show-buffer-file-name)
(global-defkey "C-c w" 'copy-filename-as-kill)
;; (global-defkey "C-c w" 'locate)

(bind-with-new-map help-map "a"
  ("a" . 'apropos)
  ("c" . 'apropos-command)
  ("f" . 'apropos-function)
  ("v" . 'apropos-variable)
  ("d" . 'apropos-documentation)
  ("l" . 'apropos-value))

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

;; (global-defkey "C-<prior>"      'backward-page)
;; (global-defkey "C-<next>"       'forward-page)

;; (global-defkey "C-<home>"       'beginning-of-buffer)
;; (global-defkey "C-<end>"        'end-of-buffer)

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
;; (global-defkey "S-<f2>"         'save-some-buffers)
;; (global-defkey "S-<f2>"         'revert-buffer)
(global-defkey "S-<f2>"         'revert-buffer-preserve-modes)

(global-defkey "<f3>"           'dired-jump)
(global-defkey "S-<f3>"         'shell-hier)

;; (global-defkey "<f4>"           'next-buffer)
(global-defkey "<f4>"           'iswitchb-buffer)
(global-defkey "S-<f4>"         'bury-buffer)

;; (global-defkey "<f5>"           'other-window)
(global-defkey "<f5>"           'resize-window)
(global-defkey "S-<f5>"         'swap-window-positions)

;; (global-defkey "<f6>"           'call-last-kbd-macro)
;; (global-defkey "S-<f6>"         'edit-kbd-macro)

(global-defkey "<f7>"           'insert-default-register)
(global-defkey "S-<f7>"         'copy-to-default-register)

(global-defkey "<f8>"           'grep)
;; (global-defkey "S-<f8>"         'occur)
(global-defkey "S-<f8>"         'grep-find)

(global-defkey "<f9>"           'next-error)
(global-defkey "S-<f9>"         'compile)

;; (global-defkey "<f10>"          'repeat)

(global-defkey "<f10>"           'call-last-kbd-macro)
(global-defkey "S-<f10>"         'apply-macro-to-region-lines)

;; (global-defkey "<f12>"           'toggle-window-dedicated)

;; Make <f12> act like Hyper, for keyboards without it. Just like
;; <f11> acts as Meta on older DEC terminals.
;; Must undef it first for the function-key-map binding to work
;; (global-unset-key [f12])
;; (define-key function-key-map [f12] 'event-apply-hyper-modifier)


'(global-defkey "<f12>" (lambda ()
                         (interactive)
                         (save-window-excursion
                           (save-excursion
                             (message "Recursive edit in progress")
                             (recursive-edit)))))

(global-defkey "<f12>" 'imenu)

(global-defkey "<key-20>" 'toggle-window-dedicated) ; that's `scroll-lock'

(global-defkey "<find>"         'isearch-forward)
(global-defkey "<execute>"      'execute-extended-command)

;; (global-defkey "C-r"            'isearch-backward-regexp)
;; (global-defkey "C-M-r"          'isearch-backward)
;; (global-defkey "C-s"            'isearch-forward-regexp)
;; (global-defkey "C-M-s"          'isearch-forward)

(global-defkey "<print>"        'ps-spool-buffer-with-faces)
(global-defkey "S-<print>"      'set-default-printer)

;;}}}


;;; Frame parameters
(add-to-list 'initial-frame-alist '(cursor-type . box))
(add-to-list 'default-frame-alist '(cursor-type . box))

;;; Time (and date) display setup.
(display-time-mode 1)
(setq display-time-interval 5)
(setq display-time-day-and-date nil)
(setq display-time-24hr-format t)
(setq display-time-use-mail-icon t)
(and running-nt (set-time-zone-rule "CET-1CDT"))

;;; Mode-line and Frame-title format:

(setq line-number-display-limit-width 512)

(setq-default frame-title-format (list "" "Emacs Macht Alle Computer Sch\366n"))

(setq-default icon-title-format frame-title-format)

;;; Common modes stuff
;; Add some suffix defs to auto-mode-alist:
(setq auto-mode-alist (append '(
                                ("\\.\\(pl\\|pm\\)\\'" . cperl-mode)
                                ("\\.\\([bB][aA][tT]\\|[cC][mM][dD]\\)\\'" . ntcmd-mode)
                                ;; ("CONFIG\\." . bat-mode)
                                ;; ("AUTOEXEC\\." . bat-mode)
                                ("[^/]\\.dired\\'" . dired-virtual-mode)
                                ("\\.fi\\'" . fortran-mode)
                                ("\\.bash_\\(functions\\|aliases\\)\\'" . sh-mode)
                                ("\\.\\(SCORE\\|ADAPT\\)\\'" . gnus-score-mode)
                                ("\\.gpt?\\'" . gnuplot-interaction-mode)
                                ("\\.mak?\\'" . makefile-mode)
                                ("\\.col?\\'" . c-mode)
                                ("\\.hol?\\'" . c-mode)
                                ("\\.kgs?\\'" . c-mode)
                                ("\\.dtx\\'" . latex-mode))
                              auto-mode-alist))


(when at-bmw
  (setq auto-mode-alist (append '(("\\.dat?\\'" . c-mode)) auto-mode-alist)))

;;; kill-ring
(setq kill-ring-max 1024)

;; Thanks to Karl Fogel:
;; http://svn.red-bean.com/repos/kfogel/trunk/.emacs
(defun kf-browse-kill-ring ()
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
  (goto-char (point-min)))

(global-defkey "<f11>" 'kf-browse-kill-ring)
;; From http://lists.gnu.org/archive/html/emacs-devel/2008-03/msg00128.html
;; See also http://svn.red-bean.com/repos/kfogel/trunk/code/yank-match/
(defun insert-yank-from-kill-ring (string)
  "Insert the selected item from the kill-ring in the minibuffer history.
Use minibuffer navigation and search commands to browse the kill-ring
in the minibuffer history."
  (interactive (list (read-string "Yank from kill-ring: " nil 'kill-ring)))
  (insert-for-yank string))

(defadvice kill-new (around browse-kill-ring activate)
  "STRING is only inserted in the kill-ring if not already there."
  (setq kill-ring (delete (ad-get-arg 0) kill-ring))
  ad-do-it)


;;; Ispell
(setq-default ispell-local-dictionary "deutsch8")

(global-defkey "C-c i w" 'ispell-word)
(global-defkey "C-c i m" 'ispell-message)
(global-defkey "C-c i b" 'ispell-buffer)
(global-defkey "C-c i r" 'ispell-region)
(global-defkey "C-c i c" 'ispell-change-dictionary)
(global-defkey "C-c i k" 'ispell-kill-ispell)

(global-defkey "C-c i d"
  (lambda () "*Set German dictionary (Ispell)."
    (interactive) 
    (ispell-change-dictionary "deutsch8")))
(global-defkey "C-c i e"
  (lambda () "*Set English dictionary (Ispell)."
    (interactive)
    (ispell-change-dictionary "english")))
(global-defkey "C-c i p"
  (lambda () "*Set Portuguese dictionary (Ispell)."
    (interactive)
    (ispell-change-dictionary "portugues")))

(defun show-current-ispell-dictionary ()
  "*Display the value of ispell-dictionary in the echo area."
  (interactive)
  (if ispell-dictionary
      (message "Current dictionary: %s" ispell-dictionary)
    (message "Variable `ispell-dictionary' is not set.")))

(global-defkey "C-c i w" 'show-current-ispell-dictionary)



;;; PSGML
;; (setq sgml-auto-activate-dtd t)
;; (setq-default sgml-set-face t)
;; (setq sgml-auto-activate-dtd t)
;; (autoload 'xxml-mode-routine "xxml")
;; (add-hook 'sgml-mode-hook 'xxml-mode-routine)
;; ;; XXML binds `M-_' to something else. Restore the original binding here.
;; (add-hook 'sgml-mode-hook
;;           (lambda ()
;;             (when (fboundp 'redo) (local-defkey "M-_" 'redo))
;;             (local-defkey "<S-f9>" 'sgml-validate-it)
;;             (local-defkey "C-c M-k" 'sgml-kill-element-contents)
;;             (local-defkey "C-x n d" 'sgml-narrow-to-current-element)) 'append)

;; ;; Dominique Quatravaux [http://www.geocrawler.com/archives/3/7086/2002/5/0/8656969/]
;; (defadvice sgml-parse-external (before sgml-xml-pubid-ok activate)
;;   "Allow parsing of doctypes without a system ID, as XML requires."
;;   (ad-set-arg 0 t))

;; (defun sgml-validate-it (&optional arg)
;;   "Validate the current SGML document.
;; By default use `sgml-validate-command' to validate the document. For
;; XML documents use `sgml-xml-validate-command' instead.
;; If an optional prefix argument is given, ask for the command to be
;; used to validate the document."
;;   (interactive "P")
;;   (if arg
;;       (call-interactively 'sgml-validate)
;;     (sgml-validate (sgml-default-validate-command))))

;; (defun sgml-narrow-to-current-element ()
;;   "*Narrow to the current element, making text outside it invisible."
;;   (interactive)
;;   (narrow-to-region
;;    (save-excursion
;;      ;; use `skip-chars-backward' so that indentation is preserved
;;      (sgml-backward-up-element) (skip-chars-backward " \t") (point))
;;    (save-excursion
;;      (sgml-up-element) (point))))

;; (defun sgml-kill-element-contents ()
;;   "*Kill the contents of the current element.
;; Point is left at the beginning of the element."
;;   (interactive)
;;   (kill-region
;;    (save-excursion (sgml-end-of-element) (point))
;;    (progn (sgml-beginning-of-element) (point))))


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

;; (defun dsg-complete-alias ()
;;   (interactive)
;;   (let* ((b-o-w (save-excursion
;;                   (forward-word -1)
;;                   (point)))
;;          (word (buffer-substring b-o-w (point)))
;; ;;         (maybes (all-completions word mail-aliases)))
;;          (maybes (all-completions word mail-abbrevs)))
;;     (cond ((null maybes)
;;            (error "no aliases match %s" word))
;;           ((= (length maybes) 1)
;;            (delete-region b-o-w (point))
;;            (insert (car maybes)))
;;           (t
;;            (with-output-to-temp-buffer "*Completions*"
;;              (display-completion-list maybes))))
;;     (expand-abbrev)))


;;; SES
(eval-after-load "ses" '(require-soft 'ses-formulas))



;;; text
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook (lambda ()
;;                             (flyspell-mode +1)))
;; (add-hook 'text-mode-hook (lambda ()
;;                             (longlines-mode +1)))

(add-hook 'text-mode-hook 'fix-broken-outlook-replies)

(defun fix-broken-outlook-replies ()
  (let ((bname (buffer-file-name)))
    (when (and (stringp bname)
               (string-match-p "/bmwmail\\." bname))
      (goto-char (point-max))
      ;; (delete-blank-lines)
      (goto-char (point-min))
      ;; (delete-blank-lines)

      (let ((sig-start (and (search-forward-regexp "^-- *$" nil t)
                            (point-at-bol)))
            (citation-start (and (search-forward-regexp "^_______________+$" nil t)
                                 (progn (delete-region (point-at-bol) (min (1+ (point-at-eol)) (point-max)))
                                        (point-at-bol)))))

        (when citation-start
          (replace-regexp "^" "> " nil citation-start (1- (point-max))))

        ;; (goto-char (point-max))

        (save-excursion
          (when sig-start
            (let ((sig (delete-and-extract-region sig-start (or citation-start (point-max)))))
              (goto-char (point-max))
              (insert "\n" sig))))))))

;;; Fortran
(defun fortran-uncomment-empty-lines (beg end)
  "*Remove comment characters from empty lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^[*Cc!]+[ \t]*$" end t)
      (replace-match ""))))

(defun fortran-insert-print (vars)
  (interactive (list (read-from-minibuffer "Print: ")))
  (let ((vars (split-string vars "[ \t,]+"))
        varstring)
    (while vars
      (setq varstring (concat varstring (and varstring ", ") (car vars)))
      (setq vars (cdr vars)))
    (insert (format "print *, '%s: ', %s" varstring varstring))))

(defun my-fortran-mode-hook ()
  "*Setup fortran-mode."
  (setq fortran-startup-message nil
        fill-column 72
        fortran-continuation-string "&"
        fortran-comment-region "*"
        comment-line-start "*"
        comment-start "! "
        fortran-comment-indent-style nil
        fortran-blink-matching-if t
        fortran-tab-mode-default nil
        fortran-line-number-indent 5)
  (defkey fortran-mode-map "M-q" 'fortran-fill)
  (defkey fortran-mode-map "C-c p" 'fortran-insert-print)
  (abbrev-mode 1)
  (set (make-local-variable 'grep-command) "grep -ni ")
  (set (make-local-variable 'igrep-options) "-i")
  (set-compile-command "g77 -c %s"))
(add-hook 'fortran-mode-hook 'my-fortran-mode-hook)



;;; Stick REPL
;; (autoload 'sticky-repl-display "sticky-repl" nil t)
;; (setq special-display-function 'sticky-repl-display)
;; (setq compilation-window-height 12)
;; (setq same-window-buffer-names nil)
;; (setq special-display-buffer-names
;;       '("*Apropos*"
;;         "*Backtrace*"
;;         "*Calculator*"
;;         "*Compile-log*"
;;         "*Help*"
;;         "*Messages*"
;;         "*Occur*"
;;         "*Shell Command Output*"
;;         "*compilation*"
;;         "*grep*"
;;         "*ielm*"
;;         "*inferior-lisp*"
;;         "*scheme*"
;;         "*vc*"
;;         "*vc-diff*"))
;; (setq special-display-regexps
;;       '("\\*shell\\(<[0-9]+>\\)?\\*"
;;         "\\*slime-repl .*\\*"
;;         "\\*sldb .*\\*"))


;;; Scheme/Lisp modes

(require 'ecl_lisp)
;; (defun scheme-scratch (&optional arg)
;;   "*Switch to buffer `*Scheme scratch*', creating it if necessary.
;; The buffer is put in Scheme mode.
;; With prefix arg clear the buffers content."
;;   (interactive "P")
;;   (switch-to-buffer-create "*Scheme scratch*" 'scheme-mode arg))

;; (defun comment-sexp (&optional arg)
;;   "Comment out the following sexp.
;; With argument ARG comment that many sexps out."
;;   (interactive "p")
;;   (comment-region (point) (save-excursion (forward-sexp arg) (point))))

;; (defun lisp-comment-dwim (&optional arg)
;;   "Comment or uncomment the next \"chunk\" of code.
;; If looking at a comment start, uncomment it.  Else comment the
;; next ARG sexps."
;;   (interactive "p")
;;   (let ((looking-at-comment (looking-at (concat "\\s-*" comment-start-skip))))
;;     (if looking-at-comment
;;         (uncomment-region (point) (save-excursion (forward-comment (point-max)) (point)))
;;       (comment-sexp arg))))

;; (setq same-window-buffer-names (delete "*scheme*" same-window-buffer-names))
;; (mapc (lambda (mode)
;;         (font-lock-add-keywords mode '(("[()]" 0 '(((:foreground "gray50")))))))
;;       '(emacs-lisp-mode lisp-mode lisp-interaction-mode inferior-lisp-mode scheme-mode inferior-scheme-mode))

(defun jump-to-scratch-buffer (&optional arg)
  "*Switch to buffer `*scratch*', creating it if necessary.
With prefix arg clear the buffers content."
  (interactive "P")
  (switch-to-buffer-create "*scratch*" 'lisp-interaction-mode arg))

(defun jump-to-text-scratch-buffer (&optional arg)
  "*Switch to buffer `*text scratch*', creating it if necessary.
With prefix arg generate a fresh buffer."
  (interactive "P")
  (let ((buffer-name "*text scratch*"))
    (switch-to-buffer-create (if arg (generate-new-buffer-name buffer-name) buffer-name)
                             default-major-mode
                             nil)))

;; (defun backward-down-list (&optional arg)
;;   "Move backward down one level of parentheses.
;; With ARG, do this that many times.
;; A negative argument means move forward but still down a level."
;;   (interactive "p")
;;   (down-list (- (or arg 1))))

;; ;; From http://www.cs.indiana.edu/chezscheme/emacs/iuscheme.el
;; (defun scheme-return ()
;;   "Newline and indent, or evaluate the sexp before the prompt.
;; Complete sexps are evaluated; for incomplete sexps inserts a newline
;; and indents."
;;   (interactive)
;;   (let ((input-start (process-mark (get-buffer-process (current-buffer)))))
;;     (if (< (point) input-start)
;;         (comint-send-input)             ; this does magic stuff
;;       (let ((state (save-excursion
;;                      (parse-partial-sexp input-start (point)))))
;;         (if (and (< (car state) 1)      ; depth in parens is zero
;;                  (not (nth 3 state))    ; not in a string
;;                  (not (save-excursion   ; nothing after the point
;;                         (search-forward-regexp "[^ \t\n\r]" nil t))))
;;             (comint-send-input)         ; then go for it.
;;           (newline-and-indent))))))

;; ;; From http://www.cs.indiana.edu/chezscheme/emacs/iuscheme.el
;; (defun scheme-indent-definition ()
;;   "Fix indentation of the current definition."
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-defun)
;;     (indent-sexp)))

;; (add-hook 'scheme-mode-hook
;;           (lambda ()
;;             (defkey scheme-mode-map "C-c <tab>" 'scheme-indent-definition)
;;             (defkey scheme-mode-map "C-c S" 'scheme-scratch)
;;             (defkey scheme-mode-map "C-M-S-d" 'backward-down-list)))

;; (add-hook 'inferior-scheme-mode-hook
;;           (lambda ()
;;             (defkey inferior-scheme-mode-map "C-c <tab>" 'scheme-indent-definition)
;;             (defkey inferior-scheme-mode-map "<return>" 'scheme-return)
;;             (defkey inferior-scheme-mode-map "C-j" 'comint-send-input)))

;; ;; Automode
;; (add-to-list 'interpreter-mode-alist '("scsh" . scheme-mode))

;;  (setenv "SCSH_LIB_DIRS" "\"/usr/local/scsh-packages/0.6\"")

;; ;;(setq scheme-program-name "scsh")
;; ;;(setq scheme-program-name "e:/tools/PLT-Scheme/MzScheme.exe")
;; (setq scheme-program-name "e:/home/ecl/.bin/i686-cygwin/s48.bat")
;; (add-hook 'scheme-mode-hook
;;           (lambda ()
;;             (modify-syntax-entry ?\| "_")
;;             (mapc-pair (lambda (x y)
;;                          (font-lock-add-keywords
;;                           nil
;;                           `((,(format "\\<%s\\>" x) . font-lock-keyword-face)))
;;                          (put x 'scheme-indent-function y))
;;                        '((begin0 . 0)
;;                          (when . 1)
;;                          (unless . 1)
;;                          (scheme-indent-function . scheme-let-indent)
;;                          (with-current-input-port . 1)))))


;;; Perl mode
;; use cperl-mode as default
;; (defalias 'perl-mode 'cperl-mode)
(setq cperl-hairy nil)
(setq cperl-font-lock t)
(setq cperl-clobber-lisp-bindings nil)
(setq cperl-lazy-help-time 1)
(setq cperl-info-page "Perl")
;;(setq cperl-electric-parens t)
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
            ;;(cperl-toggle-autohelp)
            (cperl-lazy-install)
            (when (fboundp 'skeleton-pair-insert-maybe)
              (fset 'cperl-electric-paren 'skeleton-pair-insert-maybe)
              (fset 'cperl-electric-rparen 'self-insert-command))
	    (set-compile-command "perl -cw %s")))


;;; PHP
(require-soft 'php-mode)
;; (when (require-soft 'html-script)
;;   (defkey html-mode-map "C-c C-c" 'html-script-pop-to-script-buffer))

(defun php (symbol)
  (require 'thingatpt)
  (interactive (list 
                (let ((symbol (thing-at-point 'symbol)))
                  (if symbol
                      (read-string (format "Symbol [default \"%s\"]: " symbol) nil nil symbol)
                    (read-string "Symbol: ")))))
  
  (browse-url (format "http://www.php.net/%s" (string-make-unibyte symbol))))

;; (add-to-path 'load-path "~/Emacs/mmm-mode-0.4.8")
;; (when (require-soft 'mmm-auto)
;;   (setq mmm-global-mode 'maybe)
;;   (set-face-background 'mmm-default-submode-face "gray90")
;;   ;; (setq mmm-mode-ext-classes-alist nil)
;;   (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)
;;   ;; (mmm-add-mode-ext-class 'html-mode "" 'fancy-html)
;;   ;; (mmm-add-group 'fancy-html
;;   ;;                '((html-php-tagged
;;   ;;                   :submode php-mode
;;   ;;                   :face mmm-code-submode-face
;;   ;;                   :front "<[?]php"
;;   ;;                   :back "[?]>")))

;;   ;; (mmm-add-classes
;;   ;;  '((php-here-doc
;;   ;;     :front "<<<\\([a-zA-Z0-9_-]+\\)"
;;   ;;     :back "^~1[;]?$"
;;   ;;     :save-matches 1
;;   ;;     :submode html-mode
;;   ;;     :delimiter-mode nil)))
;;   )


;;; Compile
(setq compile-command "make ")
(setq compilation-read-command nil)
(setq compilation-ask-about-save t)
(setq compilation-scroll-output t)
(add-hook 'compilation-mode-hook (lambda () (toggle-truncate-lines -1)))

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

;; ;; Hilight error-line, by Kevin Rodgers and Klaus Berndl
;; (defvar compilation-source-overlay (make-overlay 1 1)
;;   "Internal overlay used for the source-line in the source-buffer")

;; (defcustom compilation-highlight-source 'secondary-selection
;;   "*Face to highlight the source-line in the source-buffer.
;; Nil for no highlight."
;;   :group 'compilation
;;   :set '(lambda (symbol value)
;;           (set symbol value)
;;           (if (and value (facep value))
;;               (overlay-put compilation-source-overlay 'face value)))
;;   :type '(radio (const :tag "No highlighting of source-line" :value nil)
;;                 (face :tag "Face for the source-line")))

;; (defvar compilation-error-overlay (make-overlay 1 1)
;;   "Internal overlay used for the error-line in the compilation-buffer")

;; (defcustom compilation-highlight-error 'secondary-selection
;;   "*Face used to highlight the error-line in the compilation-buffer.
;; Nil for no highlight."
;;   :group 'compilation
;;   :set '(lambda (symbol value)
;;           (set symbol value)
;;           (if (and value (facep value))
;;               (overlay-put compilation-error-overlay 'face value)))
;;   :type '(radio (const :tag "No highlighting of error-line" :value nil)
;;                 (face :tag "Face for the error-line")))

;; (add-hook 'pre-command-hook
;;           (lambda ()
;;             (delete-overlay compilation-source-overlay)))

;; (defadvice compilation-goto-locus (after highlight)
;;   "If `compilation-highlight-error' is non-nil, highlight the ERROR line.
;; If `compilation-highlight-source' is non-nil, highlight the SOURCE line."
;;   (let ((error-marker (car (ad-get-arg 0))) ; (car NEXT-ERROR)
;;         (source-marker (cdr (ad-get-arg 0)))) ; (cdr NEXT-ERROR)
;;     (if compilation-highlight-error
;;         (save-excursion
;;           (set-buffer (marker-buffer error-marker))
;;           (goto-char (marker-position error-marker))
;;           (move-overlay compilation-error-overlay
;;                         (line-beginning-position)
;;                         (line-end-position)
;;                         (current-buffer))))
;;     (if compilation-highlight-source
;;         (save-excursion
;;           (set-buffer (marker-buffer source-marker))
;;           (goto-char (marker-position source-marker))
;;           (move-overlay compilation-source-overlay
;;                         (line-beginning-position)
;;                         (line-end-position)
;;                         (current-buffer))))))

;; ;; this function's advice only enables the advice of `compilation-goto-locus'
;; ;; temporally so the highlighting is performed. This is necessary because
;; ;; other packages use `compilation-goto-locus' and these packages don't need
;; ;; the highlight stuff.
;; (defadvice next-error (around highlight activate)
;;   "If `compilation-highlight-error' is non-nil, highlight the ERROR line.
;; If `compilation-highlight-source' is non-nil, highlight the SOURCE line."
;;   (unwind-protect
;;       (progn
;;         (ad-enable-advice 'compilation-goto-locus 'after 'highlight)
;;         (ad-activate 'compilation-goto-locus)
;;         ad-do-it)
;;     (ad-disable-advice 'compilation-goto-locus 'after 'highlight)
;;     (ad-activate 'compilation-goto-locus)))


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
(when at-bmw
  (setq user-mail-address "Emilio.Lopes@partner.bmw.de")
  (setq mail-default-reply-to "Emilio.Lopes@partner.bmw.de"))

(setq mail-from-style nil)

(setq mail-aliases t)
(setq mail-personal-alias-file "~/.mailrc")

(setq mail-yank-prefix "> ")

(add-hook 'mail-setup-hook 'mail-abbrevs-setup)


;;; Message (see "~/.message")
(eval-after-load "message" '(load (expand-file-name "~/.message")))



;;; Resume/Server configuration.
;; With these hooks and using emacs.bash (or emacs.csh), both from
;; "etc" dir, it is possible to specify arguments when resuming emacs
;; after a suspension.
(add-hook 'suspend-hook 'resume-suspend-hook)
(add-hook 'suspend-resume-hook 'resume-process-args)
(server-start)
;; (when window-system 
;;   (server-start))
;; (setq server-program "e:/tools/emacs/gnuserv/gnuserv.exe")
;; (when (require-soft 'gnuserv)
;;   ;;  (gnuserv-start)
;;   (defun server-make-window-visible ()
;;     "*Try to make the window visible.
;; Fixes a bug in gnuserv.el 2.1."
;;     (make-frame-visible)
;;     (raise-frame (selected-frame)))
;;   (setq gnuserv-frame (selected-frame)))

;; (when (load "e:/tools/emacs/emacs-cvs/EmacsW32/bin/gnuserv" 'no-error)
;;   (unless gnuserv-process
;;     (setq gnuserv-program "e:/tools/emacs/emacs-cvs/EmacsW32/bin/gnuserv.exe")
;;     (gnuserv-start))
;;   (setq gnuserv-frame (selected-frame)))


;;; Abbrevs
(setq save-abbrevs t)
(setq abbrev-file-name (expand-file-name "~/.abbrevs"))
(when (file-readable-p abbrev-file-name)
  (quietly-read-abbrev-file))



;;; Bookmarks
(setq bookmark-save-flag 1)
(setq bookmark-default-file "~/.emacs.bookmarks")

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

;; From http://www.emacswiki.org/cgi-bin/wiki.pl/GraphicalBookmarkJump
(defun iswitchb-bookmark-jump (bname)
  "*Switch to bookmark interactively using `iswitchb'."
  (interactive (list (flet
                         ((iswitchb-make-buflist (default)
                                                 (require 'bookmark)
                                                 (setq iswitchb-buflist (bookmark-all-names))))
                       (iswitchb-read-buffer "Jump to bookmark: "))))
  (bookmark-jump bname))
(substitute-key-definition 'bookmark-jump 'iswitchb-bookmark-jump global-map)

;;; folding
(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)


;;; TMM
(setq tmm-completion-prompt nil)
(setq tmm-mid-prompt ": ")
(setq tmm-shortcut-style 'downcase)
(setq tmm-shortcut-words nil)



;;; ibuffer
(when (require-soft 'ibuffer)
  (global-defkey "C-x C-b" 'ibuffer)
  (setq ibuffer-formats
        '((mark modified read-only " " (name 16 16) " " (mode 16 16) " "  filename)
          (mark modified read-only " " (name 16 16) " "  filename)
          (mark modified read-only " " (name 16 16) " " (size 6 -1 :right) " " (mode 16 16) "  " (process 8 -1) " " filename)))
  (setq ibuffer-elide-long-columns t)
  (setq ibuffer-never-show-regexps '("^ "))
  (setq ibuffer-maybe-show-regexps '("^\*" "\.newsrc-dribble"))
  (setq ibuffer-expert t)
  (setq-default ibuffer-default-sorting-mode 'major-mode)

  (setq ibuffer-display-summary nil)

  (setq ibuffer-saved-filter-groups
        '(("default" 
           ("Dired" (mode . dired-mode))
           ("Remote" (predicate file-remote-p (or (buffer-file-name (current-buffer))
                                                  (directory-file-name default-directory))))
           ;; ("Shells" (mode . shell-mode))
           ("Shells" (predicate processp (get-buffer-process (current-buffer))))
           ("Project" (filename . "/bms08/"))
           ("Org" (or (mode . org-mode) (filename . "u:/cenis/")))
           ("Gnus" (saved . "gnus"))
           ("Help" (predicate memq major-mode ibuffer-help-buffer-modes))
           ("Volatile" (name . "^\\*")))))

  (setq ibuffer-show-empty-filter-groups nil)

  (defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups () activate)
    (let ((default (assoc-string "Default" ad-return-value)))
      (setq ad-return-value (nconc (delq default ad-return-value) (list default)))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))

  (defun ibuffer-dired-buffers ()
    "*Limit current view to Dired buffers only."
    (interactive)
    (ibuffer-filter-by-mode 'dired-mode))

  (defkey ibuffer-mode-map "/ D" 'ibuffer-dired-buffers)

  (defadvice ibuffer-confirm-operation-on (around confirm-with-y-or-n-p activate)
    "Use `y-or-n-p' instead of `yes-or-no-p' to confirm operations."
    (flet ((yes-or-no-p (prompt) (y-or-n-p prompt)))
      ad-do-it))

  (defadvice ibuffer-marked-buffer-names (after current-buffer-if-none-marked activate)
    "*Return current buffer (and mark it) if none is marked.
This way the `ibuffer-do-*' commands operate on the current buffer if
none is marked."
    (unless ad-return-value
      (let ((buffer (ibuffer-current-buffer)))
        (when buffer
          (ibuffer-mark-interactive 1 ibuffer-marked-char 0)
          (setq ad-return-value (list buffer))))))
  )

;;; Dired
;;(eval-after-load "ls-lisp" '(require-soft 'ls-lisp-bugfix))
(setq ls-lisp-use-insert-directory-program t)
(add-hook 'dired-load-hook (lambda () (require-soft 'dired_rc)))

;;; Org-mode
(require-soft 'orgrc)


;;; eshell
;; TODO: Make it more generic (`next-buffer-satisfying') and use "ring.el"
(defun eshell-next-buffer (arg)
"Switch to the next EShell buffer.
Start a new Eshell session if invoked with prefix argument ARG or if
no EShell session is currently active."
  (interactive "P")
  (let (eshell-buffers)
    (mapc (lambda (buffer)
            (when (with-current-buffer buffer
                    (string-match "\\`EShell\\'" mode-name))
              (add-to-list 'eshell-buffers buffer 'append)))
          (buffer-list))
    (if (or arg (not eshell-buffers))
        (eshell t)
      (switch-to-buffer (car (delete (current-buffer) eshell-buffers))))))

(add-hook 'eshell-load-hook (lambda () (require-soft 'eshell_rc)))



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

(defun shell-hier (&optional dir)
  (interactive)
  (let* ((dir (or dir default-directory))
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

(eval-after-load "shell" '(require-soft 'shell_rc))


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
  '(add-to-list 'grep-find-ignored-directories "_darcs"))

;; (setq grep-find-command
;;       '("find . \\( -path '*/_darcs' -o -path '*/CVS' -o -path '*/RCS' -o -path '*/{arch}' \\) -prune -o -type f -and \"!\" \\( -name '*~' -o -name '*#' \\) -exec grep -s -n  {} NUL \\;" . 160))

;; (setq grep-find-template "find . <X> -type f -and \"!\" \\( -name '*~' -o -name '*#' \\) <F> -exec grep <C> -n -e <R> {} NUL \\;")

;;; igrep
(autoload 'igrep "igrep"
  "*Run `grep' PROGRAM to match EXPRESSION in FILES..." t)
(autoload 'igrep-find "igrep"
  "*Run `grep' via `find'" t)
(autoload 'igrep-visited-files "igrep"
  "*Run `grep' on all visited files." t)
(autoload 'dired-do-igrep "igrep"
  "*Run `grep' on the marked (or next prefix ARG) files." t)
(autoload 'dired-do-igrep-find "igrep"
  "*Run `grep' via `find` on the marked (or next prefix ARG) directories." t)
(autoload 'Buffer-menu-igrep "igrep"
  "*Run `grep' on the files visited in buffers marked with `>'." t)

(setq igrep-verbose-prompts t)
(setq igrep-expression-option nil)


;;; calendar
(add-hook 'calendar-load-hook
          (lambda ()
            (european-calendar)
            (defun calendar-goto-iso-week (week year &optional noecho)
              "Move cursor to start of ISO WEEK in YEAR; echo ISO date unless NOECHO is t.
Interactively asks for YEAR only when called with a prefix argument."
              (interactive
               (let* ((today (calendar-current-date))
                      (year (if current-prefix-arg
                                (calendar-read
                                 "ISO calendar year (>0): "
                                 '(lambda (x) (> x 0))
                                 (int-to-string (extract-calendar-year today)))
                              (extract-calendar-year today)))
                      (no-weeks (extract-calendar-month
                                 (calendar-iso-from-absolute
                                  (1-
                                   (calendar-dayname-on-or-before
                                    1 (calendar-absolute-from-gregorian
                                       (list 1 4 (1+ year))))))))
                      (week (calendar-read
                             (format "ISO calendar week (1-%d): " no-weeks)
                             '(lambda (x) (and (> x 0) (<= x no-weeks))))))
                 (list week year)))
              (calendar-goto-date (calendar-gregorian-from-absolute
                                   (calendar-absolute-from-iso
                                    (list week calendar-week-start-day year))))
              (or noecho (calendar-print-iso-date)))
            (defkey calendar-mode-map "g w" 'calendar-goto-iso-week)
            (setq calendar-week-start-day 1)))


;; display the ISO week numbers (from the help of `calendar-intermonth-text')
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-function-name-face))

;; German settings and holidays

(setq calendar-time-display-form
      '(24-hours ":" minutes (and time-zone (concat " (" time-zone ")"))))

(setq calendar-day-name-array
      ["Sonntag" "Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag" "Samstag"])
(setq calendar-month-name-array
      ["Januar" "Februar" "März" "April" "Mai" "Juni"
       "Juli" "August" "September" "Oktober" "November" "Dezember"])
(setq solar-n-hemi-seasons
      '("Frühlingsanfang" "Sommeranfang" "Herbstanfang" "Winteranfang"))

(setq general-holidays
      '((holiday-fixed 1 1 "Neujahr")
        (holiday-fixed 5 1 "1. Mai")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit")))

(setq christian-holidays
      '((holiday-float 12 0 -4 "1. Advent" 24)
        (holiday-float 12 0 -3 "2. Advent" 24)
        (holiday-float 12 0 -2 "3. Advent" 24)
        (holiday-float 12 0 -1 "4. Advent" 24)
        (holiday-fixed 12 25 "1. Weihnachtstag")
        (holiday-fixed 12 26 "2. Weihnachtstag")
        (holiday-fixed 1 6 "Heilige Drei Könige")
        ;; Date of Easter calculation taken from holidays.el.
        (let* ((century (1+ (/ displayed-year 100)))
               (shifted-epact (% (+ 14 (* 11 (% displayed-year 19))
                                    (- (/ (* 3 century) 4))
                                    (/ (+ 5 (* 8 century)) 25)
                                    (* 30 century))
                                 30))
               (adjusted-epact (if (or (= shifted-epact 0)
                                       (and (= shifted-epact 1)
                                            (< 10 (% displayed-year 19))))
                                   (1+ shifted-epact)
                                 shifted-epact))
               (paschal-moon (- (calendar-absolute-from-gregorian
                                 (list 4 19 displayed-year))
                                adjusted-epact))
               (easter (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))
          (filter-visible-calendar-holidays
           (mapcar
            (lambda (l)
              (list (calendar-gregorian-from-absolute (+ easter (car l)))
                    (nth 1 l)))
            '(
              ;;(-48 "Rosenmontag")
              ( -2 "Karfreitag")
              (  0 "Ostersonntag")
              ( +1 "Ostermontag")
              (+39 "Christi Himmelfahrt")
              (+49 "Pfingstsonntag")
              (+50 "Pfingstmontag")
              (+60 "Fronleichnam")))))
        (holiday-fixed 8 15 "Mariae Himmelfahrt")
        (holiday-fixed 11 1 "Allerheiligen")
        ;;(holiday-float 11 3 1 "Buss- und Bettag" 16)
        (holiday-float 11 0 1 "Totensonntag" 20)))

(setq calendar-holidays
      (append general-holidays local-holidays other-holidays
              christian-holidays solar-holidays))


;;; Occur
(defun my-occur-mode-hook ()
  (defkey occur-mode-map "n" 'occur-next)
  (defkey occur-mode-map "<down>" 'occur-next)
  (defkey occur-mode-map "p" 'occur-prev)
  (defkey occur-mode-map "<up>" 'occur-prev))
(add-hook 'occur-mode-hook 'my-occur-mode-hook)

(defun occur-shrink-window ()
  "*Shrink the \"*Occur*\" window as much as possible to display its contents."
  (let ((win (get-buffer-window "*Occur*")))
    (when (windowp win)
      (shrink-window-if-larger-than-buffer win))))
(add-hook 'occur-hook 'occur-shrink-window)



;;; Diff/Ediff

(setq diff-switches "-u")

(setq ediff-keep-variants nil)

(add-hook 'ediff-load-hook
          (lambda ()
            (setq ediff-diff-options (concat ediff-diff-options " --minimal --ignore-all-space"))
            (mapc (lambda (face)
                    (set-face-foreground face "black")
                    (set-face-background face "sky blue"))
                  (list ediff-current-diff-face-A
                        ediff-current-diff-face-B))

            (set-face-foreground ediff-fine-diff-face-A "firebrick")
            (set-face-background ediff-fine-diff-face-A "pale green")
            (set-face-foreground ediff-fine-diff-face-B "dark orchid")
            (set-face-background ediff-fine-diff-face-B "yellow")

            (setq ediff-window-setup-function 'ediff-setup-windows-plain)

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


(defun vc-ediff ()
  "*Compare revisions of the visited file using Ediff."
  (interactive)
  (require 'ediff)
  (ediff-load-version-control)
  (ediff-vc-internal "" "" nil))

(global-defkey "C-x v =" 'vc-ediff)

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


;;{{{ which-function-mode
;; (eval-after-load "which-func"
;;   '(setq  which-func-modes (remove 'emacs-lisp-mode which-func-modes)))
;; (which-function-mode +1)
;;}}}


;;{{{ Printing
(require-soft 'printing)

(if running-nt
    ;; (progn                              ; Windows NT settings
;;       (setq lpr-command "")             ; write directly to the printer port
;;       (when at-bmw
;;         (setq pr-path-alist '((windows   PATH ghostview)
;;                               (ghostview "e:/tools/GSTools/GSView/4.0/gsview")))
;;         (setq pr-ps-printer-alist
;;               (mapcar (lambda (prt)
;;                         (list (make-symbol (file-name-nondirectory prt)) "print" nil "/D:" prt))
;;                       '("\\\\eepmuc02\\pm193000"
;;                         "\\\\eepmuc01\\pm193001"
;;                         "\\\\eepmuc02\\pm192304"
;;                         "\\\\gmuc0119\\pmuc0516")))
;;         (setq pr-ps-name 'pmuc0516))
;;       (setq printer-name
;;             (cond
;;              (at-bmw "//gmuc0119/PMUC0516")
;; ;;             (at-bmw "//eepmuc02/PM192304")
;;              (t "LPT1"))))

    (progn                              ; Windows NT settings
      (setq lpr-command "")            ; write directly to the printer port
      (setq printer-name
            (cond
             (at-bmw "//gmuc0153.muc/PMUC0716")
             (t "LPT1")))
      (when at-bmw
        (setenv "GS_LIB" "e:/tools/GSTools/gs8.14/gs8.14/lib;e:/tools/GSTools/gs8.14/fonts")
        (setq ps-lpr-command "e:/tools/GSTools/gs8.14/gs8.14/bin/gswin32c.exe")
        (setq ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2"))
        (setq ps-printer-name t)))
  (progn                                ; Unix settings
    (setq lpr-command "lpr")
    (setq printer-name
          (cond
           (at-home
            "stylus810-draft")))))


(when (require-soft 'printing)
  (pr-update-menus t))

;; (setq ps-lpr-command lpr-command)
;; (setq ps-printer-name printer-name)

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

;; (if (require-soft 'printer)
;;     (global-defkey "C-c p p" 'printer-print)
;;   (global-defkey "C-c p p" (if window-system 'ps-print-buffer-with-faces 'ps-print-buffer)))

(global-defkey "C-c p p" (if window-system 'ps-print-buffer-with-faces 'ps-print-buffer))

(global-defkey "C-c p s" (if window-system 'ps-spool-buffer-with-faces 'ps-spool-buffer))

(global-defkey "C-c p b" (if window-system 'ps-print-buffer-with-faces 'ps-print-buffer))
(global-defkey "C-c p B" (if window-system 'ps-spool-buffer-with-faces 'ps-spool-buffer))
(global-defkey "C-c p r" (if window-system 'ps-print-region-with-faces 'ps-print-region))
(global-defkey "C-c p R" (if window-system 'ps-spool-region-with-faces 'ps-spool-region))

(global-defkey "C-c p l" 'ps-print-buffer-maybe-with-faces-landscape)
(global-defkey "C-c p n" 'ps-print-buffer-maybe-with-faces-n-up)

(global-defkey "C-c p S" 'set-default-printer)


;;}}}

;;; Calc
(setq calc-settings-file "~/.calc")
(setq calc-full-mode t)
(setq calc-display-trail nil)



;;; timeclock
;; (setq timeclock-workday (* 9 60 60))
;; (setq timeclock-relative nil)
;; (setq timeclock-get-project-function nil)
;; (setq timeclock-ask-before-exiting t)

;; (require-soft 'timeclock)
;; (timeclock-modeline-display)
;; (add-hook 'kill-emacs-hook 'timeclock-query-out)


;; (global-defkey "C-c t" (make-sparse-keymap))
;; (global-defkey "C-c t i" 'timeclock-in)
;; (global-defkey "C-c t o" 'timeclock-out)
;; (global-defkey "C-c t s" 'timeclock-status-string)
;; (global-defkey "C-c t r" 'timeclock-reread-log)
;; (global-defkey "C-c t u" 'timeclock-update-modeline)
;; (global-defkey "C-c t w" 'timeclock-when-to-leave-string)


;;; Man
(eval-after-load "man"
  '(progn
     (setq Man-notify-method 'friendly)
     (defkey Man-mode-map "q" 'Man-kill)))



;;; Woman
(setq woman-use-own-frame nil)



;;; Browse URL
(setq browse-url-new-window-flag nil)
(cond
 ((and running-nt at-bmw)
  (setq browse-url-browser-function 'browse-url-default-windows-browser)
  ;; (setq browse-url-mozilla-program "C:/Program Files/Mozilla Firefox/firefox.exe")
  ;; (setq browse-url-browser-function 'browse-url-mozilla)
  (setq browse-url-mozilla-new-window-is-tab t)
  )
 (at-home
  (setq browse-url-browser-function 'browse-url-netscape)
  (setq browse-url-netscape-program "mozilla"))
 ((require-soft 'w3-auto)
  (setq browse-url-browser-function 'browse-url-w3)))

;;(setq browse-url-browser-function 'browse-url-lynx-xterm)
;; (setq browse-url-xterm-program "xterm")
;; (setq browse-url-xterm-args
;;       '("+sb" "-T" "Lynx" "-n" "Lynx" "-g" "100x50" "-tn" "xterm-color"))



;;; W3
(when (or (require-soft 'w3-auto)
          (require-soft 'url))
  (cond
   (at-bmw
    (setq url-proxy-services '(("http" . "proxy.muc:8080")
                               ("ftp" .  "proxy.muc:8080"))))))



;;; Custom
(setq custom-file "~/.custom")
(when (file-readable-p custom-file)
  (load-file custom-file))



;;; X11
;; Add a new submenu to the font menu
(let ((submenu '("ETL"
                 ("12" "-etl-fixed-medium-r-normal-*-*-120-*-*-*-*-iso8859-1")
                 ("14" "-etl-fixed-medium-r-normal-*-*-140-*-*-*-*-iso8859-1")
                 ("16" "-etl-fixed-medium-r-normal-*-*-160-*-*-*-*-iso8859-1")
                 ("18" "-etl-fixed-medium-r-normal-*-*-180-*-*-*-*-iso8859-1")
                 ("24" "-etl-fixed-medium-r-normal-*-*-240-*-*-*-*-iso8859-1"))))
  (unless (member submenu (cdr x-fixed-font-alist))
    (setcdr x-fixed-font-alist
            (append (cdr x-fixed-font-alist) (list submenu)))))


;;; isearch
;; (setq lazy-highlight-initial-delay 3)
(setq isearch-allow-scroll t)

;; (add-hook 'isearch-mode-end-hook 'goto-isearch-match-beginning)

;; (defun goto-isearch-match-beginning ()
;;   (when (and isearch-forward
;;              (number-or-marker-p isearch-other-end))
;;     (goto-char isearch-other-end)))

;; (defadvice isearch-exit (after goto-match-beginning activate)
;;   "Go to beginning of match."
;;   (goto-isearch-match-beginning))

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

(defadvice describe-function (after where-is activate)
  "Call `\\[where-is] FUNCTION' iff it's interactive."
  (let ((func (ad-get-arg 0)))
    (when (commandp func)
      (where-is func))))

(defun unfill-paragraph ()
  (interactive "*")
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; see http://angg.twu.net/eev-article.html
(defun ee (s e)
  "Save the region in a temporary script"
  (interactive "r")
  (write-region s e "~/.ee.sh"))


(mapc-pair (lambda (x y)
             (when (fboundp x)
               (funcall x y)))
           '((menu-bar-mode . -1)
             (tool-bar-mode . -1)
             (scroll-bar-mode . -1)
             (transient-mark-mode . -1)
             (blink-cursor-mode . -1)
             (show-paren-mode . +1)
             (line-number-mode . +1)
             (column-number-mode . +1)
             (savehist-mode . +1)))

(global-defkey "<down-mouse-3>" 'mouse-major-mode-menu)

;;(x11-maximize-frame-vertically)
(when (and running-nt (eq window-system 'w32))
  (add-hook 'window-setup-hook 'w32-maximize-frame 'append))

;;; Restore mini-buffer history
;; see call to `savehist-mode' above



;;; Misc history
;; Add *all* visited files to `file-name-history', no matter if they
;; are visited through Dired or gnuclient or whatever.
(defun add-filename-to-history ()
  "*Add or move the visited file to the beginning of `file-name-history'."
  (let ((filename buffer-file-name))
    (when filename
      (setq file-name-history (cons filename (delete filename file-name-history)))))
  nil)
(add-hook 'find-file-hook 'add-filename-to-history)

;;  show paren
(setq show-paren-mode-hook nil)
(add-hook 'show-paren-mode-hook 
          (lambda ()
            (set-face-foreground 'show-paren-match-face "orange")
            (set-face-background 'show-paren-match-face "moccasin")))

;;; Local Configuration
(when at-bmw
  ;;(set-register ?A '(file . "//smuc1805/EE-ORG/Austausch/EE-22/ecl/"))
  (defun bmw-jump-to-exchange-dir (&optional arg)
    "*Visit (using Dired) the exchange directory, creating it if necessary."
    (interactive "P")
    (let ((exchange "//easerv.muc/Organisation/EA-41/Austausch/ecl")
          (find-file-existing-other-name t))
      (when arg (setq exchange (file-name-directory (directory-file-name exchange))))
      (unless (file-accessible-directory-p exchange)
        (make-directory exchange t))
      (dired exchange)))
  (global-defkey "C-c j A" 'bmw-jump-to-exchange-dir)
  (global-defkey "C-c j u" (lambda () (interactive) (dired "u:/")))
  (set-register ?P '(file . "//smuc1830/Projekt/DDE/")))

;; Fix the system PATH at BMW
(when at-bmw
  (setq exec-path
        (let (path)
          (mapc (lambda (dir)
                  (let ((case-fold-search t))
                    (cond
                     ((string-match "[\\/]winnt" dir)
                      (add-to-list 'path dir 'append)) ; append
                     ((string-match "[\\/]\\(orant\\|oracle\\|dds\\)" dir)
                      nil)              ; do nothing i.e. remove
                     (t
                      (add-to-list 'path dir)))))
                (reverse exec-path))
          path))
  (add-to-path 'exec-path "e:/tools/ispell" 'append)
  (add-to-path 'exec-path "c:/Program Files/PuTTY" 'append)
  (require-soft 'cygpath)
  (let ((cygwin-prefix "e:/tools/gnu"))
    (add-to-path 'exec-path (concat cygwin-prefix "/bin") 'append)
    (add-to-path 'exec-path (concat cygwin-prefix "/usr/local/bin") 'append)
    (add-to-path 'Info-default-directory-list (concat cygwin-prefix "/usr/info") 'append)
    (setenv "MAGIC" (cygpath-windows2unix (concat cygwin-prefix "/usr/share/magic")))
    (setq woman-manpath
          (mapcar (lambda (dir) (concat cygwin-prefix dir)) '("/usr/local/man" "/usr/man"))))
  (add-to-list 'woman-manpath "/sshx:eas254@eas254.muc:/usr/share/man")
  (setenv "PATH" (mapconcat (if running-nt
                                (lambda (dir)
                                  (subst-char-in-string ?/ ?\\ dir))
                              'identity) exec-path path-separator)))


;;; periodically kill old buffers
(require 'midnight)


;; ;; save a bunch of variables to the desktop file
;; ;; for lists specify the len of the maximal saved data also
;; (setq desktop-globals-to-save
;;       (append '((extended-command-history . 30)
;;                 (file-name-history        . 100)
;;                 (grep-history             . 30)
;;                 (compile-history          . 30)
;;                 (minibuffer-history       . 50)
;;                 (query-replace-history    . 60)
;;                 (read-expression-history  . 60)
;;                 (regexp-history           . 60)
;;                 (regexp-search-ring       . 20)
;;                 (search-ring              . 20)
;;                 (shell-command-history    . 50)
;;                 tags-file-name
;;                 register-alist)))


(when  (and (fboundp 'escreen-create-screen)
            running-interactively
            ;; (yes-or-no-p "Setup desktop?")
            )
  (escreen-create-screen)
  (escreen-create-screen)
  (escreen-create-screen)
  (escreen-create-screen)
  (escreen-create-screen)
  (escreen-create-screen)
  (when (fboundp 'escreen-set-screen-number)
    (escreen-set-screen-number -1))
  (escreen-create-screen)
  ;; (when (fboundp 'escreen-set-screen-number) (escreen-set-screen-number 9))
  ;; (when (yes-or-no-p "Start Gnus?")
  ;;     (gnus))
  )

(when at-bmw
  (setq bmw-suppress-local-keybindings t)
  (setq bmw-suppress-local-look-and-feel t))
;;; "~/.emacs" ends here

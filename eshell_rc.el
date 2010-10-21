;; eshell_rc: Eshell initialization
;; Time-stamp: <2006-07-13 10:26:26 Emilio C. Lopes>

(require 'esh-module)
;;(add-to-list 'eshell-modules-list 'eshell-rebind)
;;(add-to-list 'eshell-modules-list 'eshell-smart)
(add-to-list 'eshell-modules-list 'eshell-zle)

(setq eshell-history-size 1024)
(setq eshell-hist-ignoredups t)
(setq eshell-ask-to-save-history 'always)

(setq eshell-buffer-maximum-lines 2048)

(setq eshell-scroll-to-bottom-on-input 'this)
(add-hook 'eshell-output-filter-functions 'eshell-postoutput-scroll-to-bottom)

(setq eshell-cmpl-cycle-completions nil)
(setq eshell-cmpl-use-paring nil)

;;(setq eshell-ls-initial-args '("-h" "-I" "*~"))
(setq eshell-ls-initial-args '("-h"))

(require 'em-ls)
(setq eshell-ls-exclude-regexp eshell-ls-backup-regexp)

(setq eshell-pushd-dunique t)
(setq eshell-last-dir-unique t)

(setq eshell-mv-interactive-query t)
(setq eshell-cp-interactive-query t)
(setq eshell-ln-interactive-query t)

(defalias 'eshell/ff 'find-name-dired)
(defalias 'eshell/gf 'find-grep-dired)

(when (eshell-under-windows-p)
  (require 'esh-ext)
  (setq eshell-windows-shell-file (or (eshell-search-path "cmd.exe")
                                      (eshell-search-path "command.exe")))
  (setq eshell-force-execution t)
  (add-to-list 'eshell-binary-suffixes ".pl")
  (defsubst eshell-invoke-perl (&rest args)
    "*Invoke Perl to run a Perl-Script."
    (throw 'eshell-replace-command
           (eshell-parse-command (eshell-search-path "perl") (cons "-S" args))))
  (add-to-list 'eshell-interpreter-alist '("\\.pl\\'" . eshell-invoke-perl)))

(defun eshell-dired-pwd ()
  "*Run Dired on the current working directory."
  (interactive)
  (dired (eshell/pwd)))

(defun eshell/path ()
  "*Show the components of the execution PATH, one per line."
  (eshell/echo (replace-regexp-in-string path-separator "\n" (eshell-get-variable "PATH"))))

(defadvice eshell/cd (before allow-files activate)
  "If argument is a file, change to it's directory."
  (when (and (ad-get-arg 0)
             (file-regular-p (ad-get-arg 0))
             (= (length (ad-get-args 0)) 1))
    (ad-set-arg 0 (file-name-directory (ad-get-arg 0)))))

;; Kai Groﬂjohann
(defun eshell/less (&rest args)
  "Invoke `view-file' on the file.
  \"less +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\+\\([0-9]+\\)\\'" (car args))
        (let ((line (string-to-number (match-string 1 (pop args))))
              (file (pop args)))
          (message "line: %s" line)
          (view-file file)
          (goto-line line))
      (view-file (pop args)))))
(defalias 'eshell/l 'eshell/less)

;; From http://www.khngai.com/emacs/eshell.php
(defun eshell/clear ()
  "*Clear the eshell buffer, deleting its entire contents."
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; Based on some code by Joseph L. Casadonte Jr.
(defun eshell-substitute-tilde-for-homedir (path)
  "*Substitute a tilde (\"~\") for the user's home-directory in PATH."
  (let ((home (replace-regexp-in-string "\\\\" "/" (expand-file-name "~"))))
    (when (string-match (concat "\`\\(" (regexp-quote home) "\\)\\(/.*\\)?\'") path)
      (setq path (replace-match "~" nil nil path 1)))
    path))
(setq eshell-pwd-convert-function 'eshell-substitute-tilde-for-homedir)

;; Mathias Dahl
(defun eshell/open (FILE)
  "*Invoke (w32-shell-execute \"Open\" FILE) substituting slashes for backslashes"
  (unless (equal system-type 'windows-nt)
    (error "This command is not available on this system"))
  (w32-shell-execute "Open" (subst-char-in-string ?/ ?\\ (expand-file-name FILE))))

(defalias 'eshell/op 'eshell/open )

(setq eshell-prompt-function
      (lambda ()
        (concat
         (unless (= eshell-last-command-status 0)
           (format "[Last command exited with status %d]\n" eshell-last-command-status))
         (eshell/pwd)
         (if (= (user-uid) 0) " # " " $ "))))

(defun eshell-setup-keys ()
  "*Keybindings for Eshell."
  (defkey eshell-mode-map "<up>"   'eshell-previous-input)
  (defkey eshell-mode-map "<down>" 'eshell-next-input)
  (defkey eshell-mode-map "M-." 'eshell-insert-last-word)
  (defkey eshell-mode-map "C-M-_" 'eshell-copy-prev-shell-word)
  (defkey eshell-mode-map "C-a" 'eshell-bol)
  (defkey eshell-mode-map "C-x C-v" 'view-file)
  (defkey eshell-mode-map "C-x C-d" 'eshell-dired-pwd)
  (defkey eshell-command-map "C-a" 'beginning-of-line)
  ;; The following commands come from "em-zle"
  (defkey eshell-mode-map "M-." 'eshell-zle-insert-last-word)
  (defkey eshell-mode-map "C-M-_" 'eshell-zle-copy-prev-shell-word)
  (defkey eshell-mode-map "M-q" 'eshell-zle-push-line)
  (defkey eshell-mode-map "M-g" 'eshell-zle-get-line)
  (defkey eshell-mode-map "M-a" 'eshell-zle-accept-and-hold)
  (defkey eshell-command-map "M-?" 'eshell-zle-which-command))
(add-hook 'eshell-mode-hook 'eshell-setup-keys)

(defun iswitchb-only-eshell-buffers (buffer)
  "*Ignore all buffers not in EShell-mode."
  (not (major-mode-matches buffer "\\`EShell\\'")))

(defun iswitchb-eshell-buffers ()
  "*Switch to an EShell buffer."
  (interactive)
  (let ((iswitchb-buffer-ignore '(iswitchb-only-eshell-buffers)))
    (call-interactively 'iswitchb-buffer)))
(global-defkey "C-x S" 'iswitchb-eshell-buffers)




(provide 'eshell_rc)

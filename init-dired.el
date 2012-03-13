;; init-dired: Dired initialization

(setq dired-x-hands-off-my-keys t)
(require 'dired-x)

(require 'sorter nil t)

(when (require 'dired-details nil t)
  (setq dired-details-initially-hide nil)
  (dired-details-install))

(setq dired-listing-switches "-lFGht")
(setq dired-ls-F-marks-symlinks t)
(setq dired-guess-shell-gnutar "tar")
(setq dired-bind-jump nil)
(setq dired-no-confirm
      '(byte-compile chgrp chmod chown compress delete hardlink load shell symlink uncompress))
(setq dired-deletion-confirmer 'y-or-n-p)
(setq dired-recursive-deletes 'top)
(setq dired-recursive-copies 'always)
(setq dired-dwim-target t)
(setq dired-find-subdir t)
;; (setq dired-omit-files-p t)
(setq dired-omit-extensions (delete ".pdf" dired-omit-extensions))
;; (setq dired-enable-local-variables 'ask)
(setq dired-enable-local-variables t)
(setq dired-guess-shell-gzip-quiet t)
;; (setq dired-guess-shell-alist-user '(("\\.e?ps$" "gv" "lpr")
;;                                      ("\\.pdf$" "gv" "acroread" "xpdf")
;;                                      ("\\.\\(jpe?g\\|gif\\|png\\)$" "xli -quiet" "display")))
(setq dired-guess-shell-alist-user `(
                                     ,(if (equal system-type 'windows-nt)
                                          '("\\.e?ps$" 
                                           "e:/tools/GSTools/GSView/2.4/gsview32.exe"
                                           "cat * > //eepmuc02/pm193000"
                                           "cat * > //eepmuc01/pm193001"
                                           "c:/Win32App/Postscript/Ghostscript/7.0/gs7.00/bin/gswin32.exe -q -dBATCH")
                                        '("\\.e?ps$" "gv" "lpr"))
                                     ("\\.pdf$" "gv" "acroread" "xpdf")
                                     ("\\.\\(jpe?g\\|gif\\|png\\)$" "xli -quiet" "display")))


(set-face-foreground 'dired-ignored "gray60")
(copy-face 'dired-mark 'dired-marked)
(set-face-bold-p 'dired-marked t)
(set-face-inverse-video-p 'dired-marked t)
(set-face-inverse-video-p 'dired-flagged t)
(setq dired-perm-write-face font-lock-warning-face)

(when (fboundp 'turn-on-gnus-dired-mode)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))

(defun dired-w32-open-files (&optional arg files)
  "*Open FILES using the applications registered to handle them.
Return an error message if there is no application registered to handle the files.
This command simply calls `w32-shell-execute' and only works on Window systems."
  (interactive (list current-prefix-arg (dired-get-marked-files nil current-prefix-arg)))
  (unless (fboundp 'w32-shell-execute)
    (error "This command is not available on this system."))
  (mapc (lambda (fn)
          (w32-shell-execute "Open" (subst-char-in-string ?/ ?\\ (expand-file-name fn))))
        files))

(defun dired-do-deletions-and-exit ()
  "*Exit Dired, deleting the files flagged for deletion."
  (interactive)
  (dired-do-flagged-delete t)
  (kill-this-buffer))

(defun dired-copy-dirname-as-kill ()
  "*Copy the name of current directory into the kill ring."
  (interactive)
  (save-excursion
    (dired-prev-subdir 0)
    (dired-copy-filename-as-kill)))

(defadvice dired-copy-filename-as-kill (before prefix-arg-default-0 activate)
  "*Invert the sense of prefix arg.
With no prefix arg, copy the complete pathname of each marked file.
With a zero prefix arg, use just the filenames."
  (if (interactive-p)
      (cond
       ((not current-prefix-arg) (ad-set-arg 0 0))
       ((and (numberp current-prefix-arg) (= current-prefix-arg 0)) (ad-set-arg 0 nil)))))


;; (defun dired-do-kill-current-subdir ()
;;   "*Remove subdirectory listing from the Dired buffer."
;;   (interactive)
;;   (let ((dir (dired-current-directory)))
;;     (dired-prev-subdir 0)
;;     (dired-do-kill-lines 1)
;;     (or (dired-goto-file dir)
;;         (and (dired-prev-subdir 0)
;;              (dired-goto-next-file)))))

(defun dired-do-kill-current-subdir ()
  "*Remove subdirectory listing from the Dired buffer."
  (interactive)
  (let ((dir (dired-current-directory)))
    (dired-kill-subdir)
    (dired-prev-subdir 0)
    (or (dired-goto-file dir)
        (and (dired-prev-subdir 0)
             (dired-goto-next-file)))))

(defun dired-goto-first-file-subdir (arg)
  "*Position point on first file in this subdir.
With prefix ARG go to first nontrivial file.
Leave mark at previous position." 
  (interactive "P")
  (let (point-save)
    (dired-move-to-filename)
    (setq point-save (point))
    (dired-prev-subdir 0)
    (if arg
        (dired-goto-next-nontrivial-file)
      (dired-goto-next-file))
    ;; only set the mark if point moved
    (unless (= point-save (point))
      (set-mark point-save))))


;; ;; adapted from `dired-flag-backup-files'
;; (defun dired-flag-orphan-backup-files ()
;;   "*Flag all orphan backup files for deletion."
;;   (interactive)
;;   (let ((dired-marker-char dired-del-marker))
;;     (dired-mark-if
;;      ;; Don't call backup-file-name-p unless the last character looks like
;;      ;; it might be the end of a backup file name.  This isn't very general,
;;      ;; but it's the only way this runs fast enough.
;;      (and (save-excursion (end-of-line)
;; 			  ;; Handle executables in case of -F option.
;; 			  ;; We need not worry about the other kinds
;; 			  ;; of markings that -F makes, since they won't
;; 			  ;; appear on real backup files.
;; 			  (if (eq (preceding-char) ?*)
;; 			      (forward-char -1))
;; 			  (eq (preceding-char) ?~))
;; 	  (not (looking-at dired-re-dir))
;; 	  (let ((fn (dired-get-filename t t)))
;;             (and fn 
;;                  (backup-file-name-p fn)
;;                  (not (file-exists-p (file-name-sans-versions fn))))))
;;      "backup file")))

;; ACHTUNG! OVERLOADS ORIGINAL DIRED FUNCTION!!!
;; adapted from `dired-flag-backup-files'
(defun dired-flag-backup-files (&optional orphans-only)
  "*Flag all backup files for deletion.
With prefix argument, flag only orphaned backup files."
  (interactive "P")
  (let ((dired-marker-char dired-del-marker))
    (dired-mark-if
     ;; Don't call backup-file-name-p unless the last character looks like
     ;; it might be the end of a backup file name.  This isn't very general,
     ;; but it's the only way this runs fast enough.
     (and (save-excursion (end-of-line)
			  ;; Handle executables in case of -F option.
			  ;; We need not worry about the other kinds
			  ;; of markings that -F makes, since they won't
			  ;; appear on real backup files.
			  (if (eq (preceding-char) ?*)
			      (forward-char -1))
			  (eq (preceding-char) ?~))
	  (not (looking-at dired-re-dir))
	  (let ((fn (dired-get-filename t t)))
            (and fn 
                 (backup-file-name-p fn)
                 (or (not orphans-only)
                     (not (file-exists-p (file-name-sans-versions fn)))))))
     "backup file")))

;; From dired.el:
;; (defun dired-goto-next-file ()
;;   (let ((max (1- (dired-subdir-max))))
;;     (while (and (not (dired-move-to-filename)) (< (point) max))
;;       (forward-line 1))))

(defun dired-next-file-intern (arg &optional move-line-func)
  (while (> arg 0)
    (or move-line-func (setq move-line-func 'next-line))
    (funcall move-line-func 1)
    (while (not (dired-move-to-filename))
      (funcall move-line-func 1))
    (setq arg (1- arg))))

(defun dired-next-file (&optional arg)
  "*Move down to next filename.
Optional prefix ARG says how move to next ARGth file; default is one."
  (interactive "p")
  (dired-next-file-intern arg))

(defun dired-previous-file (&optional arg)
  "*Move down to next filename.
Optional prefix ARG says how move to next ARGth file; default is one."
  (interactive "p")
  (dired-next-file-intern arg 'previous-line))

;; (defun dired-sort-directories-first ()
;;   ;; From http://www.emacswiki.org/cgi-bin/wiki.pl?DiredSortDirectoriesFirst
;;   ;; A nice hack, which unfortunately has its problems with included subdirs
;;   ;; and/or marked files.
;;   (save-excursion
;;     (let (buffer-read-only)
;;       (forward-line 2) ;; beyond dir. header  
;;       (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
;;   (set-buffer-modified-p nil))
;; (add-hook 'dired-after-readin-hook 'dired-sort-directories-first)

(defun dired-mark* (char arg)
  "*Mark the current (or next ARG) files with CHAR.
If on a subdir headerline, mark all its files except `.' and `..'."
  (interactive (list (read-char "Mark using: ")
                     current-prefix-arg))
  (let ((dired-marker-char char))
    (dired-mark arg)))

;; this key is normally bound to `dired-mark', but since this command
;; is also avaliable on "m" use the binding for the "extended" version.
(defkey dired-mode-map "* m" 'dired-mark*)

(defun dired-mark-region-files (beg end &optional char)
  "*Mark all files in the current region.
Interactively, when called with a prefix arg, ask for the mark character to use."
  (interactive "r\nP")
  ;; dired-mark-files-in-region assumes `beg' at beginning of line
  (when char
    (setq char (read-char "Mark region using: ")))
  (let ((dired-marker-char (or char dired-marker-char)))
    (dired-mark-files-in-region (save-excursion
                                  (goto-char beg)
                                  (point-at-bol))
                                end))
  (dired-move-to-filename))

(defkey dired-mode-map "* w" 'dired-mark-region-files)

(defun dired-flag-marked-files  ()
  "*Flag marked files for deletion.
In other words: change all instances of `dired-marker-char' to `dired-del-marker'."
  (interactive)
  (dired-change-marks dired-marker-char dired-del-marker))

(defkey dired-mode-map "* d" 'dired-flag-marked-files)

;; http://www.masteringemacs.org/articles/2011/03/25/working-multiple-files-dired/
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

(require 'ls-lisp)
(setq ls-lisp-emulation nil)            ; means GNU
(setq ls-lisp-dirs-first t)
(setq ls-lisp-verbosity nil)
(setq ls-lisp-ignore-case nil)

(setq dired-isearch-filenames 'dwim)

(defkey dired-mode-map "n" 'dired-next-file)
(defkey dired-mode-map "p" 'dired-previous-file)
(defkey dired-mode-map "C-n" 'dired-next-file)
(defkey dired-mode-map "C-p" 'dired-previous-file)
(defkey dired-mode-map "<down>" 'dired-next-file)
(defkey dired-mode-map "<up>" 'dired-previous-file)

(defkey dired-mode-map "N" 'dired-next-subdir)
(defkey dired-mode-map "P" 'dired-prev-subdir)

(defkey dired-mode-map "X" 'dired-w32-open-files)

(defkey dired-mode-map "," 'dired-goto-first-file-subdir)
(defkey dired-mode-map "W" 'dired-copy-dirname-as-kill)
(defkey dired-mode-map "K" 'dired-do-kill-current-subdir)
(defkey dired-mode-map "q" 'dired-do-deletions-and-exit)
(defkey dired-mode-map "s" 'dired-sort) ; from `sorter'
(defkey dired-mode-map "a" 'dired-toggle-hidden) ; from `sorter'
(defkey dired-mode-map "RET" 'dired-view-file)
;; (defkey dired-mode-map "RET" 'dired-find-file-read-only)
(defkey dired-mode-map "v" 'dired-find-file-read-only)
(defkey dired-mode-map "M-RET" 'dired-find-alternate-file)

(defun dired-find-file-read-only ()
  "In Dired, visit the file or directory named on this line."
  (interactive)
  ;; Bind `find-file-run-dired' so that the command works on directories
  ;; too, independent of the user's setting.
  (let ((find-file-run-dired t))
    (find-file-read-only (dired-get-file-for-visit))))

(require 'dired-rename-commands)
(defkey dired-mode-map "% r" 'dired-ren-command)
(defkey dired-mode-map "% c" 'dired-cop-command)

(defun iswitchb-only-dired-buffers (buffer)
  "*Ignore all buffers not in dired-mode."
  (not (major-mode-matches buffer "\\`Dired\\>")))

(defun iswitchb-dired-buffers ()
  "*Switch to a Dired buffer."
  (interactive)
  (let ((iswitchb-buffer-ignore '(iswitchb-only-dired-buffers)))
    (call-interactively 'iswitchb-buffer)))
(global-defkey "C-x D" 'iswitchb-dired-buffers)

;; adapted from `dired-diff'
(defun dired-ediff (file &optional switches)
  "*Compare file at point with file FILE using `ediff'.
FILE defaults to the file at the mark.  (That's the mark set by
\\[set-mark-command], not by Dired's \\[dired-mark] command.)
The prompted-for file is the first file given to `diff'.
With prefix arg, prompt for second argument SWITCHES,
 which is options for `diff'."
  (interactive
   (let ((default (if (mark t)
		      (save-excursion (goto-char (mark t))
				      (dired-get-filename t t)))))
     (require 'ediff)
     (list (read-file-name (format "ediff %s with: %s"
				   (dired-get-filename t)
				   (if default
				       (concat "(default " default ") ")
				     ""))
			   (dired-current-directory) default t)
	   (if current-prefix-arg
	       (read-string "Options for ediff: " ediff-diff-options)))))
  (unless switches
    (setq switches ediff-diff-options))
  (let ((ediff-diff-options switches))
    (ediff file (dired-get-filename t))))
(defkey dired-mode-map "=" 'dired-ediff)


(defun dired-up-directory-substitute ()
  "*Substitute the current Dired buffer with the contents of parent directory."
  (interactive)
  (set-buffer-modified-p nil)
  (find-alternate-file (file-name-directory (directory-file-name (dired-current-directory)))))
(defkey dired-mode-map "U" 'dired-up-directory-substitute)

;; Thi and Klaus Berndl. The following advice also works with
;; `dired-advertised-find-file'.
;; (defadvice dired-view-file (around dired-subst-directory activate)
;;   "Replace current buffer if file is a directory."
;;   (interactive)
;;   (let* ((orig (current-buffer))
;;          (filename (dired-get-filename)))
;;     ad-do-it
;;     (when (and (file-directory-p filename) (not (eq (current-buffer) orig)))
;;       (kill-buffer orig))))

(provide 'init-dired)

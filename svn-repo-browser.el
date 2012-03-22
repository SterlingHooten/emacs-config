(defvar srb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'srb-next-file)
    (define-key map "p" 'srb-previous-file)
    (define-key map "q" 'bury-buffer)
    (define-key map (kbd "<return>") 'srb-visit)
    map)
  "Keymap for `srb-mode'.")

(define-derived-mode srb-mode nil "SVN Repo"
  "Major mode for browsing SVN repositories.
\\{srb-mode-map}"
  (make-local-variable 'svn-repo-browser-target)
  (goto-char (point-min))
  (srb-move-to-filename))

(defun svn-repo-browser (target)
  (interactive "starget: ")
  (let ((buffer (generate-new-buffer "*SVN repo browser*")))
    (switch-to-buffer buffer)
    (call-process "svn" nil t t "--non-interactive" "ls" "--verbose" target)
    (srb-mode)
    (setq svn-repo-browser-target target)
    (setq buffer-read-only t)
    ))

(defun srb-visit ()
  (interactive)
  (let ((url (concat svn-repo-browser-target "/" (srb-get-filename))))
    (if (srb-looking-at-directory)
        (svn-repo-browser url)
      (srb-find-file url))))

(defun srb-find-file (url)
  (let ((buffer (generate-new-buffer (file-name-nondirectory url))))
    (with-current-buffer buffer
      (call-process "svn" nil t t "--non-interactive" "cat" url)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (normal-mode)
      (rename-buffer (format "*%s*" (buffer-name)) 'unique))
    (switch-to-buffer buffer)))

(defun srb-next-file (arg)
  (interactive "p")
  (forward-line arg)
  (srb-move-to-filename))

(defun srb-previous-file (arg)
  (interactive "p")
  (forward-line (- arg))
  (srb-move-to-filename))

(defun srb-get-filename ()
  (srb-move-to-filename)
  (buffer-substring-no-properties (point) (point-at-eol)))

(defun srb-looking-at-directory ()
  (string-match-p "/$" (srb-get-filename)))

(defun srb-move-to-filename ()
  (let ((date-regexp "\\s-[ 0-3][0-9]\\s-+\\([0-2][0-9][:.][0-5][0-9]\\|[0-9]\\{4\\}\\) ")
        (eol (line-end-position)))
    (cond
     ((looking-back date-regexp)
      t)
     ((re-search-forward date-regexp eol t)
      (goto-char (match-end 0)))
     (t
      (error "No file on this line")))))

;; (defun srb-run-svn (args)
;;   (let ((buffer (get-buffer-create " *svn-repo-browser*")))
;;     (with-current-buffer buffer
;;       (erase-buffer)
;;       (if (apply call-process "svn" nil t t "--non-interactive" args)
;;           ...
;;         (progn
;;           (display-buffer buffer)
;;           (error "Subversion command failed")))))
;; )

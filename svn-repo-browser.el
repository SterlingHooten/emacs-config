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
  (svn-repo-browser (concat svn-repo-browser-target "/" (srb-get-filename))))

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

(defun srb-move-to-filename ()
  (let ((HH:MM "[ 0-2][0-9][:.][0-5][0-9] ")
        (eol (line-end-position)))
    (cond
     ((looking-back HH:MM)
      t)
     ((re-search-forward HH:MM eol t)
      (goto-char (match-end 0)))
     (t
      (error "No file on this line")))))

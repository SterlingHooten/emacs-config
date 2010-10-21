;;; pwdsync.el
;;
;; Usage:
;;
;; In your ~/.emacs:
;;
;;    (add-hook 'comint-output-filter-functions 'pwdsync-filter 'append)
;;
;; Then convince your shell to output an escape sequence containing
;; the current working directory.  Example for Bash:
;;
;;    prompt_command() {
;;        test -n "$INSIDE_EMACS" && echo -en "\e|CWD:$PWD|"
;;    }
;;
;;    PROMPT_COMMAND=prompt_command

(defvar pwdsync-escape-sequence-regexp "\e|CWD:\\(.+\\)|")

;; (defvar pwdsync-shell-buffer-name-format "*shell: %s*")

(defvar pwdsync-shell-buffer-name-format
  (lambda (dir)
    (format "*shell: %s*"
            (if (string-match (format "^%s/?$" (regexp-quote (expand-file-name "~"))) (expand-file-name dir))
                "~"
              (file-name-nondirectory (directory-file-name dir))))))

(defun pwdsync-filter (string)
  (save-match-data
    (when (string-match pwdsync-escape-sequence-regexp string)
      (let ((pmark (process-mark (get-buffer-process (current-buffer))))
            (cwd))
	(save-excursion
	  (save-restriction
	    (widen)
	    (let ((inhibit-field-text-motion t)
		  (buffer-read-only nil))
	      (goto-char pmark)
              (when (re-search-backward pwdsync-escape-sequence-regexp comint-last-output-start 'noerror)
                (setq cwd (match-string-no-properties 1))
                (delete-region (match-beginning 0) (match-end 0))
                (shell-process-cd cwd)
                (when pwdsync-shell-buffer-name-format
                  (rename-buffer (if (functionp pwdsync-shell-buffer-name-format)
                                     (funcall pwdsync-shell-buffer-name-format default-directory)
                                   (format pwdsync-shell-buffer-name-format (file-name-as-directory default-directory)))
                                 'unique))))))))))

(provide 'pwdsync)


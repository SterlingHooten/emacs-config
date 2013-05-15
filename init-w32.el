;;; System-dependent configuration

(setq user-login-name (downcase user-login-name))

(modify-coding-system-alist 'process "svn" '(latin-1 . latin-1))

(setq focus-follows-mouse nil)
(auto-raise-mode -1)

(setq w32-enable-synthesized-fonts nil)

(setq w32-alt-is-meta t)
(setq w32-pass-alt-to-system nil)

(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'hyper)
(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'hyper)

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

(setq w32-quote-process-args t)
(add-to-list 'process-coding-system-alist '("bash" . undecided-unix))
(add-to-list 'process-coding-system-alist '("zsh" . undecided-unix))
(add-to-list 'process-coding-system-alist '("svn" . undecided-dos))
(setq shell-file-name "bash")
(setq explicit-bash-args '("-i"))
(setenv "SHELL" shell-file-name)

(setq tramp-default-method "sshx")

(setq ffap-url-regexp nil)

(defun shell-cmd ()
  (interactive)
  (let ((explicit-shell-file-name (or (executable-find "cmdproxy.exe")
                                      (getenv "ComSpec")
                                      (executable-find "cmd.exe")
                                      "command.com"))
        (default-process-coding-system '(dos . dos))
        (comint-process-echoes t))
    (shell "*shell: cmd*")))

(defun shell-bash3 ()
  (interactive)
  (let ((explicit-shell-file-name "f:/Apps/PortableGit-1.7.4/bin/bash.exe"))
    (shell (generate-new-buffer-name "*shell*"))))

(provide 'init-w32)

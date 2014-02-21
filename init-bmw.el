(setq user-mail-address "Emilio.Lopes@partner.bmw.de")
(setq mail-default-reply-to "Emilio.Lopes@partner.bmw.de")

;; /sudo:eas254.muc:/etc/fstab
(add-to-list 'tramp-default-proxies-alist
             '("eas254\\.muc\\'" "\\`root\\'" "/sshx:eas254@%h:"))

(setq url-proxy-services '(("http"  . "proxy.muc:8080")
                           ("https" . "proxy.muc:8080")
                           ("ftp"   . "proxy.muc:8080")))

(add-hook 'text-mode-hook
          (lambda nil
            (let ((bname (buffer-file-name)))
              (when (and (stringp bname)
                         (string-match-p "/bmwmail\\." bname))
                (fix-broken-outlook-replies)))))

(defun fix-broken-outlook-replies ()
  (interactive "*")
  (goto-char (point-max))
  (delete-blank-lines)
  (goto-char (point-min))
  ;; (delete-blank-lines)

  (let ((sig-start (and (search-forward-regexp "^-- *$" nil t)
                        (progn (just-one-space)
                               (point-at-bol))))
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
          (insert "\n" sig))))))

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

(setq sql-oracle-program "c:/Oracle/Client/bin/sqlplus.exe")

(let ((cygwin-prefix "e:/tools/gnu"))
  (add-to-path 'exec-path (concat cygwin-prefix "/bin"))
  (add-to-path 'exec-path (concat cygwin-prefix "/usr/local/bin"))
  (add-to-path 'Info-default-directory-list (concat cygwin-prefix "/usr/info") 'append)
  (setq woman-manpath
        (mapcar (lambda (dir) (concat cygwin-prefix dir)) '("/usr/local/man" "/usr/man"))))

(mapc (lambda (dir)
        (when (file-accessible-directory-p dir)
          (add-to-path 'exec-path dir 'append)))
      '("f:/bin"
        "f:/Apps/PortableGit-1.9.0-preview20140217/bin"))

(setenv "PATH" (mapconcat (if running-nt
                              (lambda (dir)
                                (subst-char-in-string ?/ ?\\ dir))
                            'identity) exec-path path-separator))

(provide 'init-bmw)

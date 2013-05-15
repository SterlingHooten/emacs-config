(add-to-path 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(when (and (executable-find "mu")
           (require-soft 'mu4e))

  (setq mu4e-drafts-folder "/[Gmail].Entw&APw-rfe")
  (setq mu4e-sent-folder   "/[Gmail].Gesendet")
  (setq mu4e-trash-folder  "/[Gmail].Papierkorb")
  (setq mu4e-refile-folder "/[Gmail].Alle Nachrichten")

  (setq mu4e-sent-messages-behavior 'delete)

  (setq mu4e-headers-skip-duplicates t)

  (setq mu4e-maildir-shortcuts
        `(("/INBOX"                    . ?i)
          (,mu4e-sent-folder           . ?s)
          (,mu4e-trash-folder          . ?t)
          (,mu4e-refile-folder         . ?a)))

  (setq mu4e-bookmarks
        '(("flag:unread OR flag:flagged"      "Unread/flagged messages" ?u)
          ("date:today..now"                  "Today's messages"        ?t)
          ("date:7d..now"                     "Last 7 days"             ?w)))

  (add-to-list 'mu4e-view-actions
               '("view in browser" . mu4e-action-view-in-browser) t)

  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "offlineimap")

  (setq mu4e-html2text-command "html2text -utf8 -width 72")

  (setq mu4e-user-mail-address-list
        '("eclig@gmx.net"
          "emilio.lopes@gmx.net"
          "lopes.emilio@gmail.com"
          "emilio.lopes@partner.bmw.de"))

  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587))

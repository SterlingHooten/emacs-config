
(defvar bmw-default-database "bmsnf"
  "Default Continuus database used when creating tasks.")
;; TODO
;; Werte: nil -> use default in script
;;        string -> use value of string
;;        ask -> fragen!
;;        t   -> ?

(defun bmw-new-requirement ()
  "*Create a new requirement file and edit it."
  (interactive)
  (let ((find-file-existing-other-name t)
        (reqfile (replace-regexp-in-string
                  "[ \t\n\r]*\\'" ""
                  (shell-command-to-string "perl -S newreq.pl -c"))))
    (if (file-exists-p reqfile)
        (find-file reqfile)
      (error "Error creating requirement file"))))

(defun bmw-create-task (&optional arg)
  "*Create or modify a Continuus task from a requirement file."
  (interactive "P")
  (unless buffer-file-name 
    (error "Current buffer is not associated with a file"))
  (when (buffer-modified-p)
    (if (y-or-n-p "Save current buffer now? ")
        (save-buffer)
      (error "You must save the current buffer before proceeding")))
  (save-excursion
    (sgml-parse-prolog)
    (when (or sgml-dtd-less (not (equal (sgml-dtd-doctype sgml-dtd-info) "REQUIREMENTS")))
      (error "Not a requirement file")))
  (let ((db bmw-default-database)
        tn)
    (setq db (read-string (format "Use database [%s]: " bmw-default-database) nil nil bmw-default-database))
    (when arg
      (setq tn (read-string "Number of the task to be modified: ")))
;; TODO use `apply' here!
;; (funcall 'call-process "echo" nil nil nil "foo" (if t "" "b"))
;; ODER (noch besser!)
;;(call-process shell-file-name nil nil nil shell-command-switch "perl -S ...")
    (if tn 
        (call-process "perl" nil nil nil
                      "-S" "createtask.pl" "-DB" db
                      "-TN" tn
                      "-TD" buffer-file-name)

      (call-process "perl" nil nil nil
                    "-S" "createtask.pl" "-DB" db
                    "-TD" buffer-file-name)
      )))

;;; pcomplete-bash.el --- pcomplete completion using BASH's `compgen'

;; Copyright (C) 2012  Emilio C. Lopes

;; Author: Emilio C. Lopes <eclig@gmx.net>
;; Keywords: processes, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Caveats:
;;   * `shell-prompt-pattern' should be set to a sensible value
;;   * Bash option `extglob' should be set ("shopt -s extglob")

;;; Code:

(defcustom pcmpl-bash-file-ignore "~\\'"
  (documentation-property 'pcomplete-file-ignore
			  'variable-documentation)
  :type (get 'pcomplete-file-ignore 'custom-type))

(defcustom pcmpl-bash-dir-ignore nil ;"\\`\\(\\.\\.?\\|CVS\\)/\\'"
  (documentation-property 'pcomplete-dir-ignore
			  'variable-documentation)
  :type (get 'pcomplete-dir-ignore 'custom-type))

(defun pcmpl-bash-complete-command ()
  "Completion function for Bash command names.
Uses Bash's builtin `compgen' to get a list of possible commands."
  (let ((cmd (or (pcomplete-arg 'first) "")))
    (when (> (length cmd) 0)        ; do not complete an empty command
      (pcomplete-here* (pcomplete-uniqify-list (pcmpl-bash-command-completions cmd))))))

(defun pcmpl-bash-complete-filename ()
  "Completion function for file names.
Uses Bash's builtin `compgen' to get a list of completions."
  (let ((name (or (nth pcomplete-index pcomplete-args) "")))
    (pcomplete-here (pcmpl-bash-filter-map
                     (lambda (f)
                       (let ((fn (file-name-nondirectory f)))
                         (if (file-directory-p f)
                             (and (not (pcmpl-bash-ignore-p fn pcomplete-dir-ignore))
                                  (concat fn "/"))
                           (and (not (pcmpl-bash-ignore-p fn pcomplete-file-ignore))
                                fn))))
                     (pcmpl-bash-file-completions name))
                    (file-name-nondirectory name))))

(defun pcmpl-bash-command-name ()
  (let ((cmd (file-name-nondirectory (pcomplete-arg 'first))))
    (if (memq system-type '(ms-dos windows-nt cygwin))
	(file-name-sans-extension cmd)
      cmd)))

(defun pcmpl-bash-default-completion-function ()
  (while (pcmpl-bash-complete-filename)))

;; See commentary in eshell/em-cmpl.el for ideas!

;; (defun pcmpl-bash-environment-variable-completion ()
;;   "Completion data for an environment variable at point, if any."
;;   (let ((var (nth pcomplete-index pcomplete-args)))
;;     (when (and (not (zerop (length var))) (eq (aref var 0) ?$))
;;       (pcomplete-here* (pcomplete-uniqify-list (comint-redirect-results-list (format "compgen -P \\$ -v %s" (substring var 1)) "^\\(.+\\)$" 1))))))

(defun pcmpl-bash-command-completions (cmd)
  (pcmpl-bash-run-collecting-output
   (format (if (memq system-type '(ms-dos windows-nt cygwin)) 
               "compgen -X '*.@(dll|ime)' -c '%s'"
             "compgen -c '%s'")
           cmd)))

(defun pcmpl-bash-file-completions (name)
  ;; TODO
  ;; echo ~-/d* => compgen -f -X '!~-/d*' '~-/'
  ;; echo ~-/*.cmd => compgen -f -X '!~-/*.cmd' '~-/'
  (pcmpl-bash-run-collecting-output (format "compgen -f '%s'" name)))

(defun pcmpl-bash-run-collecting-output (cmd)
  (comint-redirect-results-list (format " %s" cmd) "^\\(.+\\)$" 1))

(defun pcmpl-bash-expand-and-send-input ()
  (interactive)
  (let* ((pmark (process-mark (get-buffer-process (current-buffer))))
         (input (if (>= (point) (marker-position pmark))
                    (progn (if comint-eol-on-send (end-of-line))
                           (buffer-substring pmark (point)))
                  (let ((copy (funcall comint-get-old-input)))
                    (goto-char pmark)
                    (insert copy)
                    copy))))
    (when (or t (eq comint-input-autoexpand 'history))
      (let ((expanded (car (pcmpl-bash-history-expansion input))))
        (unless (string= input expanded)
          (delete-region pmark (point))
          (insert expanded))))
    (comint-send-input)))

;; "history -p '!18337 '==>' !35'"
;; (insert (replace-regexp-in-string "[^-0-9a-zA-Z_./\n]" "\\\\\\&" "history -p '!18337 '==>' !35'"))

(defun pcmpl-bash-history-expansion (input)
  (pcmpl-bash-run-collecting-output (format "history -p '%s'" input)))

(defun pcmpl-bash-setup ()
  (set (make-local-variable 'pcomplete-file-ignore) pcmpl-bash-file-ignore)
  (set (make-local-variable 'pcomplete-dir-ignore) pcmpl-bash-dir-ignore)
  (set (make-local-variable 'pcomplete-command-completion-function) #'pcmpl-bash-complete-command)
  (set (make-local-variable 'pcomplete-command-name-function) #'pcmpl-bash-command-name)
  (set (make-local-variable 'pcomplete-default-completion-function) #'pcmpl-bash-default-completion-function))

(defmacro pcmpl-bash-filter-map (f list)
  (let ((result (make-symbol "result")))
    `(let ((,result '()))
       (dolist (x ,list ,result)
         (let ((fx (funcall ,f x)))
           (when fx
             (setq ,result (cons fx ,result)))))
       (nreverse ,result))))

(defun pcmpl-bash-ignore-p (x regexp)
  (and regexp (string-match-p regexp x)))

(provide 'pcomplete-bash)
;;; pcomplete-bash.el ends here

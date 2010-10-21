;;; s48.el --- A major mode for Scheme48 development

;; Copyright (C) 1992  Jonathan Rees
;; Copyright (C) 2005  Jorgen Schaefer
;; Copyright (C) 2006  Emilio Lopes

;; Author: Jonathan Rees (cmuscheme48.el)
;;         Jorgen Schaefer <forcer@forcix.cx> (scheme48-mode)
;;         Emilio Lopes <eclig@gmx.net> (s48)

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the authors may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; This file provides `s48-mode', a major mode for improved
;; interaction with Scheme48 and Scsh. It's the same as the canonical
;; `scheme-mode', but provides some commands which tell Scheme48 from
;; which a specific definition came from. This allows Scheme48 to put
;; the definition in the correct package by itself.

;; This is based om the cmuscheme48.el which comes with Scheme48.

;; You can set a buffer-local variable named `scheme48-package' or
;; even only `package' to send definitions to that package. This can
;; be done by file variables, so the following works:

;;   -*- mode: s48; scheme48-package: mypackage -*-

;; To use the special packages CONFIG, USER and EXEC, use the package
;; name in parens, like this:

;;   -*- mode: s48; scheme48-package: (exec) -*-



;;; Code:

(require 'cmuscheme)
(require 'scheme)

(defcustom s48-keywords
  '(;; R5RS
    (dynamic-wind 0)

    ;; Scheme48
    (destructure 1)
    (enum-case 2)
    (environment-define! 2 no-font-lock)
    (environment-set! 2 no-font-lock)
    (guard 1)
    (iterate 3)
    (make-usual-resumer 2 no-font-lock)
    (mvlet 1)
    (mvlet* 1)
    (search-tree-modify! 2 no-font-lock)
    (usual-resumer 0 no-font-lock)
    (with-exception-handler 1)
    (with-handler 1)
    (with-interaction-environment 1)
    (with-nondeterminism 0)

    ;; I/O-related
    (call-with-current-input-port 1)
    (call-with-current-noise-port 1)
    (call-with-current-output-port 1)
    (call-with-string-output-port 0)
    (limit-output 2 no-font-lock)
    (recurring-write 2 no-font-lock)
    (silently 0)
    (with-current-ports 3)

    ;; Configuration language
    (define-interface 1)
    (define-structure 2)
    (structure 1)
    (structures 1)
    ;; These don't improve (for some, even degrade) the readability.
    ;; (modify 1 no-font-lock)
    ;; (subset 1 no-font-lock)

    ;; Concurrency-related
    (atomically 0)
    (atomically! 0)
    (call-ensuring-atomicity 0)
    (call-ensuring-atomicity! 0)
    (ensure-atomicity 0)
    (ensure-atomicity! 0)
    (interrupt-thread 1 no-font-lock)
    (let-fluid 2)
    (let-fluids defun)
    (spawn-on-scheduler 1 no-font-lock)
    (with-new-proposal 1)

    ;; SCSH
    (with-current-input-port 2)
    (with-current-output-port 2)
    (awk 3)
    (close-after 2 no-font-lock)
    (if-match 2)
    (with-cwd 1)
    (with-cwd* 1)

    ;; Others
    (let-optionals scheme-let-indent)
    (let-optionals* scheme-let-indent)

    ;; SRFI-2
    (and-let* 1)

    ;; SRFI-8
    (receive 2)

    ;; SRFI-11
    (let-values 1)
    (let*-values 1)
    )
  "A list of Scheme48-related keywords.
The list consists of lists of the form (KEYWORD INDENT [NO-FONT-LOCK].
The keywords named KEYWORD will be indented according to INDENT,
and will also be highlighted as keywords unless NO-FONT-LOCK is
non-nil."
  :group 'scheme
  :type '(repeat (list symbol sexp boolean)))

(defvar scheme48-package nil
  "The name of the package definitions from this file should go to.")
(make-variable-buffer-local 'scheme48-package)

(defvar s48-alternative-package '(config)
  "Alternative package to send definitions from this file to.")

(defvar s48-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\C-x" 's48-send-definition) ;gnu convention
    (define-key map "\C-x\C-e" 's48-send-last-sexp) ;gnu convention
    (define-key map "\C-c\C-e" 's48-send-definition)
    (define-key map "\C-c\M-e" 's48-send-definition-and-go)
    (define-key map "\C-c\C-r" 's48-send-region)
    (define-key map "\C-c\M-r" 's48-send-region-and-go)
    (define-key map "\C-c\C-l" 's48-load-file)
    map)
  "The keymap used in `s48-mode'.")

(define-derived-mode s48-mode scheme-mode "S48"
  "Major mode for improved Scheme48 interaction.
This mode is derived from `scheme-mode', so see there for
information.

The commands that send code to the Scheme48 process attach
information as to from which file the code comes from. This
allows Scheme48 to put the corresponding definitions in the
package associated with that file name.

\\{s48-mode-map}"
  (s48-initialize))

(defvar s48-mode-initialized-p nil
  "This is non-nil when `s48-mode' has been initialized.
Set it to nil if you want the next invocation of `s48-mode'
to re-read `s48-keywords'.")

(defun s48-initialize ()
  "Initialize `s48-mode' from `s48-keywords'.
Only run when `s48-mode-initialized-p' is nil.
This is done so that the user can modify `s48-keywords'
before the first time the mode is run, but after this package has
been loaded."
  (unless s48-mode-initialized-p
    (mapc (lambda (entry)
            (put (car entry)
                 'scheme-indent-function
                 (cadr entry))
            (put (intern (upcase (symbol-name (car entry))))
                 'scheme-indent-function
                 (cadr entry)))
          s48-keywords)
    (let ((regexp (concat "("
                          (regexp-opt
                           (delete nil
                                   (mapcar (lambda (elt)
                                             (if (nth 2 elt)
                                                 nil
                                               (symbol-name (car elt))))
                                           s48-keywords))
                           t)
                          "\\>")))
      (font-lock-add-keywords 's48-mode
                              (list (list regexp 1 'font-lock-keyword-face))))
    (setq s48-mode-initialized-p t)))

(defun s48-send-region (start end &optional pack)
  "Send the current region to the inferior Scheme48 process."
  (interactive "r\nP")
  (cond
   (pack
    (s48-send-with-prefix (s48-package-sender s48-alternative-package)
                               start
                               end))
   (scheme48-package
    (s48-send-with-prefix (s48-package-sender scheme48-package)
                               start
                               end))
   (t
    (comint-send-region (scheme-proc) start end)
    (comint-send-string (scheme-proc) "\n"))))

(defun s48-send-with-prefix (prefix start end)
  "Send all Scheme definitions in the region to the Scheme process."
  (let ((region (buffer-substring-no-properties start end))
        (p prefix)) ; Emacs lossage - prefix is suddenly nil below
    (with-temp-buffer
      (insert region "\n")
      ;; `backward-sexp' relies on this. Thanks to Riastradh for
      ;; finding it out :-)
      (set-syntax-table scheme-mode-syntax-table)
      (set (make-local-variable 'parse-sexp-ignore-comments) t)
      ;; Add prefix
      (while (> (point)
                (progn (backward-sexp)
                       (point)))
        (save-excursion
          (insert "\n" p " ")))
      ;;(message (buffer-substring-no-properties (point-min) (point-max)))
      (comint-send-region (scheme-proc)
                          (point-min)
                          (point-max)))))

(defun s48-send-definition (&optional pack)
  "Send the current definition to the inferior Scheme48 process."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (s48-send-region (point) end pack))))

(defun s48-send-last-sexp (&optional pack)
  "Send the previous sexp to the inferior Scheme process."
  (interactive "P")
  (s48-send-region (save-excursion (backward-sexp) (point)) (point) pack))

(defun s48-send-region-and-go (start end &optional pack)
  "Send the current region to the inferior Scheme48 process,
and switch to the process buffer."
  (interactive "r\nP")
  (s48-send-region start end pack)
  (switch-to-scheme t))

(defun s48-send-definition-and-go (&optional pack)
  "Send the current definition to the inferior Scheme48,
and switch to the process buffer."
  (interactive "P")
  (s48-send-definition pack)
  (switch-to-scheme t))

(defun s48-load-file (file-name &optional pack)
  "Load a Scheme file into the inferior Scheme48 process."
  (interactive (list (car (comint-get-source "Load Scheme48 file: "
                                             scheme-prev-l/c-dir/file
                                             scheme-source-modes t)) ; T because LOAD
                                        ; needs an exact name
                     (read-string "Load in package: " nil nil scheme48-package)))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                       (file-name-nondirectory file-name)))
  (comint-send-string (scheme-proc)
		      (concat ",load " file-name "\n")))

(defun s48-package-sender (package)
  "Return the prefix to send a definition to PACKAGE."
  (cond
   ((equal package '(config))
    ",config")
   ((equal package '(user))
    ",user")
   ((equal package '(exec))
    ",exec")
   (t
    (format ",in %s" package))))

(provide 's48)
;;; s48.el ends here

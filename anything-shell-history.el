;; (setq comint-prompt-regexp "~?\\(/+[^/#$%>\n]+\\)*\\$ ")

(defun shell-fetch-history ()
  "Return the shell's history as list of strings."
  (let ((cmd (format "\\history %s" 10000)))
    (comint-redirect-results-list cmd "^[0-9]+\\s-+\\(.+\\)$" 1)))

(defun anything-shell-fetch-history ()
  (with-current-buffer anything-current-buffer
    (shell-fetch-history)))

(setq anything-c-source-shell-history
      '((name . "History")
        (candidates . anything-shell-fetch-history)
        (action . anything-shell-history-insert-command)
        (type . string)))

(defun anything-shell-history-insert-command (str)
  (with-current-buffer anything-current-buffer
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (insert str)))

(defun comint-anything-shell-history ()
  (interactive)
  (anything
   :prompt "History item matching: "
   :candidate-number-limit nil
   :sources
   '(anything-c-source-shell-history)))

(defun anything-shell-input-ring ()
  (with-current-buffer anything-current-buffer
    (ring-elements comint-input-ring)))

(setq anything-c-source-shell-input-ring
      '((name . "History")
        (candidates . anything-shell-input-ring)
        (action . anything-shell-history-insert-command)
        (type . string)))

(defun comint-anything-input-ring ()
  (interactive)
  (anything
   :prompt "History item matching: "
   :candidate-number-limit nil
   :sources
   '(anything-c-source-shell-input-ring)))

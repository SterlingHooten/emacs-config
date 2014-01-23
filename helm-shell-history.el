(require 'helm)

(defun helm-shell-history-insert-command (str)
  (with-current-buffer helm-current-buffer
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (insert str)))

(defun helm-shell-input-ring ()
  (with-current-buffer helm-current-buffer
    (ring-elements comint-input-ring)))

(setq helm-source-shell-input-ring
      '((name . "History")
        (candidates . helm-shell-input-ring)
        (action . helm-shell-history-insert-command)
        (type . string)))

(defun comint-helm-input-ring ()
  (interactive)
  (helm
   :prompt "History item matching: "
   :candidate-number-limit nil
   :sources
   '(helm-source-shell-input-ring)))

(provide 'helm-shell-history)

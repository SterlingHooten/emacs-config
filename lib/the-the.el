;; Adapted from "Programming in Emacs Lisp. An Introduction",
;; by Robert J. Chassell, avaliable from GNU directory
;; (prep.ai.mit.edu:/pub/gnu) and mirrors.
(defun the-the ()
  "Search forward for for a duplicated word."
  (interactive)
  (message "Searching for for duplicated words ...")
  (push-mark (point) 'nomsg nil)
  ;; This regexp is not perfect
  ;; but is fairly good over all:
  (if (re-search-forward
       "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
      (message "Found duplicated word.")
    (message "End of buffer.")))

;; Bind `the-the' to C-c \
(global-set-key "\C-c\\" 'the-the)

(provide 'the-the)

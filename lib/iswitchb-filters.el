;; (listp (car '(a b c)))
;; (listp (car '("foo" (a b c))))
;; (listp (car '(("foo") (a b c))))
;; (listp (car '((a b c) (d e f) (g h i))))

(setq iswitchb-current-filter 0)
(setq iswitchb-filters
      `(
        ,iswitchb-buffer-ignore
        ("^ " "^\\*.*\\*")
        ("^ ")
;;        ("a" "c")
        ))
(setq iswitchb-buffer-ignore (nth iswitchb-current-filter iswitchb-filters))


(add-hook 'iswitchb-define-mode-map-hook
          (lambda ()
            (defkey iswitchb-mode-map "C-a" 'iswitchb-next-filter)) 'append)

;; (defun iswitchb-next-filter (&optional arg)
;;   "*Cycle through buffer filters.
;; If optional argument is non nil but not a number just toggle filtering.
;; If optional argument is a number, use filter indicated by that number.
;; If it is a negative number, list the defined filters."
;;   (interactive "P")
;;   (if arg
;;       (iswitchb-toggle-ignore)
;;     (when (and iswitchb-filters (listp (car-safe iswitchb-filters)))
;;       (setq iswitchb-current-filter (1+ iswitchb-current-filter))
;;       (when (> iswitchb-current-filter (1- (length iswitchb-filters)))
;;         (setq iswitchb-current-filter 0))
;;       (setq iswitchb-buffer-ignore (nth iswitchb-current-filter iswitchb-filters))
;; ;      (message "Filter %s: %s" iswitchb-current-filter (mapcar 'regexp-quote iswitchb-buffer-ignore))
;;       (message "Filter %s: %s" iswitchb-current-filter (princ iswitchb-buffer-ignore))
;;       ))
;;   (iswitchb-make-buflist iswitchb-default)
;;   ;; ask for list to be regenerated.
;;   (setq iswitchb-rescan t))

(defun iswitchb-next-filter (&optional arg)
  "*Cycle through buffer filters.
If optional argument is non nil but not a number just toggle filtering.
If optional argument is a number, use filter indicated by that number.
If it is a negative number, list the defined filters."
  (interactive "P")
  (if (and arg (not (numberp arg)))
      (iswitchb-toggle-ignore)
    (when (and iswitchb-filters (listp (car-safe iswitchb-filters)))
      (setq iswitchb-current-filter (if (numberp arg) arg (1+ iswitchb-current-filter)))
;;       (setq iswitchb-current-filter
;;             (if (numberp arg)
;;                 (min arg (length ...))
;;               (min ... (1+ iswitchb-current-filter))))
      ;; If at the end of the filter-list, wrap to first filter if
      ;; called without arg or use the last one otherwise
      (when (> iswitchb-current-filter (1- (length iswitchb-filters)))
        (setq iswitchb-current-filter (if (numberp arg) (1- (length iswitchb-filters)) 0)))
      (setq iswitchb-buffer-ignore (nth iswitchb-current-filter iswitchb-filters))
;      (message "Filter %s: %s" iswitchb-current-filter (mapcar 'regexp-quote iswitchb-buffer-ignore))
      (message "Filter %s: %s" iswitchb-current-filter (princ iswitchb-buffer-ignore))
      ))
  (iswitchb-make-buflist iswitchb-default)
  ;; ask for list to be regenerated.
  (setq iswitchb-rescan t))

;;(princ iswitchb-buffer-ignore)

(provide 'iswitchb-filters)

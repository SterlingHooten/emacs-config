(defvar dired-vc-state-mark-mapping
  '((up-to-date nil)
    (edited ?\m)
    (added ?\a)
    (removed ?\d)))

(defun dired-vc-mark-subdir-files ()
  (interactive)
  (let ((p-min (dired-subdir-min)))
    (dired-vc-mark-files-in-region p-min (dired-subdir-max)))
  )

;; dired-mark-files-in-region
(defun dired-vc-mark-files-in-region (start end)
  (let ((inhibit-read-only t))
    (if (> start end)
	(error "start > end"))
    (goto-char start)			; assumed at beginning of line
    (while (< (point) end)
      ;; Skip subdir line and following garbage like the `total' line:
      (while (and (< (point) end) (dired-between-files))
	(forward-line 1))
      (when (and (not (looking-at dired-re-dot))
                 (dired-get-filename nil t))
        (let* ((fn (dired-get-filename nil t))
               (state (vc-git-state fn))
               (marker (car (assoc-default state dired-vc-state-mark-mapping nil nil))))
          (when marker
           (delete-char 1)
           (insert marker))))
      (forward-line 1))))

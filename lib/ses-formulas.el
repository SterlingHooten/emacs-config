(require 'cl)

(defun worktime-time-printer (hhmm)
  (cond 
   ((null hhmm) "")
   ((integerp hhmm)
    (list (apply 'format (concat (if (< hhmm 0) "-" "")
                                 "%02d:%02d")
                 (worktime-decompose-hhmm (abs hhmm)))))
   (t (list (format "%s" hhmm)))))

(defun worktime-decompose-hhmm (hhmm)
  (let ((h (truncate hhmm 100))
        (m (% hhmm 100)))
    (list h (if (zerop h) m (abs m)))))

(defun worktime-hhmm-hours (hhmm)
  (car (worktime-decompose-hhmm hhmm)))

(defun worktime-hhmm-minutes (hhmm)
  (cadr (worktime-decompose-hhmm hhmm)))

;; (worktime-hhmm-minutes -30)

(defun worktime-hhmm-to-minutes (hhmm)
  (let ((hours (worktime-hhmm-hours hhmm))
        (minutes (worktime-hhmm-minutes hhmm)))
    (* (if (>= hours 0) 1 -1)
       (+ (* (abs hours) 60)
          minutes))))

(defun worktime-minutes-to-hhmm (minutes)
  ;; MINUTES *must* be positive!
  (let ((m (% minutes 60))
        (h (truncate minutes 60)))
    (+ (* h 100) m)))

(defun worktime-time-diff (hhmm2 hhmm1)
  (cond ((and (integerp hhmm1) (integerp hhmm2))
         (worktime-minutes-to-hhmm (- (worktime-hhmm-to-minutes hhmm2)
                                      (worktime-hhmm-to-minutes hhmm1))))
        (t nil)))
;; (worktime-time-diff 1800 1930)

(defun worktime-time-add (hhmm2 hhmm1)
  (cond ((and (integerp hhmm1) (integerp hhmm2))
         (worktime-minutes-to-hhmm (+ (worktime-hhmm-to-minutes hhmm2)
                                      (worktime-hhmm-to-minutes hhmm1))))
        (t nil)))

;; Convert HHMM to decimal
(defun worktime-hm2dec (time)
  (if (numberp time)
      (let ((h (% (/ time 100) 100))
            (m (% time 100)))
        (+ h (/ (%  m 60) 60.0)))
    nil))
;; (worktime-hm2dec 116)
;; Convert decimal to HHMM
(defun worktime-dec2hm (time)
  (if (numberp time)
      (let* ((h (truncate time))
             (m (* (- time h) 60)))
        (round (+ (* h 100) m)))
    nil))
;; (worktime-dec2hm 1.27)
;; (worktime-dec2hm 154.83)

(setq safe-functions '(worktime-time-printer
                       worktime-time-add
                       worktime-time-diff
                       worktime-hm2dec
                       worktime-dec2hm))

;; (worktime-hm2dec 859)
;; (worktime-hm2dec (worktime-time-diff (worktime-time-diff 1845 830) 45))

(provide 'ses-formulas)

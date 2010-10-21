;; creators and accessors
(defun worktime-time-p (time)
  (integerp time))

(defun worktime-make-time-from-hhmm (hhmm)
  (let ((h (truncate hhmm 100))
        (m (% hhmm 100)))
    (+ (* 60 h) m)))

(progn 
  (assert (= (worktime-make-time-from-hhmm 59) 59))
  (assert (= (worktime-make-time-from-hhmm 100) 60))
  (assert (= (worktime-make-time-from-hhmm 150) 110))
  (assert (= (worktime-make-time-from-hhmm -150) -110))
  (assert (= (worktime-make-time-from-hhmm -15) -15))
  (assert (= (worktime-make-time-from-hhmm 0) 0)))

(defun worktime-format-time (time)
  (let* ((h (abs (/ time 60)))
         (m (abs (% time 60)))
         (s (format "%02d:%02d" h m)))
    ;; this hack is necessary because the sign
    ;; gets lost if h is zero!
    (if (>= time 0)
        s
      (concat "-" s))))

(progn
  (assert (string= (worktime-format-time 0) "00:00"))
  (assert (string= (worktime-format-time 15) "00:15"))
  (assert (string= (worktime-format-time -15) "-00:15"))
  (assert (string= (worktime-format-time 115) "01:55"))
  (assert (string= (worktime-format-time -115) "-01:55"))
  (assert (string= (worktime-format-time -12345) "-205:45")))


(defun worktime-time-as-hhmm (time)
  (let* ((h (/ time 60))
         (m (% time 60)))
    (+ (* h 100) m)))

(mapc (lambda (hhmm)
        (assert (= (worktime-time-as-hhmm (worktime-make-time-from-hhmm hhmm))
                   hhmm)))
      '(59 100 150 -150 -15 15 0))


(defun worktime-time-add (hhmm2 hhmm1)
  (and (integerp hhmm1) 
       (integerp hhmm2)
       (worktime-time-as-hhmm
        (+ (worktime-make-time-from-hhmm hhmm2)
           (worktime-make-time-from-hhmm hhmm1)))))

(defun worktime-time-diff (hhmm2 hhmm1)
  (and (integerp hhmm1) 
       (integerp hhmm2)
       (worktime-time-add hhmm2 (- hhmm1))))



(defun worktime-time-printer (hhmm)
  (cond 
   ((null hhmm) "")
   ((integerp hhmm)
    (worktime-format-time (worktime-make-time-from-hhmm hhmm)))
   (t (list (format "%s" hhmm)))))

;; (worktime-time-printer nil)
;; (worktime-time-printer 15450)
;; (worktime-time-printer 5450)
;; (worktime-time-printer 150)
;; (worktime-time-printer 0)
;; (worktime-time-printer 5)
;; (worktime-time-printer 050)
;; (worktime-time-printer 105)
;; (worktime-time-printer -15450)
;; (worktime-time-printer -150)
;; (worktime-time-printer -30)


(defun worktime-hhmm-to-minutes (hhmm)
  (+ (* 60 (worktime-hhmm-hours hhmm))
     (worktime-hhmm-minutes hhmm)))

;; From cl-extra.el
(defun worktime-signum (a)
  "Return 1 if A is positive, -1 if negative, 0 if zero."
  (cond ((> a 0) 1) ((< a 0) -1) (t 0)))

(defun worktime-time-diff (hhmm2 hhmm1)
  (cond ((and (integerp hhmm1) (integerp hhmm2))
         (let ((dmin (- (worktime-hhmm-to-minutes hhmm2)
                        (worktime-hhmm-to-minutes hhmm1))))
           (* (worktime-signum dmin)
              (worktime-make-hhmm 0 (abs dmin)))))
        (t nil)))
;; (worktime-time-diff 0030 1830)

(defun worktime-time-add (hhmm2 hhmm1)
  (cond ((and (integerp hhmm1) (integerp hhmm2))
         (worktime-make-hhmm 0 (+ (worktime-hhmm-to-minutes hhmm2)
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

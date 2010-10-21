(require 'regress)

(require 'ses-formulas)


(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (put 'worktime-time-printer-regress 'regression-suite t)
  (setq worktime-time-printer-regress
   '("worktime-time-printer-regress"
     ;; Each test in the suite is of the form:
     ;;   ([description] probe grader)
     ;;   DESCRIPTION - string
     ;;   PROBE -  a sexp which runs the actual test
     ;;   GRADER - the desired result or a sexp which determines
     ;;   how we did
     ("Calling `worktime-time-printer' with a NIL argumen"
      (worktime-time-printer nil)
      "")
     ("Calling `worktime-time-printer' with some sample value 1"
      (worktime-time-printer 5450)
      :test
       (equal RESULT '("54:50")))
     ("Calling `worktime-time-printer' with some sample value 2"
      (worktime-time-printer 15450)
      :test
       (equal RESULT '("154:50")))
     ("Calling `worktime-time-printer' with some sample value 3"
      (worktime-time-printer 150)
      :test
       (equal RESULT '("01:50")))
     ("Calling `worktime-time-printer' with some sample value 4"
      (worktime-time-printer 105)
      :test
       (equal RESULT '("01:05")))
     ("Calling `worktime-time-printer' with a zero"
      (worktime-time-printer 0)
      :test
       (equal RESULT '("00:00")))
     ("Calling `worktime-time-printer' with a 5 as argument"
      (worktime-time-printer 5)
      :test
       (equal RESULT '("00:05")))
     ("Calling `worktime-time-printer' with a zero hour argument"
      (worktime-time-printer 50)
      :test
       (equal RESULT '("00:50")))
     ("Calling `worktime-time-printer' with a negative argument"
      (worktime-time-printer -15450)
      :test
       (equal RESULT '("-154:50")))
     ("Calling `worktime-time-printer' with a negative argument 2"
      (worktime-time-printer -150)
      :test
       (equal RESULT '("-01:50")))
     ("Calling `worktime-time-printer' with a negative, zero hour argument"
      (worktime-time-printer -30)
      :test
       (equal RESULT '("-00:30")))
     )))

;; (regress worktime-time-printer-regress)


(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (put 'worktime-decompose-hhmm-regress 'regression-suite t)
  (setq worktime-decompose-hhmm-regress
   '("worktime-decompose-hhmm-regress"
     ;; Each test in the suite is of the form:
     ;;   ([description] probe grader)
     ;;   DESCRIPTION - string
     ;;   PROBE -  a sexp which runs the actual test
     ;;   GRADER - the desired result or a sexp which determines
     ;;   how we did
     ("Calling `worktime-decompose-hhmm' with a sample argument"
      (worktime-decompose-hhmm 125)
      :test
       (equal RESULT '(1 25)))
     ("Calling `worktime-decompose-hhmm' with a negative argument"
      (worktime-decompose-hhmm -125)
      :test
       (equal RESULT '(-1 25)))
     ("Calling `worktime-decompose-hhmm' with a zero hour argument"
      (worktime-decompose-hhmm 30)
      :test
       (equal RESULT '(0 30)))
     ("Calling `worktime-decompose-hhmm' with a negative, zero hour argument"
      (worktime-decompose-hhmm -30)
      :test
       (equal RESULT '(0 -30)))
     ("Calling `worktime-decompose-hhmm' with a zero argument"
      (worktime-decompose-hhmm 0)
      :test
       (equal RESULT '(0 0)))
      )))

;; (regress worktime-decompose-hhmm-regress)


(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (put 'worktime-hhmm-to-minutes-regress 'regression-suite t)
  (setq worktime-hhmm-to-minutes-regress
   '("ses-formulas-regress"
     ;; Each test in the suite is of the form:
     ;;   ([description] probe grader)
     ;;   DESCRIPTION - string
     ;;   PROBE -  a sexp which runs the actual test
     ;;   GRADER - the desired result or a sexp which determines
     ;;   how we did
     ("Simple test of `worktime-hhmm-to-minutes'"
      (worktime-hhmm-to-minutes 0150)
      110)
     ("Test of `worktime-hhmm-to-minutes' with a zero hours argument"
      (worktime-hhmm-to-minutes 0005)
      5)
     ("Test of `worktime-hhmm-to-minutes' with a zero minutes argument"
      (worktime-hhmm-to-minutes 0300)
      180)
     ("Test of `worktime-hhmm-to-minutes' with both hours and minutes zero"
      (worktime-hhmm-to-minutes 0000)
      0)
     ("Test of `worktime-hhmm-to-minutes' with a negative hours argument"
      (worktime-hhmm-to-minutes -0559)
      -359)
     ("Test of `worktime-hhmm-to-minutes' with a negative hours argument and zero minutes"
      (worktime-hhmm-to-minutes -800)
      -480)
      )))

;; (regress worktime-hhmm-to-minutes-regress)


(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (put 'worktime-minutes-to-hhmm-regress 'regression-suite t)
  (setq worktime-minutes-to-hhmm-regress
   '("ses-formulas-regress"
     ;; Each test in the suite is of the form:
     ;;   ([description] probe grader)
     ;;   DESCRIPTION - string
     ;;   PROBE -  a sexp which runs the actual test
     ;;   GRADER - the desired result or a sexp which determines
     ;;   how we did
     ("Simple test of `worktime-minutes-to-hhmm'"
      (worktime-minutes-to-hhmm 110)
      150)
     ("Test of `worktime-minutes-to-hhmm' with a zero hours argument"
      (worktime-minutes-to-hhmm 5)
      5)
     ("Test of `worktime-minutes-to-hhmm' with a zero minutes argument"
      (worktime-minutes-to-hhmm 180)
      300)
     ("Test of `worktime-minutes-to-hhmm' with both hours and minutes zero"
      (worktime-minutes-to-hhmm 0)
      0)
     ("Test of `worktime-minutes-to-hhmm' with a negative hours argument"
      (worktime-minutes-to-hhmm -359)
      -559)
     ("Test of `worktime-minutes-to-hhmm' with a negative hours argument"
      (worktime-minutes-to-hhmm -361)
      -601)
     ("Test of `worktime-minutes-to-hhmm' with a negative hours argument and zero minutes"
      (worktime-minutes-to-hhmm -480)
      -800)
      )))

;; (regress worktime-minutes-to-hhmm-regress)

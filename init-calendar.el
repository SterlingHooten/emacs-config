;;; init-calendar.el --- setup for the Calendar package
;;  Emilio C. Lopes <eclig@gmx.net>

(setq calendar-latitude 48.28)
(setq calendar-longitude 11.57)
(setq calendar-location-name "Unterschleißheim")

;; (setq calendar-mark-diary-entries-flag t)
(setq calendar-mark-holidays-flag t)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

(setq calendar-view-holidays-initially-flag t)
(add-hook 'calendar-move-hook
          (lambda ()
            "Update the holidays window (if visible) as we move in calendar."
            (when (get-buffer-window holiday-buffer 'visible)
              (calendar-list-holidays))))

;; display the ISO week numbers (see also the docstring of `calendar-intermonth-text')
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 0.7)
(copy-face 'default 'calendar-iso-week-header-face)
(set-face-attribute 'calendar-iso-week-header-face nil
                    :height 0.7)
(setq calendar-intermonth-header
      (propertize "KW"
                  'font-lock-face 'calendar-iso-week-header-face))
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

(setq calendar-intermonth-spacing 8)

;; German settings and holidays
(dolist (pattern '((day "\." month "\." year)
                   (day "\." month)
                   (day "\." monthname "\." year)
                   (day "\." monthname)))
  (add-to-list 'diary-date-forms pattern))

(calendar-set-date-style 'european)

(setq calendar-week-start-day 1)

(setq calendar-time-display-form
      '(24-hours ":" minutes (and time-zone (concat " (" time-zone ")"))))

(setq calendar-day-name-array
      ["Sonntag" "Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag" "Samstag"])
(setq calendar-month-name-array
      ["Januar" "Februar" "März" "April" "Mai" "Juni"
       "Juli" "August" "September" "Oktober" "November" "Dezember"])
(setq solar-n-hemi-seasons
      '("Frühlingsanfang" "Sommeranfang" "Herbstanfang" "Winteranfang"))

(setq calendar-daylight-savings-starts '(calendar-nth-named-day -1 0 3 year))
(setq calendar-daylight-savings-ends '(calendar-nth-named-day -1 0 10 year))

(setq holiday-general-holidays
      '((holiday-fixed 1 1 "Neujahr")
        (holiday-fixed 5 1 "1. Mai")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit")))

(setq holiday-christian-holidays
      '((holiday-float 12 0 -4 "1. Advent" 24)
        (holiday-float 12 0 -3 "2. Advent" 24)
        (holiday-float 12 0 -2 "3. Advent" 24)
        (holiday-float 12 0 -1 "4. Advent" 24)
        (holiday-fixed 12 24 "Heiligabend")
        (holiday-fixed 12 25 "1. Weihnachtstag")
        (holiday-fixed 12 26 "2. Weihnachtstag")
        (holiday-fixed 12 31 "Silvester")
        (holiday-fixed 1 6 "Heilige Drei Könige")
        (holiday-easter-etc -48 "Rosenmontag")
        ;; (holiday-easter-etc -3 "Gründonnerstag")
        (holiday-easter-etc  -2 "Karfreitag")
        (holiday-easter-etc   0 "Ostersonntag")
        (holiday-easter-etc  +1 "Ostermontag")
        (holiday-easter-etc +39 "Christi Himmelfahrt")
        (holiday-easter-etc +49 "Pfingstsonntag")
        (holiday-easter-etc +50 "Pfingstmontag")
        (holiday-easter-etc +60 "Fronleichnam")
        (holiday-fixed 8 15 "Mariæ Himmelfahrt")
        (holiday-fixed 11 1 "Allerheiligen")
        ;; (holiday-float 11 3 1 "Buss- und Bettag" 16)
        (holiday-float 11 0 1 "Totensonntag" 20)))

(setq holiday-other-holidays
      '((holiday-fixed 6 12 "Dia dos Namorados")
        (holiday-float 5 0 2 "Dias das Mães")
        (holiday-float 8 0 2 "Dias dos Pais")))

(setq holiday-solar-holidays
      '((solar-equinoxes-solstices)
        (holiday-sexp calendar-daylight-savings-starts "Beginn der Sommerzeit")
        (holiday-sexp calendar-daylight-savings-ends "Ende der Sommerzeit")))

(setq calendar-holidays
      (append holiday-general-holidays holiday-local-holidays holiday-other-holidays
              holiday-christian-holidays holiday-solar-holidays))

(provide 'init-calendar)
;;; init-calendar.el ends here

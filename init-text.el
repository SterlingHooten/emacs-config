;;; Filladapt
(when (require-soft 'filladapt)
  (setq filladapt-fill-column-tolerance 6)
  (setq filladapt-mode-line-string nil)
  (add-hook 'text-mode-hook 'turn-on-filladapt-mode))

;;; Abbrevs
(mapc (lambda (x)
        (define-abbrev text-mode-abbrev-table (car x) (cadr x)))
      '(("bkt" "barkeit")
        ("lkt" "lichkeit")
        ("bzgl" "bezüglich")
        ("fÜr" "für")
        ("ggf" "gegebenenfalls")
        ("gr" "Gruß\n\n Emílio")
        ("gre" "are")
        ("grs" "Grüße\n\n Emílio")
        ("mfg" "Mit freundlichen Grüßen\n\n Emílio Lopes")
        ("mgl" "möglich")
        ("vllt" "vielleicht")
        ("zv" "zur Verfügung")))

(add-hook 'text-mode-hook
          (lambda ()
            (abbrev-mode 1)))

;;; Ispell
(setq ispell-local-dictionary-alist
      '(("deutsch" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "de_DE") nil utf-8)))

(setq-default ispell-local-dictionary "deutsch")

(global-defkey "C-c i m" 'ispell-message)
(global-defkey "C-c i b" 'ispell-buffer)
(global-defkey "C-c i r" 'ispell-region)
(global-defkey "C-c i c" 'ispell-change-dictionary)
(global-defkey "C-c i k" 'ispell-kill-ispell)

(global-defkey "C-c i d"
  (lambda () "*Set German dictionary (Ispell)."
    (interactive)
    (ispell-change-dictionary "deutsch8")))
(global-defkey "C-c i e"
  (lambda () "*Set English dictionary (Ispell)."
    (interactive)
    (ispell-change-dictionary "english")))
(global-defkey "C-c i p"
  (lambda () "*Set Portuguese dictionary (Ispell)."
    (interactive)
    (ispell-change-dictionary "portugues")))

(defun show-current-ispell-dictionary ()
  "*Display the value of ispell-dictionary in the echo area."
  (interactive)
  (if ispell-dictionary
      (message "Current dictionary: %s" ispell-dictionary)
    (message "Variable `ispell-dictionary' is not set.")))

(global-defkey "C-c i w" 'show-current-ispell-dictionary)

;;; Spelling
(setq spelling-alphabet
      ;; Buchstabe    Deutschland        ITU/ICAO/NATO
      '(("A"          "Anton"            "Alfa")
        ("Ä"          "Ärger"            "?")
        ("B"          "Berta"            "Bravo")
        ("C"          "Cäsar"            "Charlie")
        ("Ch"         "Charlotte"        "?")
        ("D"          "Dora"             "Delta")
        ("E"          "Emil"             "Echo")
        ("F"          "Friedrich"        "Foxtrot")
        ("G"          "Gustav"           "Golf")
        ("H"          "Heinrich"         "Hotel")
        ("I"          "Ida"              "India")
        ("J"          "Julius"           "Juliett")
        ("K"          "Kaufmann"         "Kilo")
        ("L"          "Ludwig"           "Lima")
        ("M"          "Martha"           "Mike")
        ("N"          "Nordpol"          "November")
        ("O"          "Otto"             "Oscar")
        ("Ö"          "Ökonom"           "?")
        ("P"          "Paula"            "Papa")
        ("Q"          "Quelle"           "Quebec")
        ("R"          "Richard"          "Romeo")
        ("S"          "Siegfried"        "Sierra")
        ("Sch"        "Schule"           "?")
        ("ß"          "Eszett"           "?")
        ("T"          "Theodor"          "Tango")
        ("U"          "Ulrich"           "Uniform")
        ("Ü"          "Übermut"          "?")
        ("V"          "Viktor"           "Victor")
        ("W"          "Wilhelm"          "Whiskey")
        ("X"          "Xanthippe"        "X-Ray")
        ("Y"          "Ypsilon"          "Yankee")
        ("Z"          "Zeppelin"         "Zulu")))

;; Stefan Reichör (http://www.xsteve.at/prg/emacs/xsteve-functions.el)
(defun buchstabiere (str &optional international)
  (interactive "sBuchstabiere: \nP")
  (message "%s"
           (mapcar
            (lambda (ch)
              (or (nth (if international 2 1) (assoc (char-to-string ch) spelling-alphabet)) "?"))
            (upcase str))))

(defun spell (str)
  (interactive "sSpell: ")
  (buchstabiere str 1))

;;; miscellaneous functions
(defun unfill-paragraph ()
  (interactive "*")
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; see http://www.emacswiki.org/emacs/RotateWordCapitalization
(defun cycle-word-capitalization ()
  "*Change the capitalization of the current word.
If the word under point is in lower case, capitalize it.  If it
is in capitalized form, change it to upper case.  If it is in
upper case, downcase it."
  (interactive "*")
  (let ((case-fold-search nil))
    (save-excursion
      (skip-syntax-backward "w")
      (cond
       ((looking-at-p "[[:lower:]]+")
        (capitalize-word 1))
       ;; ((looking-at-p "[[:upper:]][[:lower:]]+")
       ;;  (upcase-word 1))
       ;; ((looking-at-p "[[:upper:]]+")
       ;;  (downcase-word 1))
       (t
        (downcase-word 1))))))

(global-defkey "M-C" 'cycle-word-capitalization)

;;; Text scratch
(defun jump-to-text-scratch-buffer (&optional arg)
  "*Switch to buffer `*text scratch*', creating it if necessary.
With prefix arg generate a fresh buffer."
  (interactive "P")
  (let ((buffer-name "*text scratch*"))
    (switch-to-buffer-create
     (if arg (generate-new-buffer-name buffer-name) buffer-name)
     'text-mode
     nil)))

(provide 'init-text)

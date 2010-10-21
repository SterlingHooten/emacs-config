(setq printer-list
;; "A list of pairs of printer names and printer descriptions.
;; The printer descriptions are themselves a list of pairs of
;; variables and their values for the relevant printer
;; configuration."
      '((pm193000 . ((description . "Benziner, S/W")
                     (parameters . ((lpr-command  "")
                                    (ps-printer-name "//gmuc0122/pm193000")))
                     (function . printer-ps-print-buffer-with-faces-maybe)))

        (pm192304 . ((description . "Bei der Tina, S/W")
                     (parameters . ((lpr-command  "")
                                    (ps-printer-name "//eepmuc02/pm192304")))
                     (function . printer-ps-print-buffer-with-faces-maybe)))

        (pmuc0516 . ((description . "Poolraum, S/W")
                     (parameters . ((lpr-command  "")
                                    (ps-printer-name "//gmuc0119/pmuc0516")))
                     (function . printer-ps-print-buffer-with-faces-maybe)))

        (pmuc0516-noheaders . ((description . "Poolraum, S/W, no headers")
                               (parameters . ((lpr-command  "")
                                              (ps-printer-name "//gmuc0119/pmuc0516")
                                              (ps-print-header nil)))
                               (function . printer-ps-print-buffer-with-faces-maybe)))

        (pmuc0516-noheaders-landscape . ((description . "Poolraum, S/W, no headers, landscape")
                                         (parameters . ((lpr-command  "")
                                                        (ps-landscape-mode t)
                                                        (ps-number-of-columns 1)
                                                        (ps-printer-name "//gmuc0119/pmuc0516")
                                                        (ps-print-header nil)))
                                         (function . printer-ps-print-buffer-with-faces-maybe)))

        (pmuc0516-2up . ((description . "Poolraum, S/W, 2up")
                         (parameters . ((lpr-command  "")
                                        (ps-landscape-mode t)
                                        (ps-number-of-columns 2)
                                        (ps-printer-name "//gmuc0119/pmuc0516")))
                         (function . printer-ps-print-buffer-with-faces-maybe)))
        ))

(defun printer-ps-print-buffer-with-faces-maybe ()
  "Print current buffer using faces if available."
  (funcall (if window-system
               'ps-print-buffer-with-faces
             'ps-print-buffer)))

(defmacro printer-assq-val (key alist)
  `(cdr-safe  (assq ,key ,alist)))

(defun printer-get-config (printer)
  "Return the configuration of printer PRINTER in `printerlist'."
  (printer-assq-val printer printer-list))

(defun printer-get-parameters (printer)
  "Return the configuration of printer PRINTER in `printerlist'."
  (printer-assq-val 'parameters (printer-get-config printer)))

(defun printer-get-description (printer)
  "Return the configuration of printer PRINTER in `printerlist'."
  (printer-assq-val 'description (printer-get-config printer)))

(defun printer-get-function (printer)
  "Return the configuration of printer PRINTER in `printerlist'."
  (printer-assq-val 'function (printer-get-config printer)))

(defun printer-print (printer)
  (interactive (list (printer-get-name)))
  (eval `(let ,(printer-get-parameters printer)
           ;;(message ps-printer-name)
           (funcall (printer-get-function printer))
           ;;(printer-get-function printer)
           )))

(defun printer-get-name ()
  "Select interactively a PostScript printer."
  (printer-complete-alist "Printer" printer-list nil))

;; Adapted from code from printing.el
(defun printer-complete-alist (prompt alist default)
  (let ((collection (mapcar #'(lambda (elt)
				(setq elt (car elt))
				(cons (concat (symbol-name elt)
                                              " -- "
                                              (printer-get-description elt))
                                      elt))
			    alist)))
    (cdr (assoc (completing-read (concat prompt ": ")
				 collection nil t
				 default nil
				 default)
		collection))))

(provide 'printer)
;; (printer-get-config 'pm193000)
;; (printer-get-parameters 'pm193000)
;; (printer-get-parameters 'pmuc0516-2up)
;; (printer-get-description 'pmuc0516-noheaders)
;; (printer-get-function 'pm192304)

;; (printer-print 'pm192304)
;; (printer-print 'pmuc0516-2up)

;; (eval `(let ,(printer-get-parameters 'pm192304)
;;          ps-printer-name))

;; (let ((cfg (printer-get-config 'pm193000)))
;;   cfg)

;; '("\\\\eepmuc02\\pm193000"
;;   "\\\\eepmuc01\\pm193001"
;;   "\\\\eepmuc02\\pm192304"
;;   "\\\\gmuc0119\\pmuc0516")

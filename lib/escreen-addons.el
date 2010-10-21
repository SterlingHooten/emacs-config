;; Time-stamp: <2003-07-10 10:04:08 Emilio C. Lopes>


;; For escreen-desktop we need to save:
;;
;; escreen-configuration-alist
;; escreen-current-screen-number
;; escreen-current-screen-string
;; escreen-last-screen-number
;; escreen-highest-screen-number-used
;; escreen-mode-line-format

;; (nth 0 (car escreen-configuration-alist))

;; (nth 0 (escreen-configuration-escreen escreen-current-screen-number))
;; (car (escreen-configuration-escreen escreen-current-screen-number))

;; Maybe TODO: if number already in use, exchange the number of the
;; two screens instead of signaling an error.
(defun escreen-set-screen-number (n)
  "Set the number of the currently selected screen to N.
If there is already a screen with number N, signal an error."
  (interactive "NSet escreen number to: ")
  (setq n (prefix-numeric-value n))
  (when (escreen-screen-defined n)
    (error "escreen: screen number %d is already in use." n))
  (when (eq escreen-current-screen-number escreen-highest-screen-number-used)
    ;; We're renumbering the screen with the highest number.
    ;; Look for the next highest number.
    (setq escreen-highest-screen-number-used
          (or (nth 1 (sort (escreen-configuration-screen-numbers) '>)) ; `nth' returns nil if list is too short
              escreen-current-screen-number)))
  (when (> n escreen-highest-screen-number-used)
    (setq escreen-highest-screen-number-used n))
  (setcar (escreen-configuration-escreen escreen-current-screen-number) n)
  (setq escreen-current-screen-number n)
  (setq escreen-current-screen-string (int-to-string n)))

(defun escreen-first-unused-screen-number ()
  (when (< (length (escreen-configuration-screen-numbers)) escreen-max-screens)
    (let ((number 0))
      (while (escreen-configuration-escreen number)
        (setq number (1+ number)))
      number)))

(define-key escreen-map "#"    'escreen-set-screen-number)

(provide 'escreen-addons)

;;; insert-delims.el --- insertion of paired delimiters

;; Note: I changed the following code to skip over sexps instead of
;; over words. Emilio Lopes <eclig@gmx.net>

;; From: Erik Naggum <erik@naggum.no>
;; Subject: Re: [Q] automatic brackets in c-mode (or AWK-mode) ?
;; Date: 1997/01/28
;; Message-ID: <3063455252850083@naggum.no>#1/1
;; newsgroups: comp.emacs

;; * Rolf Meinecke
;; | I would like to have both opening and closing bracket inserted and
;; | the cursor put between them, when I invoke the opening-bracket-key.
;; | 
;; | eg.: I hit [   and see  []
;; |                          ^ with the curser placed here.
;; | 
;; | I bet someone already implemented something like that and I wouldn't
;; | be astonished if it's already in the normal c-mode or AWK-mode.
;; | (How can I activate that feature if it is?)

;; the simplest solution:

;; (local-set-key "[" (read-kbd-macro "[ ] C-b"))

;; a more general solution:

(defvar extra-delimiter-pairs '((?\` . ?\') (?« . ?») (?< . ?>))
  "Alist of delimiters that should be paired in addition to syntax.")

(defun insert-delimiter-pair (arg)
  "Put pair of delimiters around next ARG sexps, and leave point after first.
No argument is equivalent to zero: just insert pair and leave point between.
  If the last command character has open-parenthesis syntax or is equal to
the car of an element of `extra-delimiter-pairs', insert it and the
matching parenthesis.  If it has close-parenthesis syntax or is equal to
the cdr of an element of `extra-delimiter-paris', search for the first
occurrence and leave point past it; ignore ARG.
  Any other syntax means just insert a pair of the last command character."
  (interactive "P")
  (let* ((arg (if arg (prefix-numeric-value arg) 0))
         (command (logand last-command-char ?\xff))
         (before (assoc command extra-delimiter-pairs))
         (after (rassoc command extra-delimiter-pairs)))
    (if (or after (eq (char-syntax command) ?\)))
        (search-forward (char-to-string command) nil t)
      (if (or before (eq (char-syntax command) ?\())
          (setq after (if before (cdr before) (matching-paren command))
                before command)
        (setq before command
              after command))
      (if (< arg 0)
          (save-excursion
            (insert after)
            (forward-sexp arg)
            (insert before))
        (or (eq arg 0) (skip-chars-forward " \t"))
        (insert before)
        (save-excursion
          (or (eq arg 0) (forward-sexp arg))
          (insert after))))))

;; then I bind them to a handful of useful keys.  the useless capslock key
;; below my shift key has been remapped to the Hyper modifier with xmodmap, so
;; it is conveniently in reach with a curled pinky for the shifted ones.  note
;; that M-( already inserts a paren pair mostly the same way, except it also
;; introduces spacing around them.

;; ;; one form to map them all and in the darkness bind them
;; (map nil
;;      (lambda (char)
;;        (define-key global-map
;; 	 (read-kbd-macro (concat "H-" (char-to-string char)))
;; 	 'insert-delimiter-pair))
;;      "_\"*()[]{}<>`'")

;; if you would rather not (require 'cl), you can replace "map nil" with
;; "mapcar".  this is an Emacs special.

;; #\Erik
;; -- 
;; 1,3,7-trimethylxanthine -- a basic ingredient in quality software.

(provide 'insert-delims)

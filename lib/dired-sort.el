;;; dired-sort.el --- in Dired: press s then s, x, t or n to sort by Size, eXtension, Time or Name
;;; inspired by Francis J. Wright's dired-sort-menu.el

;; Author: Patrick Anderson 
;; Version: 1
;; License: GPL

;install:
;this file in your load path

;add
; (require 'dired-sort-map)
;to your .emacs file

;execute
; M-x eval-buffer
;so you don't have to restart

;todo:

(defvar dired-sort-map (make-sparse-keymap))

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "s" dired-sort-map)
             (define-key dired-sort-map "s" '(lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
             (define-key dired-sort-map "x" '(lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
             (define-key dired-sort-map "t" '(lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
             (define-key dired-sort-map "n" '(lambda () "sort by Name" (interactive) (dired-sort-other (concat dired-listing-switches ""))))
))

(provide 'dired-sort)

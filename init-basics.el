;;; init-basics.el --- basic functions and macros
;; Copyright (C) 2013  Emilio C. Lopes

(defun add-to-path (path dir &optional append)
  "*Add directory DIR to path PATH.
If optional argument APPEND is non-nil, DIR is added at the end."
  (setq dir (expand-file-name dir))
  (and (file-directory-p dir) (file-accessible-directory-p dir)
       (add-to-list path dir append)))

(defmacro require-soft (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror))

(defmacro global-defkey (key def)
  "*Bind KEY globally to DEF.
KEY should be a string constant in the format used for
saving keyboard macros (cf. `insert-kbd-macro')."
  `(global-set-key (kbd ,key) ,def))

(defmacro local-defkey (key def)
  "*Bind KEY locally to DEF.
KEY should be a string constant in the format used for
saving keyboard macros (cf. `insert-kbd-macro')."
  `(local-set-key (kbd ,key) ,def))

(defmacro defkey (keymap key def)
  "*Define KEY to DEF in KEYMAP.
KEY should be a string constant in the format used for
saving keyboard macros (cf. `insert-kbd-macro')."
  `(define-key ,keymap (kbd ,key) ,def))

(provide 'init-basics)
;;; init-basics.el ends here

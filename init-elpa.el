(when (require-soft 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/"))

  (package-initialize)

  (when (not package-archive-contents)
    (package-refresh-contents)))

;; http://stackoverflow.com/a/7781594

;; (defvar user-packages
;;   '(auctex clojure-mode coffee-mode deft gist haml-mode
;;            haskell-mode magit markdown-mode paredit projectile
;;            sass-mode scss-mode yaml-mode yari yasnippet)
;;   "A list of packages that should be available or installed at launch.")

;; (dolist (p user-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))

(provide 'init-elpa)

;; http://m00natic.github.io/emacs/emacs-wiki.html
(eval-after-load "semantic"
  '(progn
     (add-to-list 'semantic-default-submodes
                  'global-semantic-decoration-mode)
     (add-to-list 'semantic-default-submodes
                  'global-semantic-idle-summary-mode)
     (add-to-list 'semantic-default-submodes
                  'global-semantic-idle-local-symbol-highlight-mode)
     (add-to-list 'semantic-default-submodes
                  'global-semantic-mru-bookmark-mode)))

(when (require-soft 'auto-complete-config)
  (ac-config-default)

  (eval-after-load "semantic"
    '(setq-default ac-sources (cons 'ac-source-semantic ac-sources))))

(provide 'init-ide)

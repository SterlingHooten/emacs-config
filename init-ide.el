;; http://m00natic.github.io/emacs/emacs-wiki.html
;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
(eval-after-load "semantic"
  '(progn
     (require 'semantic/ia)
     (require 'semantic/bovine/gcc)
     (when (require-soft 'semantic-tag-folding)
       (global-semantic-tag-folding-mode +1))

     (add-to-list 'semantic-default-submodes
                  'global-semantic-decoration-mode)
     (add-to-list 'semantic-default-submodes
                  'global-semantic-idle-summary-mode)
     (add-to-list 'semantic-default-submodes
                  'global-semantic-idle-local-symbol-highlight-mode)
     (add-to-list 'semantic-default-submodes
                  'global-semantic-mru-bookmark-mode)
     (add-to-list 'semantic-default-submodes
                  'global-semantic-stickyfunc-mode)
     (add-to-list 'semantic-default-submodes
                  'global-semantic-highlight-func-mode)

     (when (and (executable-find "global")
                (require 'semantic/db-global)
                (cedet-gnu-global-version-check t))
       (semanticdb-enable-gnu-global-databases 'c-mode)
       (semanticdb-enable-gnu-global-databases 'c++-mode))))

(when (require-soft 'auto-complete-config)
  (ac-config-default)

  (eval-after-load "semantic"
    '(add-to-list 'ac-sources 'ac-source-semantic)))

(when (require-soft 'projectile)
  (projectile-mode +1))

(semantic-mode +1)

(provide 'init-ide)

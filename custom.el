(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((dired-omit-extensions "~" ".bak" ".html" ".dir-locals.el" ".git/") (auto-insert-alist ((latex-mode . "LaTeX letter template") lambda nil (when (file-exists-p "template.tex") (insert-file-contents "template.tex")))) (dired-omit-mode . t) (after-save-hook lambda nil (let ((b (org-export-as-html 3))) (and (bufferp b) (kill-buffer b))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

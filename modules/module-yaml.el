;;; module-yaml --- edit yaml files

;;; Commentary:

;;; Code:

(use-package yaml-mode
  :mode "\\.yml\\'"
  :hook (yaml-mode . (lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(provide 'module-yaml)

;;; module-yaml ends here

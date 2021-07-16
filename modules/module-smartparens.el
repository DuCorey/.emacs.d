;;; module-smartparens.el --- Dealing with pairs smartly

;;; Commentary:

;;; Code:

(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook (prog-mode . smartparens-mode)
  ;; Alllow for single insert of pair objects
  :bind (("C-(" . (lambda () (interactive) (insert "(")))
	 ("C-)" . (lambda () (interactive) (insert ")")))))

(provide 'module-smartparens)

;;; module-smartparens.el ends here

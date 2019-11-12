;;; module-helpful.el --- Improving emacs help

;;; Commentary:

;;; Code:

(use-package helpful
  :bind (("C-h k" . helpful-key)
	 ("C-h C-." . helpful-at-point))
  :custom
  ;; Integrate Ivy and helpful commands
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))


(provide 'module-helpful)

;;; module-helpful.el ends here

;;; module-smartparens.el --- Dealing with pairs smartly

;;; Commentary:

;;; Code:

(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook (prog-mode . smartparens-mode))


(provide 'module-smartparens)

;;; module-smartparens.el ends here

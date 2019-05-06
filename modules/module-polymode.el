;;; module-polymode.el --- Allow multiple modes to exist in the same buffer

;;; Commentary:

;;; Code:

(use-package polymode
  :ensure markdown-mode
  :config
  (require 'poly-R)
  (require 'poly-markdown)

  ;;; MARKDOWN
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
  ;;; R modes
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode)))

(provide 'module-polymode)

;;; module-polymode.el ends here

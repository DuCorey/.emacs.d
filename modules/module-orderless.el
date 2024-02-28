;;; module-orderless.el --- oderless copletion style

;;; Commentary:

;;; Code:

(use-package orderless
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-category-defaults nil))


(provide 'module-orderless)

;;; module-orderless.el ends here

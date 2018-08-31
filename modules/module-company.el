;;; module-company --- company-mode autocomplete

;;; Commentary:

;;; Code:

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 3)
  (company-show-numbers t)
  (company-global-modes '(not inferior-python-mode))
  ;; Company dabbrev recommendations are case sensitive
  (company-dabbrev-downcase nil))


(provide 'module-company)

;;; module-company ends here

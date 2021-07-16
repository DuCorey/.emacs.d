;;; module-company --- company-mode autocomplete

;;; Commentary:

;;; Code:

(use-package company
  :hook
  (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 3)
  (company-show-numbers t)
  (company-global-modes '(not inferior-python-mode))
  ;; Company dabbrev recommendations are case sensitive
  (company-dabbrev-downcase nil)
  ;; Avoid duplicates in company mode when use case sensitive mode
  (company-dabbrev-ignore-case nil)
  ;; Company dabbrev only search buffers with the same major mode
  (company-dabbrev-other-buffers t))

(provide 'module-company)

;;; module-company ends here

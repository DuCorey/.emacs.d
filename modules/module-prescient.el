;;; module-prescient.el --- Simple but effective sorting and filtering for Emacs

;;; Commentary:

;;; Code:

(use-package prescient
  :config
  (prescient-persist-mode 1)
  :custom
  (prescient-filter-method '(literal regexp)))

(use-package ivy-prescient
  :after (ivy counsel)
  :config
  (ivy-prescient-mode 1)
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil :foreground "#c678dd" :background "#1c1f24")
  :custom
  (ivy-prescient-retain-classic-highlighting t))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))


(provide 'module-prescient)

;;; module-prescient.el ends here

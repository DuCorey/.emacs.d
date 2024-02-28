;;; module-embark.el --- Emacs Mini-Buffer Actions

;;; Commentary:

;;; Code:

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(provide 'module-embark)

;;; module-embark.el ends here

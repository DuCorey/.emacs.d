;;; module-auto-package-update.el --- automatically updates and keeps emac packages up to date

;;; Commentary:

;;; Code:

(use-package auto-package-update
  :custom
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  :config
  (auto-package-update-maybe))


(provide 'module-auto-package-update)

;;; module-auto-package-update.el ends here


;;; module-ivy.el --- Ivy autocompletion framework

;;; Commentary:

;;; Code:

(use-package all-the-icons
  :config
  ;; Add custom icons for various modes that can break ivy-rich
  (add-to-list 'all-the-icons-mode-icon-alist '(ess-mode all-the-icons-fileicon "R" :face all-the-icons-lblue))

  ;; Dired all the icons
  (use-package all-the-icons-dired
    :after all-the-icons
    :hook (dired-mode . all-the-icons-dired-mode)))

(provide 'module-all-the-icons)

;;; module-all-the-icons.el ends here

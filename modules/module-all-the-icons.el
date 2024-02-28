;;; module-all-the-icons.el --- Add icons to emacs for further customization

;;; Commentary:

;;; Code:

(use-package all-the-icons
  :config
  ;; Add custom icons for various modes that can break ivy-rich
  (add-to-list 'all-the-icons-mode-icon-alist '(ess-mode all-the-icons-fileicon "R" :face all-the-icons-lblue)))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :after all-the-icons
  :init
  (all-the-icons-completion-mode)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(provide 'module-all-the-icons)

;;; module-all-the-icons.el ends here

;;; module-projectile.el --- project management

;;; Commentary:

;; Code:

(use-package projectile
  :bind (:map projectile-mode-map
	 ("C-c p" . projectile-command-map))
  :config
  (use-package counsel-projectile
    :after projectile
    :config
    (counsel-projectile-mode 1)))


(provide 'module-projectile)

;;; module-project.el ends here

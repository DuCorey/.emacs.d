;;; module-org.el -- the many org related configurations

;;; Commentary:

;;; Code:

;; Start recentf mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


(provide 'module-recentf)

;;; module-recentf.el ends here

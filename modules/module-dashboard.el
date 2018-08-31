;;; module-dashboard.el --- Start screen dashboard

;;; Commentary:

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-items '((recents . 5)
		     (bookmarks . 5)
		     (projects . 5)
		     (agenda . 5))))


(provide 'module-dashboard)

;;; module-dashboard ends here

;;; module-dashboard.el --- Start screen dashboard

;;; Commentary:

(use-package dashboard
  :config
  (require 'org-journal)
  (org-journal-update-org-agenda-files)
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-items '((recents . 5)
		     (bookmarks . 5)
		     (projects . 5)
		     (agenda . 5)))
  (show-week-agenda-p t))


(provide 'module-dashboard)

;;; module-dashboard ends here

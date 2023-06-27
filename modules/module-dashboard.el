;;; module-dashboard.el --- Start screen dashboard

;;; Commentary:

;;; Code:

(use-package dashboard
  :custom
  (dashboard-items '((recents . 5)
		     (bookmarks . 5)
		     (projects . 5)
		     (agenda . 5)))
;		     (todo . 10)))
  (show-week-agenda-p t)
  (dashboard-icon-type 'all-the-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-projects-backend 'project-el)
  :config
  (require 'org-journal)
  ;(org-journal-update-org-agenda-files)
  (dashboard-setup-startup-hook)

  (eval-after-load "dashboard"
    '(defun dashboard-insert-agenda (list-size)
       "Add the list of LIST-SIZE items of agenda."
       (require 'org-agenda)
       (require 'calendar)
       (let ((agenda (dashboard-get-agenda)))
	 (dashboard-insert-section
	  (or (and (boundp 'show-week-agenda-p) show-week-agenda-p "Agenda for the coming week:")
	      "Agenda for today:")
	  agenda
	  list-size
	  "a"
	  `(lambda (&rest ignore)
	     (let ((buffer (find-file (nth 4 ',el))))
	       (with-current-buffer buffer
		 (goto-char (nth 3 ',el)))
	       (switch-to-buffer buffer)))
	  (format "%s" (nth 0 el))))))


  (defun my-org-journal-cleanup (orig-fun &rest args)
    "Cleanup after org journal by closing the buffers visited while getting agenda info."
    (let ((orig-buffers (buffer-list))
  	  (orig-val (apply orig-fun args))
  	  (new-buffers (buffer-list)))
      (mapc 'kill-buffer (cl-set-difference new-buffers orig-buffers))
      orig-val))

  (advice-add 'dashboard-get-agenda :around #'my-org-journal-cleanup)

  (defun dashboard-get-todo ()
    (org-compile-prefix-format 'agenda)
    (let* ((filtered-entries nil)
  	   (buffer-list nil))
      (org-map-entries
       (lambda ()
  	 (let* ((schedule-time (org-get-scheduled-time (point)))
  		(deadline-time (org-get-deadline-time (point)))
  		(item (org-agenda-format-item
  		       (format-time-string "%Y-%m-%d" schedule-time)
  		       (org-get-heading t t)
  		       (org-outline-level)
  		       (org-get-category)
  		       (org-get-tags)
  		       t))
  		(loc (point))
  		(file (buffer-file-name))
  		(buffer (current-buffer)))
  	   (setq buffer-list
  		 (append buffer-list (list buffer)))
  	   (when (and (not (org-entry-is-done-p))
  		      (org-entry-is-todo-p))
  	     (setq filtered-entries
  		   (append filtered-entries
  			   (list (list item schedule-time deadline-time loc file)))))))
       nil
       'agenda)
      (mapc 'kill-buffer buffer-list)
      filtered-entries))

  (defun dashboard-insert-todo (list-size)
    (require 'org-agenda)
    (require 'calendar)
    (dashboard-insert-section
     "TODOs:"
     (dashboard-subseq (dashboard-get-todo)
  		       0 list-size)
     list-size
     "t"
     `(lambda (&rest ignore)
  	(let ((buffer (find-file (nth 4 ',el))))
  	  (with-current-buffer buffer
  	    (goto-char (nth 3 ',el)))
  	  (switch-to-buffer buffer)))
     (format "%s" (nth 0 el))))

  (add-to-list 'dashboard-item-generators  '(todo . dashboard-insert-todo)))


(provide 'module-dashboard)

;;; module-dashboard ends here

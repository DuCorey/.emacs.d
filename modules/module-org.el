;;; module-org.el -- the many org related configurations

;;; Commentary:

;;; Code:

;; The main org package we also import the contribs for later use
(use-package org
  :ensure org-plus-contrib
  :hook
  ;; Line wrapping in org mode
  (org-mode . (lambda ()
		(visual-line-mode)
		(org-indent-mode)))
  :custom
  ;; Don't prompt for confirmation when evaluating code block
  (org-confirm-babel-evaluate nil)
  ;; Syntax highlight in #+BEGIN_SRC blocks
  (org-src-fontify-natively t)
  ;; Change tab behavior in source code to be like normal major mode
  (org-src-tab-acts-natively t)
  ;; Code minting
  (org-latex-listings 'minted)
  (org-latex-minted-options
   '(("fontsize" "\\scriptsize")))
  ;; Org agenda files should not be folded when opened
  ;; (org-agenda-inhibit-startup t)
  (org-startup-folded nil)
  ;; Avoid inadvertent text edit in invisible area
  (org-catch-invisible-edits 'show-and-error)
  ;; Hide empty lines between subtrees in collapsed view
  (org-cycle-seperator-lines 0)
  ;; Increase sub-item indentation
  (org-list-indent-offset 1)
  :bind (("C-c c" . org-capture))
  :config
  ;; Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (shell . t)
     (python . t)
     (js . t)))

  ;; Add R's <- in org source blocks
  (defun my/org-underscore-command ()
    (interactive)
    (or (org-babel-do-key-sequence-in-edit-buffer "_")
  	(org-self-insert-command 1)))
  (define-key org-mode-map "_" 'my/org-underscore-command)

  ;; display/update images in the buffer after I evaluate
  ;;(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; Minted for ipython
  ;;(add-to-list 'org-latex-minted-langs '(ipython "R"))
  ;; Did not work fixed in customize directly
  ;; Will have to double check if the changes to emacs jupter affected this
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-pdf-process
	(mapcar
	 (lambda (s)
           (replace-regexp-in-string "%latex " "%latex -shell-escape " s))
	 org-latex-pdf-process))

  ;; Export languages
  (require 'ox-beamer))

;; Easy templates
;;(require 'org-tempo)

;; change the look of org-bullets
(use-package org-bullets
  :after org
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;; Async org
;; (use-package ob-async
;;   :after org
;;   :custom
;;   (ob-async-no-async-languages-alist 'ipython
;; 				       'jupyter-R))

;; Jupyter kernels
;; (use-package ob-jupyter
;;   :after org
;;   :load-path "packages/ob-jupyter")

(use-package jupyter
  :after org
  :defer t)
;; :after ess
;; :config
;; ;(require 'ess-r-mode)
;;
;; ;(setq-default ess-local-customize-alist ess-r-customize-alist)
;; ;(setq-default inferior-ess-font-lock-keywords 'inferior-ess-r-font-lock-keywords))

(use-package org-drill
  :ensure nil
  :after org
  :defer t
  :config
  (add-to-list 'org-modules 'org-drill)
  :custom
  (org-drill-add-random-noise-to-intervals-p t)
  (org-drill-hint-seperator "||")
  (org-drill-left-cloze-delimiter "<[")
  (org-drill-right-cloze-delimiter "]>")
  (org-drill-learn-fraction 0.25))

(use-package org-journal
  :after org
  :custom
  (org-journal-dir "~/Dropbox/org-journal/")
  (org-journal-enable-agenda-integration t)
  (org-journal-carryover-items "")
  (org-journal-file-type 'weekly)
  :hook
  ;; We update the org-agenda files when initializing emacs so the the dashboard mode
  ;; has the right files
  (after-init . org-journal-update-org-agenda-files)
  :config
  (defun org-journal-new-deadline-entry (prefix &optional scheduled-time)
    "Create a new deadline entry in the future."
    (interactive "P")
    (let ((scheduled-time (or scheduled-time (org-read-date nil nil nil "Date:")))
	  (raw (prefix-numeric-value prefix)))
      (org-journal-new-entry (= raw 16) (org-time-string-to-time scheduled-time))
      (unless (= raw 16)
	(if (not prefix)
	    (insert "TODO "))
	(save-excursion
	  (insert "\nDEADLINE: <" scheduled-time ">")))))

  ;; By default the org-journal only looks to the future when updating the files.
  ;; This is inconvenient when todos are set in the past and not brought forward.
  ;; As such we have edited this function to look forever for those files.
  (eval-after-load "org-journal"
    '(defun org-journal-update-org-agenda-files ()
       "Adds the current and future journal files to `org-agenda-files', and cleans
out past org-journal files."
       (when org-journal-enable-agenda-integration
	 (let ((not-org-journal-agenda-files
		(seq-filter
		 (lambda (fname)
		   (not (string-match org-journal-file-pattern fname)))
		 (org-agenda-files)))
	       (org-journal-agenda-files
		(let* ((future (org-journal-read-period 'forever))
		       (beg (car future))
		       (end (cdr future)))
		  ;; TODO(cschwarzgruber): Needs to be adopted for weekly, monthly or yearly journal file type.
		  ;; We actually would need to limit the file scope, if we only want TODO's for today, and future.
		  (setcar (cdr beg) (1- (cadr beg)))
		  (org-journal-search-build-file-list
		   (org-journal-calendar-date->time beg)
		   (org-journal-calendar-date->time end)))))
	   (setq org-agenda-files (append not-org-journal-agenda-files
  					  org-journal-agenda-files))))))

  (defun org-journal-save-entry-and-exit()
    "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
    (interactive)
    (save-buffer)
    (kill-buffer))
  (define-key org-journal-mode-map (kbd "C-x C-s") 'org-journal-save-entry-and-exit)

  ;; Exclude journal files from recentf
  (add-to-list 'recentf-exclude (expand-file-name org-journal-dir)))


(provide 'module-org)

;;; module-org.el ends here

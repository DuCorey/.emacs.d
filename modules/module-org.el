;;; module-org.el -- the many org related configurations

;;; Commentary:

;;; Code:

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

  :bind (("C-c c" . org-capture))

  :config
  ;; Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (shell . t)
     (python . t)
     (jupyter . t)))

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
  (require 'ox-beamer)

  ;; Easy templates
  ;;(require 'org-tempo)

  ;; change the look of org-bullets
  (use-package org-bullets
    :after org
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  ;; Async org
  ;; (use-package ob-async
  ;;   :after org
  ;;   :custom
  ;;   (ob-async-no-async-languages-alist 'ipython
  ;; 				       'jupyter-R))

  ;; Jupyter kernels
  (use-package ob-jupyter
    :after org
    :load-path "packages/ob-jupyter")


  (use-package org-drill
    :ensure nil
    :config
    (add-to-list 'org-modules 'org-drill)
    :custom
    (org-drill-add-random-noise-to-intervals-p t)
    (org-drill-hint-seperator "||")
    (org-drill-left-cloze-delimiter "<[")
    (org-drill-right-cloze-delimiter "]>")
    (org-drill-learn-fraction 0.25))
)

(provide 'module-org)

;;; module-org.el ends here

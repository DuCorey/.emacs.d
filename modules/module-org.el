;;; module-org.el -- the many org related configurations

;;; Commentary:

;;; Code:

(use-package org
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

  :config
  ;; Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (shell . t)
     (ipython .t)))
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


  ;; change the look of org-bullets
  (use-package org-bullets
    :after org
    :hook
    (org-mode-hook . (lambda () (org-bullets-mode 1))))

  ;; Async org
  ;; (use-package ob-async
  ;;   :after org
  ;;   :custom
  ;;   (ob-async-no-async-languages-alist 'ipython
  ;; 				       'jupyter-R))

  ;; Jupyter kernels
  (use-package ob-ipython
    :after org
    :config
    ;;; Rewriting the original functions from the package to fix some bugs
    ;; This rewrites supresses the out in the the results output
    (defcustom ob-ipython-suppress-execution-count nil
      "If non-nil do not show the execution count in output."
      :group 'ob-ipython)

    (defun ob-ipython--process-response (ret fil result-type)
      (let ((result (cdr (assoc :result ret)))
	    (output (cdr (assoc :output ret))))
	(if (eq result-type 'output)
	    output
	  (ob-ipython--output output nil)
	  (s-concat
	   (if 'ob-ipython-supress-execution-count
	       ""
	     (format "# Out[%d]:\n" (cdr (assoc :exec-count ret))))
	   (s-join "\n" (->> (-map (-partial 'ob-ipython--render fil)
				   (list (cdr (assoc :value result))
					 (cdr (assoc :display result))))
			     (remove-if-not nil)))))))))


(provide 'module-org)

;;; module-org.el ends here

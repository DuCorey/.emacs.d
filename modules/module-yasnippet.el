;;; module-yasnippet --- code snippets

;;; Commentary:

;;; Code:

(use-package yasnippet
  :after company
  :bind (:map yas-minor-mode-map
  	 ("C-x y i" . yas-insert-snippet)
  	 ("C-x y e" . yas-expand)
;;	 ("C-<tab>" . yas-expand)
  	 ("C-x y n" . yas-new-snippet)
  	 ("C-x y f" . yas-visit-snippet-file)
  	 ("C-x y t" . yas-describe-tables))

  :config
  (yas-global-mode 1)

  ;; Make Yasnippet and company mode work better together
  ;; (advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))
  ;; (advice-add 'company-complete-common :after (lambda ()
  ;; 		  				(when (equal my-company-point (point))
  ;; 						  (yas-expand)))))
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend)
					       (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))


(provide 'module-yasnippet)

;;; module-yasnippet.el ends here

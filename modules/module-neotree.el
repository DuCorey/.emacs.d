;;; module-neotree.el --- file tree browser

;;; Comentary:

;;; Code:

;;;(global-set-key [f5] 'neotree-toggle)
(use-package neotree
  :bind (("<f5>" . neotree-toggle))
  :config
  (setq-default neo-show-hidden-files t)
  :custom
  (inhibit-compacting-font-caches t))


(provide 'module-neotree)

;;; module-neotree.el ends here

;;; module-neotree.el --- file tree browser

;;; Comentary:

;;; Code:

(use-package neotree
  :bind (("<f5>" . neotree-toggle))
  :config
  (setq-default neo-show-hidden-files t)
  :hook
  ;; Disable line-numbers minor mode for neotree
  (neo-after-create . (lambda (&optional dummy) (display-line-numbers-mode -1)))
  :custom
  (inhibit-compacting-font-caches t)
  ;; Every time when the neotree window is opened, let it find current file and jump to node.
  (neo-smart-open t))


(provide 'module-neotree)

;;; module-neotree.el ends here

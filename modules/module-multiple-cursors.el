;;; module-multiple-cursors.el --- Adding multiple cursors for editing

;;; Commentary:

;;; Code:

(use-package multiple-cursors
  :bind (("C-c m c" . 'mc/edit-lines)
	 ("C-c m n" . 'mc/mark-next-like-this)
	 ("C-c m p" . 'mc/mark-previous-like-this))
  :config
  (add-to-list 'mc/unsupported-minor-modes 'scroll-restore-mode))


(provide 'module-multiple-cursors)

;;; module-multiple-cursors.el ends here

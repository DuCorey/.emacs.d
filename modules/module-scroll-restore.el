;;; module-scroll-restore.el

;;; Commentary:

;; Allows you to scroll up and down a file while maintaining your cursor position

;;; Code:

;; TODO debug package. Doesn't feel like it works right most of the time.
(use-package scroll-restore
  :config
  (scroll-restore-mode 1)
  :custom
  ;; Allow scroll-restore to modify the cursor face
  (scroll-restore-handle-cursor t)
  ;; Jump back to the original cursor position after scrolling
  (scroll-restore-jump-back t)
  ;; Recenter the window when restoring the original position
  (scroll-restore-recenter t))


(provide 'module-scroll-restore)

;;; module-scroll-restore.el ends here

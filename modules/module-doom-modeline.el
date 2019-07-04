;;; module-auto-package-update.el --- minimalist modeline

;;; Commentary:

;;; Code:

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'relative-from-project))


(provide 'module-doom-modeline)

;;; module-doom-modeline.el ends here

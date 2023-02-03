;;; module-markdown --- edit markdown files

;;; Commentary:

;;; Code:

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(provide 'module-markdown)

;;; module-markdown ends here

;;; module-tree-sitter --- Tree sitter code navigation

;;; Commentary:

;;; Code:

(use-package tree-sitter
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-after-on-hook . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(provide 'module-tree-sitter)

;;; module-tree-sitter ends here

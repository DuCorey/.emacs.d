;;; module-nlinum.el --- display the line number for every file

;;; Commentary:

;;; Code:

(use-package nlinum
  :config
  (global-nlinum-mode t)
  :custom
  (nlinum-highlight-current-line t)
  ;; Add more space before a line begins. Fixes issue with Org mode going out of line
  (nlinum-format "%d "))


(provide 'module-nlinum)

;;; module-nlinum.el ends here

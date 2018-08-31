;;; core-windows.el --- Windows specific settings

;;; Commentary:

;;; Code:

;; Change windows button to super key when running emacs on windows
(setq w32-pass-lwindow-to-system nil
      w32-lwindow-modifier 'super)) ; left Windows key


(provide 'core-windows)

;;; core-windows.el ends here

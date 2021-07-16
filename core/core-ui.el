;;; core-ui.el -*- lexical-binding: t; -*-

;;; Commentary:

;; ui configuration

;;; Code:

;; Display the column number for the position of the cursor
(column-number-mode t)

;; Remove the scroll bar
(scroll-bar-mode -1)

;; ShowParenMode
(show-paren-mode 1)
(setq show-paren-delay 0
      show-paren-priority -50)

;; Line number
(global-display-line-numbers-mode)


(provide 'core-ui)

;;; core-ui.el ends here

;;; core-defuns.el --- personal defuns

;;; Commentary:

;;; Code:

(defun open-init-file ()
  (interactive)
  (find-file user-init-file))

;; Duplicate line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(global-set-key (kbd "C-c C-d") 'duplicate-line)

;; (defun my:select-current-line-and-forward-line (arg)
;;   "Select the current line and move the cursor by ARG lines IF
;; no region is selected.

;; If a region is already selected when calling this command, only move
;; the cursor by ARG lines."
;;   (interactive "p")
;;   (when (not (use-region-p))
;;     (forward-line 0)
;;     (set-mark-command nil))
;;   (forward-line arg))


(provide 'core-defuns)

;;; core-defuns.el ends here

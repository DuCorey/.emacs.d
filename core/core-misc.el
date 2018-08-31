;;; core-misc.el --- misc configuration for my emacs

;;; Commentary:

;;; Code:

;; Avoid asking for to follow symbolic link when original file is in VC
(setq vc-follow-symlinks t)

;; Fix C-i being the same as tab
(setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
(define-key input-decode-map "\C-i" [C-i])

;; Remove some training wheels
(put 'upcase-region 'disabled nil)

;; Remove trailing whitespace when saving a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Automatically indent code when yanked
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))


(provide 'core-misc)

;;; core-misc.el ends here

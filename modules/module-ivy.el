;;; module-ivy.el --- Ivy autocompletion framework

;;; Commentary:

;;; Code:

(use-package ivy
  :ensure counsel
  :ensure swiper
  :bind (("\C-s" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("<f6>" . ivy-resume)
	 ("C-x b" . ivy-switch-buffer)
	 ("C-x B" . ivy-switch-buffer-other-window)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> v" . counsel-describe-variable)
	 ("<f1> l" . counsel-find-library)
	 ("<f2> i" . counsel-info-lookup-symbol)
	 ("<f2> u" . counsel-unicode-char)
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 ("M-y" . counsel-yank-pop)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history)
	 :map ivy-minibuffer-map
	 ("C-m" . ivy-alt-done))
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (projectile-completion-system 'ivy)
  (ivy-count-format "(%d/%d) ")
  (ivy-display-style 'fancy)
  :config
  (ivy-mode 1)

  ;; Ivy-hydra
  (use-package ivy-hydra
    :after (ivy hydra))

  ;; Ivy-rich
  (use-package ivy-rich
    :after ivy
    :custom
    (ivy-rich-path-style 'abbrev)
    :config
    ;; All the icon support to ivy-rich
    (defun ivy-rich-switch-buffer-icon (candidate)
      "Try to find the icon for the buffer's `major-mode'.
If that fails look for an icon for the mode that the `major-mode' is derived from."
      (let ((icon (all-the-icons-icon-for-mode
		   (buffer-local-value 'major-mode (get-buffer candidate)))))
	(if (symbolp icon)
	    (all-the-icons-icon-for-mode 'fundamental-mode :face 'all-the-icons-white)
	  icon)))

    (setq ivy-rich--display-transformers-list
    	  '(ivy-switch-buffer
    	    (:columns
    	     ((ivy-rich-switch-buffer-icon (:width 2))
    	      (ivy-rich-candidate (:width 30))
    	      (ivy-rich-switch-buffer-size (:width 7))
    	      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
    	      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
    	      (ivy-rich-switch-buffer-project (:width 15 :face success))
    	      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
    	     :predicate
    	     (lambda (cand) (get-buffer cand)))))

    ;; Add custom icons for various modes that can break ivy-rich
    (add-to-list 'all-the-icons-mode-icon-alist '(ess-mode all-the-icons-fileicon "R" :face all-the-icons-lblue))

    (ivy-rich-mode 1))

  ;; SMEX
  ;; Sorting M-x by most used
  (use-package smex))


(provide 'module-ivy)

;;; module-ivy.el ends here

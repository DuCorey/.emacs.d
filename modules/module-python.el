;;; module-python.el --- Python related packages

;;; Commenary:

;;; Code:

;; Major mode settings for python
;; Use conda's root python env
;; This is incompatible with pyvevn
(setq python-shell-interpreter "python")
;;      python-shell-interpreter-args "console --simple-prompt")

;; elpy adds a lot of modules for working with python
(use-package elpy
  :defer t
  :config
  (elpy-enable)
  :custom
  (elpy-modules '(elpy-module-sane-defaults
		  elpy-module-company
		  elpy-module-eldoc
		  elpy-module-yasnippet)))

;; Virtual environment management for python
(use-package pyvenv
  :defer t
  :config
  ;; (cond (windows-system (setenv "WORKON_HOME" (concat "C:" (getenv "HOMEPATH") "\\Miniconda3\\envs")))
  ;; 	(linux-system
  ;; 	 (setenv "WORKON_HOME" "~/miniconda3/envs")
  ;; 	 (setenv "IPY_TEST_SIMPLE_PROMPT" "1")))
  (pyvenv-mode 1))
;; TODO fix virtual envs menu to only show up for python files

(defvar inject-decorator-history nil "Histor of inject-decorator")

(defun inject-decorator (decorator)
  "Inject decorator in function"
  (interactive (list (read-from-minibuffer "Function name: " (car inject-decorator-history) nil nil 'inject-decorator-history)))
  (left-char)
  (python-nav-forward-defun)
  (python-nav-forward-defun)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-for-tab-command)
  (insert decorator))

(defun add-arg-to-function ()
  (interactive)
  (python-nav-forward-defun)
  (er/expand-region 1)
  (let ((region (buffer-substring (region-beginning) (region-end))))
    (let ((args (split-string region "\\,")))
      (let ((newargs (setcdr args (push "conn: connection" (cdr args)))))
	(kill-region)
	(insert (s-join "," newargs))))))

(provide 'module-python)

;;; module-python.el ends here

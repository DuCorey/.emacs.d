;;; module-python.el --- Python related packages

;;; Commenary:

;;; Code:

(use-package python
 :custom
 ;; Use conda's root python env
 ;; This is incompatible with pyvevn
  (python-shell-interpreter "python"
   python-shell-interpreter-args "console --simple-prompt"))

(use-package elpy
  :config
  (elpy-enable)
  :custom
  (elpy-modules (quote
		 (elpy-module-sane-defaults elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet))))

;; Virtual environment management for python
(use-package pyvenv
  :config
  (cond (windows-system (setenv "WORKON_HOME" (concat "C:" (getenv "HOMEPATH") "\\Miniconda3\\envs")))
	(linux-system
	 (setenv "WORKON_HOME" "~/miniconda3/envs")
	 (setenv "IPY_TEST_SIMPLE_PROMPT" "1")))
  (pyvenv-mode 1)
  (pyvenv-workon-home))
;; TODO fix virtual envs menu to only show up for python files


(provide 'module-python)

;;; module-python.el ends here

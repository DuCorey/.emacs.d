;;; module-ess.el --- Emacs Speaks Statistics provides mode for R

;;; Commentary:

;;; Code:

(use-package ess
  :config
  ;; ESS bug?
  ;; This forces loading ess-remote functionality
  ;;(require 'essd-els)
  ;;(require 'ess-site)

  (defun R-AL-remote ()
    (interactive)
    (let ((inferior-R-program "/usr/local/anaconda3/envs/AL/bin/R"))
      (R)))

  ;; (defun R-screen-remote (&optional remote-host)
  ;;   "Connect to the remote-host's screen session running R"
  ;;   (interactive (list
  ;; 		  (read-from-minibuffer "R remote host: ")))
  ;;   (pop-to-buffer (make-comint "R-remote"
  ;; 				"ssh" nil "-t" "-t" remote-host
  ;; 				"TERM=xterm;"
  ;; 				"screen" "-dr" "R-AL"))
  ;;   (ess-remote (process-name (get-buffer-process (current-buffer))) "R")
  ;;   (setq comint-process-echoes t))

  (defun ess-dtach-remote (&optional remote-host session venv)
    "Connect to the remote-host's dtach R session and launch ESS."
    (interactive (list
		  (read-from-minibuffer "Remote host: ")
		  (read-from-minibuffer "Remote session: ")
		  (read-from-minibuffer "Virtual conda environment: ")))
    (pop-to-buffer (make-comint (concat "R-remote-" session)
				"ssh" nil "-t" "-t" remote-host
				(concat "source activate " venv ";")
				"/home/corey/bin/dtach" "-A" (concat "R-" session)
				"-z" "-E" "-r" "none"
				"R" "--no-readline"))
    (ess-remote (process-name (get-buffer-process (current-buffer))))
    (setq comint-process-echoes t))

  ;; Custom company config for ess
  (defun my-ess-config ()
    (make-variable-buffer-local 'company-backends)
    (add-to-list 'company-backends
		 '(company-R-args company-R-objects company-dabbrev-code company-yasnippet :separate)))
  (add-hook 'ess-mode-hook #'my-ess-config)

  ;; Advise the ess-send-string function
  (require 's)
  (defun my-ess-parse-print (args)
    (let* ((line (nth 1 args))
	   (var (s-trim (car (s-slice-at "<-" line)))))
      (setf (nth 1 args) (concat line "\n" "print(" var ")")))
    args)

  (defun my-ess-eval-print-line (&optional vis)
      "Similar to lisp eval-print function, this function evaluates the
line and prints the result. It does this by intercepting the call to ess-send-string,
parsing it to see what variable was declared and adding a print statement with the variable."
      (interactive)
      (advice-add 'ess-send-string :filter-args #'my-ess-parse-print)
      (ess-eval-region-or-line-visibly-and-step)
      (advice-remove 'ess-send-string #'my-ess-parse-print))

  :custom
  (comint-scroll-to-bottom-on-output t)
  (comint-scroll-to-bottom-on-input t)
  (comint-move-point-for-output t)
  ;; Turn off default company since we have custom company backends
  (ess-use-company nil)

  :hook
  ;; Add local keybiding for ess-help when in ess-mode (r files)  or iESS (interactive console)
  ;; Restore "_" to <- (why would they ever remove this)
  ((ess-mode inferior-ess-mode ess-help-mode) . (lambda ()
						  (local-set-key (kbd "C-h C-r") #'ess-help)
						  (local-set-key (kbd "_") #'ess-insert-assign))))


(provide 'module-ess)

;;; module-ess.el ends here

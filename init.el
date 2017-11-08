(require 'package)
(setq package-enable-at-startup nil)
(let* ((no-ssl (and (eq system-type 'windows-nt)
		    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;; Enter debug mode on error
(setq debug-on-error t)


;; Custom functions	   
(defun open-init-file ()
  (interactive)
  (find-file user-init-file))


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Global ensure for use-package
(setq use-package-always-ensure t)


;; Try package let's you try out a package before installing it
(use-package try)


;; Which key shows you available key prompts
(use-package which-key
  :config
  (which-key-mode))


;; Doom-theme changes the look of Emacs to Atom's one dark
(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t)	; if nil, bold is universally disabled
  (setq doom-themes-enable-itable t)	; if nil, italics is universally disabled
  (setq doom-one-brighter-comments t)   ; comments will be highlighted in more vivid colors
  (setq doom-one-padded-modeline t)     ; Adds a 4px padding to the mode-line. Can be an integer to determine the exact padding.
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (doom-themes-neotree-config))


;; ShowParenMode
(show-paren-mode 1)
(setq show-paren-delay 0)


;; Display the line number for every file
(use-package nlinum
  :config
  (setq nlinum-highlight-current-line t)
  (global-nlinum-mode t)
  ;; Add more space before a line begins. Fixes issue with Org mode going out of line
  (setq nlinum-format "%d "))


;; Display the column number for the position of the cursor
(setq column-number-mode t)


;; Remove the scroll bar
(scroll-bar-mode -1)


;; Org mode
;; Org-bullets
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;; Magit a git porcelain
(use-package magit)


;; Keybindings
;; Add move commands to C + s + ijkl
(global-set-key (kbd "C-s-l") 'forward-char)
(global-set-key (kbd "C-s-j") 'backward-char)
(global-set-key (kbd "C-s-k") 'next-line)
(global-set-key (kbd "C-s-i") 'previous-line)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

 
;; Python
;; Default python shell is ipython if it can be found
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i"))

;; Virtual environment management
(use-package pyvenv
  :config
  (if (eq system-type 'windows-nt)
    (setenv "WORKON_HOME" (concat "C:" (getenv "HOMEPATH") "\\Miniconda3\\envs")))
  (pyvenv-mode 1))


;; Neotree browser
(use-package neotree
  :config
  (setq inhibit-compacting-font-caches t))


;; This section is generated automatically by emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (haskell-mode nlinum which-key try use-package doom-themes)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "simp" :slant normal :weight normal :height 113 :width normal)))))

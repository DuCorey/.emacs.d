(require 'package)
(setq package-enable-at-startup nil)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

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
  (doom-themes-visual-bell-config))

;; ShowParenMode
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Display the line number for every file
(use-package nlinum
  :config
  (setq nlinum-highlight-current-line t)
  (global-nlinum-mode t)
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

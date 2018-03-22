(require 'package)
(setq package-enable-at-startup nil)

;; Save the OS type to simplify later calls based on OS
(defvar windows-system nil "If the current system is running under windows")
(defvar linux-system nil "If the current system is running under linux")
(defvar mac-system nil "If the current system is running under macos")

(cond ((eq system-type 'windows-nt) (setq windows-system t))
      ((eq system-type 'gnu/linux) (setq linux-system t))
      ((eq system-type 'darwin) (setq mac-system t)))


(let* ((no-ssl (and windows-system
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

;; Duplicate line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-c C-d") 'duplicate-line)


;; Macros
(defmacro add-hook! (hook &rest func-or-forms)
  "A convenience macro for `add-hook'.
HOOK can be one hook or a list of hooks. If the hook(s) are not quoted, -hook is
appended to them automatically. If they are quoted, they are used verbatim.
FUNC-OR-FORMS can be a quoted symbol, a list of quoted symbols, or forms. Forms will be
wrapped in a lambda. A list of symbols will expand into a series of add-hook calls.
Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))"
  (declare (indent defun) (debug t))
  (unless func-or-forms
    (error "add-hook!: FUNC-OR-FORMS is empty"))
  (let* ((val (car func-or-forms))
         (quoted (eq (car-safe hook) 'quote))
         (hook (if quoted (cadr hook) hook))
         (funcs (if (eq (car-safe val) 'quote)
                    (if (cdr-safe (cadr val))
                        (cadr val)
                      (list (cadr val)))
                  (list func-or-forms)))
         (forms '()))
    (mapc
     (lambda (f)
       (let ((func (cond ((symbolp f) `(quote ,f))
                         (t `(lambda (&rest _) ,@func-or-forms)))))
         (mapc
          (lambda (h)
            (push `(add-hook ',(if quoted h (intern (format "%s-hook" h))) ,func) forms))
          (-list hook)))) funcs)
    `(progn ,@forms)))


;; Functions
(defun my:select-current-line-and-forward-line (arg)
  "Select the current line and move the cursor by ARG lines IF
no region is selected.

If a region is already selected when calling this command, only move
the cursor by ARG lines."
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))


;; Fix C-i being the same as tab
(setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
(define-key input-decode-map "\C-i" [C-i])


;; Bootstrap 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Global ensure for use-package
(setq use-package-always-ensure t)


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
  (doom-themes-neotree-config)
  (setq doom-neotree-file-icons t))


;; ShowParenMode
(show-paren-mode 1)
(setq show-paren-delay 0
      show-paren-priority -50)


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
(use-package org
  :config
  ;; Line wrapping in org mode
  (add-hook 'org-mode-hook (lambda ()
			     (visual-line-mode)
			     (org-indent-mode))))

;; Org-bullets
(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;; Magit a git porcelain
(use-package magit)


;; Haskell
(use-package haskell-mode)


;; Restart emacs
;; Add functions to restart emacs from within emacs
(use-package restart-emacs)


;; Keybindings
;; Add move commands to C + s + ijkl
(global-set-key (kbd "C-s-i") 'previous-line)
(global-set-key (kbd "C-s-l") 'forward-char)
(global-set-key (kbd "C-s-j") 'backward-char)
(global-set-key (kbd "C-s-k") 'next-line)

;; Change windows button to super key when running emacs on windows
(if windows-system
  (setq w32-pass-lwindow-to-system nil
        w32-lwindow-modifier 'super)) ; left Windows key

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Neotree
(global-set-key [f5] 'neotree-toggle)


;; Python
(use-package python
  :config
  (setq python-shell-interpreter "jupyter"
	python-shell-interpreter-args "console --simple-prompt"))
;;   (define-key python-mode-map (kbd "C-c <C-i>") 'run-ipython))
(use-package elpy
  :config
  (elpy-enable)
  (setq elpy-modules (quote
		      (elpy-module-sane-defaults elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet))))


;; Open an IPython repl
;; (defun run-ipython ()
;;   (interactive)
;;   (if (executable-find "ipython")
;;       (progn
;; 	(run-python "ipython -i")
;; 	(let ((w (split-window-horizontally)))
;; 	  (set-window-buffer w "*Python*")))
;;     (progn
;;       (ding)
;;       (message "Could not find IPython in environment %s" pyvenv-virtual-env-name))))


;; Virtual environment management
(use-package pyvenv
  :config
  (cond (windows-system (setenv "WORKON_HOME" (concat "C:" (getenv "HOMEPATH") "\\Miniconda3\\envs")))
	(linux-system
	 (setenv "WORKON_HOME" "~/miniconda3/envs")
	 (setenv "IPY_TEST_SIMPLE_PROMPT" "1")))
  (pyvenv-mode 1))


;; Neotree file browser
(use-package neotree
  :config
  (setq inhibit-compacting-font-caches t)
  (setq-default neo-show-hidden-files t))


;; Company-mode autocomplete
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 3
	company-show-numbers t
	company-global-modes '(not inferior-python-mode)))

;; Python Autocomplete
;; (use-package company-anaconda
;;   :after company
;;   :init
;;   (add-hook 'python-mode-hook 'anaconda-mode)
;;   :config
;;   (add-to-list 'company-backends '(company-anaconda :with company-capf))


;; Start screen dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)
			  (bookmarks . 5)
			  (projects . 5)
			  (agenda . 5))))


;; Projectile project management
(use-package projectile
  :config
  (setq projectile-mode 1))


;; Git gutter display git changes on the left of the window
(use-package git-gutter
  :init
  (add-hook! (text-mode prog-mode conf-mode) 'git-gutter-mode)
  :ensure git-gutter-fringe
  :config
  (require 'git-gutter-fringe)

  ;; NOTE If you want the git gutter to be on the outside of the margins (rather
  ;; than inside), `fringes-outside-margins' should be non-nil.

  ;; colored fringe "bars"
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center)

  ;; Refresh git gutter
  (setq git-gutter:update-interval 1))


;; Expand region
(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))


;; Scroll restore
;; Allows you to scroll up and down a file while maintaining your cursor
(use-package scroll-restore
  :init
  ;; Allow scroll-restore to modify the cursor face
  (setq scroll-restore-handle-cursor t)
  ;; Jump back to the original cursor position after scrolling
  (setq scroll-restore-jump-back t)
  ;; Recenter the window when restoring the original position
  (setq scroll-restore-recenter t)
  :config
  (scroll-restore-mode 1))


;; Emacs Speaks Statistics
(use-package ess)


;; Auto-package-update
(use-package auto-package-update
  :config
  (setq auto-package-update-prompt-before-update t
	auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))


;; Emacs backups
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))

(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq backup-by-copying t  ; don't clobber symlinks
      delete-old-versions t  ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-new-versions 6
      kept-old-versions 2
      version-control t  ; version numbers for backup files
      )


;; Custom faces
;; (custom-set-faces
;;   '(flymake-errline ((((class color)) (:underline nil))))
;;   '(flymake-warnline ((((class color)) (:underline nil))))
;;   '(highlight-indentation-face ((((class color)) (:background "#21242b")))))

;; Set custom file that is modified automatically by customization done through emacs
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)


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


;; Fix C-i being the same as tab
(setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
(define-key input-decode-map "\C-i" [C-i])


;; Bootstrap 'use-package
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


;; Keybindings
;; Add move commands to C + s + ijkl
(global-set-key (kbd "C-s-i") 'previous-line)
(global-set-key (kbd "C-s-l") 'forward-char)
(global-set-key (kbd "C-s-j") 'backward-char)
(global-set-key (kbd "C-s-k") 'next-line)

;; Change windows button to super key when running emacs on windows
(if (eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil
        w32-lwindow-modifier 'super)) ; left Windows key


;; Magit
(global-set-key (kbd "C-x g") 'magit-status)


;; Neotree
(global-set-key [f5] 'neotree-toggle)

 
;; Python
(use-package python
  :config
  (define-key python-mode-map (kbd "C-c <C-i>") 'run-ipython))

;; Open an IPython repl
(defun run-ipython ()
  (interactive)
  (if (executable-find "ipython")
      (progn
	(run-python "ipython -i")
	(let ((w (split-window-horizontally)))
	  (set-window-buffer w "*Python*")))
    (progn
      (ding)
      (message "Could not find IPython in environment %s" pyvenv-virtual-env-name))))

;; Virtual environment management
(use-package pyvenv
  :if (eq system-type 'windows-nt)
  :config
  (setenv "WORKON_HOME" (concat "C:" (getenv "HOMEPATH") "\\Miniconda3\\envs"))
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
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3))


;; Start screen dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))


;; Projectile project management
(use-package projectile)


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
  (setq git-gutter:update-interval 0.2))

(use-package ein)

;; Set custom file that is modified automatically by customization done through emacs
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

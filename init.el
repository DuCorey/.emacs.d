;;; init.el --- initialize my emacs config

;;; Commentary:

;; My emacs init.el. It configured such that all packages used are
;; grouped into either core or modules.

;;; Code:

;;---------------------------------------------------------------------
;; Init
;;---------------------------------------------------------------------

;; Enter debug mode on error
(setq debug-on-error t)

;; Save the OS type to simplify later calls based on OS
(defvar windows-system nil "If the current system is running under windows")
(defvar linux-system nil "If the current system is running under linux")
(defvar mac-system nil "If the current system is running under macos")

(cond ((eq system-type 'windows-nt) (setq windows-system t))
      ((eq system-type 'gnu/linux) (setq linux-system t))
      ((eq system-type 'darwin) (setq mac-system t)))

;;---------------------------------------------------------------------
;; Variables and Load Paths
;;---------------------------------------------------------------------

(defvar my/emacs-dir (file-name-directory "~/.emacs.d/init.el")
  "The root dir of the Emacs distribution.")

(defvar my/core-dir (expand-file-name "core" my/emacs-dir)
  "The home of core functionality.")

(defvar my/modules-dir (expand-file-name "modules" my/emacs-dir)
  "This directory houses all of the modules.")

(defvar my/backup-dir (expand-file-name "backups" my/emacs-dir)
  "This directory houses all the backup files")

;;(defvar my/chinese-dir (expand-file-name "chinese" my/emacs-dir))

;; add directories to Emacs's load-path
(add-to-list 'load-path my/core-dir)
(add-to-list 'load-path my/modules-dir)
;;(add-to-list 'load-path my/chinese-dir)

;;---------------------------------------------------------------------
;; Core
;;---------------------------------------------------------------------

(message "Loading core...")
(require 'core-bootstrap)
(require 'core-defuns)
(require 'core-ui)
(require 'core-paths)
(require 'core-misc)

(if windows-system
    (require 'core-windows))

;;---------------------------------------------------------------------
;; Modules
;;---------------------------------------------------------------------

(message "Loading modules...")
;; Emacs additions
(require 'module-restart-emacs)
(require 'module-recentf)
(require 'module-auto-package-update)

;; Editing
(require 'module-orderless)
(require 'module-snippet)
(require 'module-wgrep)

;; UI/UX
(require 'module-hydra)
(require 'module-which-key)
(require 'module-doom-themes)
(require 'module-doom-modeline)
(require 'module-all-the-icons)
(require 'module-neotree)
(require 'module-dashboard)
(require 'module-vertico)
(require 'module-consult)
(require 'module-embark)
(require 'module-corfu)
(require 'module-tabs)
(require 'module-helpful)
(require 'module-multiple-cursors)
(require 'module-smartparens)
(require 'module-expand-region)


;; Programming
(require 'module-tree-sitter)
(require 'module-git)
(require 'module-haskell)
(require 'module-python)
(require 'module-ess)
(require 'module-markdown)
(require 'module-yaml)
(require 'module-typescript)
(require 'module-json)
(require 'module-racket)
(require 'module-restclient)

;; Org
(require 'module-org)


;;(require 'josh-chinese)

(message "Ready!")

;;; init.el ends here

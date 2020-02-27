;;; core-boostrap.el --- bootstrap for use-package

;;; Commentary:

;; Setup Emacs package repos and grab use-package for rest of package
;; management.

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)

(let* ((no-ssl (and windows-system
		    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; Org mode packages
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Bootstrap 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Global ensure for use-package
(setq use-package-always-ensure t)

;; Bootstrap for 'straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default nil)


(provide 'core-bootstrap)

;;; core-bootstrap.el ends here

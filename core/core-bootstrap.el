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

;; Bootstrap 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Global ensure for use-package
(setq use-package-always-ensure t)


(provide 'core-bootstrap)

;;; core-bootstrap.el ends here

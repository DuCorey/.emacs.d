;;; core-paths.el --- path configuration for my emacs

;;; Commentary:

;;; Code:

;; Emacs backups
(if (not (file-exists-p my/backup-dir))
    (make-directory my/backup-dir t))

(setq backup-directory-alist `(("." . ,my/backup-dir)))
(setq backup-by-copying t  ; don't clobber symlinks
      delete-old-versions t  ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-new-versions 6
      kept-old-versions 2
      version-control t  ; version numbers for backup files
      )

;; Set custom file that is modified automatically by customization done through emacs
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)


(provide 'core-paths)

;;; core-paths.el ends here

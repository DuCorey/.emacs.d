;;; module-doom-themes.el --- Doom-theme changes the look of Emacs to Atom's one dark

;;; Commentary:

;;; Code:

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t	; if nil, bold is universally disabled
	doom-themes-enable-itable t	; if nil, italics is universally disabled
	doom-one-brighter-comments t    ; comments will be highlighted in more vivid colors
	doom-one-padded-modeline t)     ; Adds a 4px padding to the mode-line. Can be an integer to determine the exact padding.
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (doom-themes-neotree-config)
  :custom
  (doom-neotree-file-icons t))


(provide 'module-doom-themes)

;;; module-doom-themes.el ends here

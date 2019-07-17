;;; module-tabber.el --- Emacs has tabs

;;; Commentary: We have one version using centaur tabs and another combining
;;; tabbar with powerline

;;; Code:

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode 1)
  ;(centaur-tabs-inherit-tabbar-faces)
  (centaur-tabs-headline-match)
  :bind
  ("C-<tab>" . centaur-tabs-forward)
  ("C-<iso-lefttab>" . centaur-tabs-backward)
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-bar 'left)
  (centaur-tabs-set-modified-marker t)
  :hook
  ;; Remove tabs from specific modes
  (dashboard-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (org-journal-mode . centaur-tabs-local-mode)
  (help-mode . centaur-tabs-local-mode))
;  (helpful-mode . centaur-tabs-local-mode))

;; (use-package tabbar
;;   :config
;;   ;; Change padding of the tabs
;;   ;; we also need to set separator to avoid overlapping tabs by highlighted tabs
;;   (customize-set-variable 'tabbar-background-color "#282c34")
;;   (customize-set-variable 'tabbar-separator '(0.5))
;;   (customize-set-variable 'tabbar-use-images nil)
;;   (setq tabbar-button-widget nil)
;;   (tabbar-mode 1)

;;   ;; Keys
;;   (define-key global-map [(C-tab)] 'tabbar-forward)
;;   (define-key global-map [(C-iso-lefttab)] 'tabbar-backward)

;;   ;; Colors
;;   (set-face-attribute 'tabbar-default nil
;; 		      :background "#282c34" :foreground
;; 		      "#282c34" :distant-foreground "#282c34"
;; 		      :box nil)
;;   (set-face-attribute 'tabbar-unselected nil
;; 		      :background "#21242b" :foreground "#bbc2cf" :box nil)
;;   (set-face-attribute 'tabbar-modified nil
;; 		      :foreground "#ff6c6b" :box nil
;; 		      :inherit 'tabbar-unselected)
;;   (set-face-attribute 'tabbar-selected nil
;; 		      :background "#32363e" :foreground "#bbc2cf" :box nil)
;;   (set-face-attribute 'tabbar-selected-modified nil
;; 		      :inherit 'tabbar-selected :foreground "#ff6c6b" :box nil)
;;   (set-face-attribute 'tabbar-button nil
;; 		      :box nil))


;; (use-package powerline
;;   :after tabbar
;;   :config
;;   (defvar my/tabbar-height 25)
;;   (defvar my/tabbar-left (powerline-wave-right 'tabbar-default nil my/tabbar-height))
;;   (defvar my/tabbar-right (powerline-wave-left nil 'tabbar-default my/tabbar-height))
;;   (defun my/tabbar-tab-label-function (tab)
;;     (powerline-render (list my/tabbar-left
;; 			    (format " %s  " (car tab))
;; 			    my/tabbar-right)))
;;   (setq tabbar-tab-label-function #'my/tabbar-tab-label-function))


(provide 'module-tabs)

;;; module-tabbar.el ends here

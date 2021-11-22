;;; module-git.el --- git related configurations

;;; Commentary:

;;; Code:

;; Git gutter display git changes on the left of the window if file is being tracked in git
(use-package git-gutter
  :ensure git-gutter-fringe
  :hook
  ((test-mode prog-mode conf-mode) . git-gutter-mode)
  :init
  (require 'git-gutter-fringe)
  :config
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

  (set-face-foreground 'git-gutter:modified "#da8548")
  (set-face-foreground 'git-gutter-fr:modified "#da8548")

  :custom
  ;; Refresh git gutter
  (git-gutter:update-interval 1))

;; Magit a git porcelain
(use-package magit
  :bind(("C-x g" . magit-status)))


(provide 'module-git)

;;; module-git ends here

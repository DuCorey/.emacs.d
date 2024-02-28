;;; module-hydra.el --- Create keymaps

;;; Commentary:

;;; Code:

(use-package hydra
  :config
  :bind ("C-c y" . hydra-yasnippet/body))

(defhydra hydra-yasnippet (:color blue :hint nil)
  "
              ^YASnippets^
--------------------------------------------
 Load/Visit:      Actions:

 [_d_] Directory    [_i_] Insert
 [_f_] File         [_t_] tryout
 [_l_] List         [_n_] New
                  ^[_a_] Auto
"
  ("q" nil "quit")
  ("d" yas-load-directory)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("a" hydra-yasnippet-auto/body :exit t))

(defhydra hydra-yasnippet-auto (:color blue :hint nil)
  "
              ^Auto YASnippets^
--------------------------------------------
 Actions:

_c_reate
_e_xpand
"
  ("q" nil "quit" :exit t)
  ("c" aya-create)
  ("e" aya-expand))

(provide 'module-hydra)

;;; module-hydra.el ends here

  (use-package ripgrep
    :straight t)
  (use-package rg
    :straight t)
  (use-package projectile
    :straight t
    :ensure t)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(provide 'ary-projectile)

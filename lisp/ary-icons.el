;;; Icons
(use-package nerd-icons
  :straight t)

(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :straight t
  :config             
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))
(provide 'ary-icons)

(use-package ef-themes
  :straight t
  :if window-system
  :ensure t)
(use-package doom-themes
  :straight t
  :ensure t)
(use-package modus-themes
  :straight t)
(load-theme 'modus-operandi-tinted t)

(add-to-list 'default-frame-alist
       '(font . "JetBrains Mono-14"))

(use-package mood-line
  :straight t
  :ensure t
  :init (mood-line-mode 1))
(provide 'ui)

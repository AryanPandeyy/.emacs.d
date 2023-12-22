(use-package ef-themes
  :straight t
  :if window-system
  :ensure t)
(use-package doom-themes
  :straight t
  :ensure t)
(use-package modus-themes
  :straight t)
(load-theme 'modus-vivendi-tinted t)

(add-to-list 'default-frame-alist
             '(font . "JetBrains Mono-14"))
(defun rha/toggle-modus-themes ()
  "Allow quick toggling between dark and light themes."
  (interactive)
  (if (member 'modus-operandi-tinted custom-enabled-themes)
      (progn
        (load-theme 'modus-vivendi-tinted t)
        (disable-theme 'modus-operandi-tinted))
    (load-theme 'modus-operandi-tinted t)
    (disable-theme 'modus-vivendi-tinted)))

(global-set-key (kbd "C-c C-t") 'rha/toggle-modus-themes)
(use-package mood-line
  :straight t
  :ensure t)
(provide 'ui)

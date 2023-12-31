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
(set-frame-parameter nil 'alpha-background 70)

(add-to-list 'default-frame-alist '(alpha-background . 70))
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
  :config
  (mood-line-mode)
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code "Use Fira Code-compatible decorative glyphs")
  (mood-line-format
   (mood-line-defformat
    :left
    (((mood-line-segment-buffer-status) . " ")
     ((mood-line-segment-buffer-name) . "  ")
     ((mood-line-segment-anzu) . "  ")
     ((mood-line-segment-multiple-cursors) . "  ")
     ((mood-line-segment-cursor-position) . " ")
     (mood-line-segment-scroll))
    :right
    (((mood-line-segment-vc) . "  ")
     ((mood-line-segment-misc-info) . "  ")
     ((mood-line-segment-checker) . "  ")
     ((mood-line-segment-process) . "  ")))))

(provide 'ui)

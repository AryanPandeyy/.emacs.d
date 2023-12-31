(use-package denote
  :straight t)
(setq denote-directory (expand-file-name "~/org"))
;;(setq denote-file-type 'markdown-yaml)
(use-package markdown-mode
  :straight t)
(provide 'ary-denote)

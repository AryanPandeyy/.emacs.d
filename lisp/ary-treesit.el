(use-package treesit
:ensure nil
:custom
(treesit-font-lock-feature-list t)
(treesit-font-lock-level 4))
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(provide 'ary-treesit)

(use-package flymake-eslint
  :straight t)
(add-hook 'tsx-ts-mode-hook ; or whatever the mode-hook is for your mode of choice
(lambda ()
  (flymake-eslint-enable)))
(provide 'ary-flymake)

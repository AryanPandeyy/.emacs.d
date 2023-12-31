(use-package flymake-eslint
  :straight t)
(add-hook 'tsx-ts-mode-hook ; or whatever the mode-hook is for your mode of choice
          (lambda ()
            (flymake-eslint-enable)))
;; https://github.com/mohkale/flymake-collection
;; TODO: flymake checkers for non-lsp files: yaml, xml, json...
(use-package flymake-collection
  :defer t)
(provide 'ary-flymake)

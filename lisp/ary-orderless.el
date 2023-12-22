(use-package orderless
  :straight t
  :init
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))
(provide 'ary-orderless)

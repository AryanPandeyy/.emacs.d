(require 'eglot)
(fset #'jsonrpc--log-event #'ignore)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
;;  (add-hook 'java-ts-mode-hook 'eglot-ensure)
(set-default 'indent-tabs-mode nil)
(use-package apheleia
  :straight t)
(apheleia-global-mode t)
(setq java-ts-mode-indent-offset 2)
;; https://www.leemeichin.com/posts/my-emacs-config.html
(use-package asdf
  :straight (:type git :host github :repo "tabfugnic/asdf.el"))
(require 'asdf)
(asdf-enable)
(provide 'ary-eglot)

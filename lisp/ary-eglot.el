(require 'eglot)
(use-package eglot
  :bind
  (("C-c l q" . eglot-shutdown)
   ("C-c l Q" . eglot-shutdown-all)
   ("C-c l d" . eglot-find-declaration)
   ("C-c l i" . eglot-find-implementation)
   ("C-c l t" . eglot-find-typeDefinition)
   ("C-c l r" . eglot-rename)
   ("C-c l f" . eglot-format)
   ("C-c l F" . eglot-format-buffer)
   ("C-c l x" . eglot-code-actions))
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-size 0 "Drop jsonrpc log to improve performance"))

(fset #'jsonrpc--log-event #'ignore)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
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
(use-package eldoc
  :straight t
  :ensure nil
  :config
  (setq eldoc-idle-delay 0
        eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly
        eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-display-truncation-message nil))

(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-mode))
(provide 'ary-eglot)

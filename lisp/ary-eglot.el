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
  :ensure nil
  :defer t
  :config
  ;; (global-eldoc-mode -1)
  ;; `eldoc-echo-area-use-multiline-p'
  (setq eldoc-idle-delay 0.5)
  (eldoc-add-command-completions "delete-char" "lispy-delete-backward" "puni-backward-delete-char")
  (set-face-foreground 'eldoc-highlight-function-argument
                       (face-attribute 'font-lock-variable-name-face :foreground)))

(use-package eldoc-box
  :straight t
  :bind ("C-M-;" . eldoc-box-help-at-point)
  ;;  :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  ;; :custom
  ;; (eldoc-box-only-multi-line nil)
  ;; (eldoc-box-clear-with-C-g t)
  ;; :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
  :config
  ;; Prettify `eldoc-box' frame
  (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
        (alist-get 'right-fringe eldoc-box-frame-parameters) 8))

(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-mode))

;; https://github.com/jacktasia/dumb-jump#obsolete-commands-and-options
(use-package dumb-jump
  :straight t
  :after xref
  :config
  (setq dump-jump-prefer-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(provide 'ary-eglot)

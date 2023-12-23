(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (js-ts-mode . lsp)
	 (tsx-ts-mode . lsp)
	 (jsx-ts-mode . lsp)
	 (java-ts-mode . lsp)))
;; if you want which-key integration
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-auto-execute-action nil)
(setq lsp-enable-on-type-formatting nil)
(setq java-ts-mode-indent-offset 2)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(use-package flycheck
  :straight t
  :ensure t
  :init (global-flycheck-mode))

(use-package apheleia
  :straight t)
(apheleia-global-mode +1)

(use-package lsp-java
  :straight t)

(use-package dap-mode
  :straight t)

;; (use-package which-key
;;   :straight t
;;   :config
;;   (which-key-mode))

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . js-ts-mode))

(add-to-list 'auto-mode-alist '("\\.java?\\'" . java-ts-mode))
(provide 'ary-lsp)

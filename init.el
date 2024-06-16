(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq make-backup-files nil)
(setq auto-save-default nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode -1)
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(global-subword-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(set-frame-parameter nil 'alpha-background 70)

(add-to-list 'default-frame-alist '(alpha-background . 70))
;; Tab and Space
;; Permanently indent with spaces, never with TABs
;; M-^ delete-indentation
(setq-default c-basic-offset   2
              tab-width        4
              indent-tabs-mode nil)

(use-package project
  :straight t
  :ensure t)

(use-package flycheck
  :straight t
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;apheleia
(use-package apheleia
  :straight t
  :config
  (apheleia-global-mode t))
(setq java-ts-mode-indent-offset 2)

;;fonts
(add-to-list 'default-frame-alist
             '(font . "JetBrains Mono-14"))

;;lang
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

(use-package yasnippet
  :straight t)
(yas-global-mode 1)

;;magit
(use-package magit
  :straight t)

;;tree-sitter
(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-feature-list t)
  (treesit-font-lock-level 4))
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Enable vertico
(use-package vertico
  :straight t
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-resize t)
  (setq vertico-cycle t))

(use-package ef-themes
  :straight t
  :ensure t)
(load-theme 'modus-operandi t)

;; multiple cursors used by rexim
(use-package multiple-cursors
  :straight t
  :ensure t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;; marginalia to desc option
(use-package marginalia
  :straight t
  :ensure t
  :init
  (marginalia-mode))

(use-package orderless
  :straight t
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

;;; Icons
(use-package nerd-icons
  :straight t
  :ensure t)

(use-package nerd-icons-completion
  :straight t
  :ensure t
  :custom
  (nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode 1))

(use-package nerd-icons-dired
  :straight t
  :ensure t
  :config
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package doom-modeline
  :straight t
  :ensure t
  :init
  (doom-modeline-mode 1))

(use-package company
  :straight t
  :ensure t
  :config (global-company-mode t))
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast
(use-package lsp-mode
  :straight t
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((tsx-ts-mode . lsp)
         (jsx-ts-mode . lsp)
         (typescript-ts-mode . lsp)
         (js-ts-mode . lsp)
         (solidity-mode . lsp)
         (php-mode . lsp)
         (go-ts-mode . lsp)
         (rust-ts-mode . lsp))
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-links nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-lens-enable nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-headerline-breadcrumb-icons-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-keep-workspace-alive nil))

(use-package lsp-java
  :straight t
  :ensure t)
(add-hook 'java-ts-mode-hook #'lsp)

(use-package lsp-treemacs
  :straight t
  :ensure t)

(use-package dap-mode
  :straight t
  :ensure t)

;; (use-package spacious-padding
;;   :straight t
;;   :ensure t
;;   :config
;;   (spacious-padding-mode 1))

(use-package solidity-mode
  :straight t
  :ensure t)

(use-package php-mode
  :straight t
  :ensure t)

(add-to-list 'apheleia-formatters '(rustfmt . ("rustfmt" "--quiet" "--emit" "stdout" "--edition" "2021")))

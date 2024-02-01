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
(load-theme 'modus-vivendi-tinted t)
;; for grid view
(ido-mode)
(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))
;; for M-x mode
;;(fido-vertical-mode)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
;; M-^ delete-indentation
(setq-default c-basic-offset   2
              tab-width        4
              indent-tabs-mode nil)

;; projectile
(use-package ripgrep
  :straight t)
(use-package rg
  :straight t)
(use-package projectile
  :straight t
  :ensure t)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;eglot
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
  (eglot-extend-to-xref t)
  (eglot-events-buffer-size 0 "Drop jsonrpc log to improve performance"))
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'java-ts-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs
             `((c++-ts-mode c-ts-mode) . ("clangd"))
             `(rust-ts-mode . ("rust-analyzer" :initializationOptions
                               (:procMacro (:enable t)
                                           :cargo (:buildScripts (:enable t)
                                                                 :features
                                                                 "all")))))

;;apheleia
(use-package apheleia
  :straight t)
(apheleia-global-mode t)
(setq java-ts-mode-indent-offset 2)

;;fonts
(add-to-list 'default-frame-alist
             '(font . "Iosevka-16"))

;;dape
(use-package dape
  :straight t)
(add-to-list 'eglot-server-programs
             `((java-mode java-ts-mode) .
               ("jdtls"
                :initializationOptions
                (:bundles ["/home/ap/.local/share/nvim/mason/share/java-debug-adapter/com.microsoft.java.debug.plugin-0.47.0.jar"]))))

;;lang
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

;; python
(use-package pet
  :straight t
  :config
  (add-hook 'python-ts-mode-hook 'pet-mode -10))
(use-package flymake-ruff
  :straight t
  :hook (eglot-managed-mode . flymake-ruff-load))

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

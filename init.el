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
;; Tab and Space
;; Permanently indent with spaces, never with TABs
;; M-^ delete-indentation
(setq-default c-basic-offset   2
              tab-width        4
              indent-tabs-mode nil)

(use-package project
  :straight t
  :ensure t)

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
  :straight t
  :ensure t)

;;tree-sitter
(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-feature-list t)
  (treesit-font-lock-level 4))

;; Enable vertico
(use-package vertico
  :straight t
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-resize t)
  (setq vertico-cycle t))

(load-theme 'modus-vivendi-deuteranopia t)

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

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

(add-to-list 'apheleia-formatters '(rustfmt . ("rustfmt" "--quiet" "--emit" "stdout" "--edition" "2021")))

(use-package markdown-mode
  :straight t
  :ensure t)

(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(java-ts-mode . ("/usr/share/java/jdtls/bin/jdtls"
                                 "-data"
                                 "/home/ap/.emacs.d/.cache/lsp/jdtls"))))
(provide 'init)

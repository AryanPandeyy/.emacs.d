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

(use-package corfu
  :straight t
  :ensure t
  :init
  (global-corfu-mode))
(define-key corfu-map (kbd "<tab>") #'corfu-complete)
(setq eldoc-message-function #'message)
(add-hook 'prog-mode-hook #'eldoc-mode)

(use-package cape
  :straight t
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package eglot
  :straight t
  :ensure t
  :bind
  (("C-c l q" . eglot-shutdown)
   ("C-c l Q" . eglot-shutdown-all)
   ("C-c l d" . eglot-find-declaration)
   ("C-c l i" . eglot-find-implementation)
   ("C-c l t" . eglot-find-typeDefinition)
   ("C-c l r" . eglot-rename)
   ("C-c l f" . eglot-format)
   ("C-c l F" . eglot-format-buffer)
   ("C-c l a" . eglot-code-actions))
  :custom
  (eglot-extend-to-xref t))

(use-package flycheck
  :straight t
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-eglot
  :straight t
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'java-ts-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(require 'eglot)
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
             '(font . "Iosevka-14"))

;;dape
(use-package dape
  :straight t)
(add-to-list 'eglot-server-programs
             `((java-mode java-ts-mode) .
               ("jdtls"
                :initializationOptions
                (:bundles ["/home/ap/.local/share/nvim/mason/share/java-debug-adapter/com.microsoft.java.debug.plugin-0.50.0.jar"]))))

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

;; kanagawa theme
;; (use-package kanagawa-theme
;;   :straight t
;;   :ensure t)
(load-theme 'modus-vivendi t)

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

(use-package nerd-icons-corfu
  :straight t
  :ensure t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

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
(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

;;; Whitespace mode
(defun rc/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))
(setq whitespace-style  '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark))
(add-hook 'prog-mode-hook 'rc/set-up-whitespace-handling)

;;(setq whitespace-style '(trailing space-mark))

(load-file "~/.emacs.d/sol.el")

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
(setq initial-scratch-message "")

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'ary-corfu)
(require 'ary-dape)
(require 'ary-denote)
(require 'ary-dired)
(require 'ary-eglot)
(require 'ary-flymake)
(require 'ary-lang)
(require 'ary-magit)
(require 'ary-marginalia)
(require 'ary-notmuch)
(require 'ary-orderless)
(require 'ary-projectile)
(require 'ary-snippet)
(require 'ary-treesit)
(require 'ary-vertico)
(require 'ui)


(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
;; (setq java-ts-mode-indent-offset 2)

;;fonts
(add-to-list 'default-frame-alist
             '(font . "JetBrains Mono-16"))

;; theme
(load-theme 'modus-vivendi t)

;;lang
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;; (add-hook 'rust-ts-mode-hook 'eglot-ensure)
;; (add-hook 'c++-ts-mode-hook 'eglot-ensure)
;; (add-hook 'c-ts-mode-hook 'eglot-ensure)
;; (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
;; (add-hook 'js-ts-mode-hook 'eglot-ensure)
;; (add-hook 'html-ts-mode-hook 'eglot-ensure)
;; (add-hook 'java-ts-mode-hook 'eglot-ensure)
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                `(java-ts-mode . ("//home/ap/.local/src/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/bin/jdtls"
;;                                  "--jvm-arg=-javaagent:/home/ap/.local/src/eclipse.jdt.ls/lombok.jar"
;;                                  "-data"
;;                                  "/home/ap/.emacs.d/.cache/lsp/jdtls"))))
;; packages

;; magit
(use-package magit
  :ensure t)

(use-package ef-themes
  :ensure t)

(use-package spacious-padding
  :ensure t
  :hook (after-init . spacious-padding-mode)
  :custom
  ;; make the modeline look minimal
  (spacious-padding-subtle-mode-line '( :mode-line-active default
                                        :mode-line-inactive vertical-border)))

(use-package notmuch
  :ensure t
  :bind (("C-c m" . notmuch-hello)))

(setq notmuch-show-logo nil
      notmuch-column-control 1.0
      notmuch-hello-auto-refresh t
      notmuch-hello-thousands-separator ""
      notmuch-always-prompt-for-sender t
      notmuch-archive-tags '("-inbox" "-unread")
      notmuch-hello-sections
      '(notmuch-hello-insert-saved-searches
        notmuch-hello-insert-alltags)
      notmuch-show-all-tags-list t)
(setq message-kill-buffer-on-exit t)
(setq notmuch-message-deleted-tags '("+trash" "-inbox" "-unread" "-new"))
(setq sendmail-program "gmi")
(setq send-mail-function 'sendmail-send-it)
(setq message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/dox/mail"))
(setq notmuch-fcc-dirs nil)

(defun jab/notmuch-search-message-delete (go-next)
  "Delete message and select GO-NEXT message."
  (notmuch-search-tag notmuch-message-deleted-tags)
  (if (eq 'up go-next)
      (notmuch-search-previous-thread)
    (notmuch-search-next-thread)))

(defun jab/notmuch-search-message-delete-down ()
  "Delete a message and select the next message."
  (interactive)
  (jab/notmuch-search-message-delete 'down))

(define-key notmuch-search-mode-map (kbd "D") 'jab/notmuch-search-message-delete-down)

(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '(("https://xcancel.com/LifeMathMoney/rss" x)
          "https://lifemathmoney.com/feed/"
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA" yt))))
(global-set-key (kbd "C-x w") 'elfeed)

(use-package elcord
  :ensure t)

(use-package notmuch-indicator
  :ensure t)
(setq notmuch-indicator-args
      '((:terms "tag:unread and tag:inbox" :label "U" :label-face success)
        (:terms "tag:unread and tag:important" :label "P" :label-face warning :counter-face inherit))
      notmuch-indicator-refresh-count (* 60 3)
      notmuch-indicator-hide-empty-counters t
      notmuch-indicator-force-refresh-commands '(notmuch-refresh-this-buffer))
(notmuch-indicator-mode 1)

(use-package telega
  :ensure t
  :commands (telega)
  :defer t)
(add-hook 'telega-load-hook 'telega-notifications-mode)
(setq telega-server-libs-prefix "/usr/")
(global-set-key (kbd "C-c t") 'telega)

(use-package pdf-tools
  :ensure t)

(use-package notmuch-notify
  :load-path "~/.emacs.d/notmuch-notify"
  :hook (notmuch-hello-refresh . notmuch-notify-hello-refresh-status-message)
  :config
  (notmuch-notify-set-refresh-timer))

(use-package alert
  :ensure t)

(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

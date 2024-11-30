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
(setq-default notmuch-search-oldest-first nil)
;; (setq notmuch-search-oldest-first nil)
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
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA" yt)
          ("http://www.youtube.com/feeds/videos.xml?channel_id=UCyzf9_LUFN-2Xr30z8hyW1A" life)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCngn7SVujlvskHRvRKc1cTw" devs)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA" devs)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA" devs)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC5KDiSAFxrDWhmysBcNqtMA" devs)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCrqM0Ym_NbK1fqeQG2VIohg" devs)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCyb_ckNk0RIVBz1vW8wNo2Q" devs)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2Qw36lBG3P6y6KmOu8JHhw" devs)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqR1s3Jy4bRMN9c5XGLuLOQ" devs)
          ("https://blog.dornea.nu/feed.xml" blog)
          ("https://www.neilhenning.dev/index.xml" devs)
          ("https://pwy.io/feed.xml" devs)
          )))
(global-set-key (kbd "C-x w") 'elfeed)
(defun browse-url-mpv (url &optional new-window)
    (start-process "mpv" "*mpv*" "mpv" url))
(setq browse-url-handlers '(("https:\\/\\/www\\.youtube." . browse-url-mpv)
    ("." . browse-url-firefox)))

(use-package telega
  :ensure t
  :commands (telega)
  :defer t)
(add-hook 'telega-load-hook 'telega-notifications-mode)
(setq telega-server-libs-prefix "~/.emacs.d/td/tdlib/")
(global-set-key (kbd "C-c t") 'telega)

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-cycle t))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrride nil))

(use-package markdown-mode
  :ensure t)
'(markdown-header-delimiter-face :foreground "#616161" :height 0.9)
'(markdown-header-face-1 :height 1.8 :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face)
'(markdown-header-face-2 :height 1.4 :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face)
'(markdown-header-face-3 :height 1.2 :foreground "#D08770" :weight extra-bold :inherit markdown-header-face)
'(markdown-header-face-4 :height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face)
'(markdown-header-face-5 :height 1.1 :foreground "#b48ead" :weight bold :inherit markdown-header-face)
'(markdown-header-face-6 :height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face)

(use-package elcord
  :ensure t)

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

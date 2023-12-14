(use-package notmuch
  :straight t)
  ;;;; General UI
(setq notmuch-show-logo nil
      notmuch-hello-auto-refresh t
      notmuch-hello-thousands-separator ""
      notmuch-show-all-tags-list t)
(setq message-kill-buffer-on-exit t)
(setq sendmail-program "gmi")
(setq send-mail-function 'sendmail-send-it)
(setq message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/mail"))
(setq notmuch-fcc-dirs nil) ;; let gmail take care of sent mail
(provide 'ary-notmuch)

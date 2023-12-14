(use-package notmuch
  :straight t)
  ;;;; General UI
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
(setq sendmail-program "gmi")
(setq send-mail-function 'sendmail-send-it)
(setq message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/mail"))
(setq notmuch-fcc-dirs nil) ;; let gmail take care of sent mail
(provide 'ary-notmuch)

(use-package corfu
  :straight t
  ;; Optional customizations
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-popupinfo-delay (cons nil 1.0))
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))
(corfu-popupinfo-mode +1)
(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (eval-after-load 'eglot
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))
(provide 'ary-corfu)

(use-package vertico
;; :commands vertico-mode
:straight t
:after minibuffer
:commands vertico-mode
:init (vertico-mode 1)
:config
(setq vertico-count 10
vertico-cycle t
vertico-resize t)
(advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
(advice-add #'ffap-menu-ask :around
(lambda (&rest args)
  (cl-letf (((symbol-function #'minibuffer-completion-help)
       #'ignore))
    (apply args)))))

(use-package vertico-repeat
:after vertico
:hook (minibuffer-setup . vertico-repeat-save)
:config
(use-package savehist
  :defer
  :config
  (add-to-list 'savehist-additional-variables
   'vertico-repeat-history)))

(use-package vertico-reverse
;; :disabled
:after vertico)

(use-package vertico-buffer
:after vertico
:config
(setq vertico-buffer-display-action 'display-buffer-reuse-window))
(provide 'ary-vertico)

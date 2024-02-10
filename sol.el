(use-package solidity-mode
  :straight t)
(use-package solidity-flycheck
  :straight t)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(solidity-mode . ("nomicfoundation-solidity-language-server" "--stdio"))))
(provide 'sol)

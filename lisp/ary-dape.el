(use-package dape
  :straight t)
(require 'eglot)
(add-hook 'java-ts-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs
             `((java-mode java-ts-mode) .
               ("jdtls"
                :initializationOptions
                (:bundles ["/home/ap/.local/share/nvim/mason/share/java-debug-adapter/com.microsoft.java.debug.plugin-0.47.0.jar"]))))
(provide 'ary-dape)

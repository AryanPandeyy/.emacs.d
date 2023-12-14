(use-package dape
:straight (dape :type git :host github :repo "svaante/dape"
                    :fork (:host github
                           :repo "MagielBruntink/dape")))
(require 'eglot)
(add-hook 'java-ts-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs
           `((java-mode java-ts-mode) .
             ("jdtls"
              :initializationOptions
              (:bundles ["/home/ap/.emacs.d/jdtls/java-debug/com.microsoft.java.debug.plugin/target/com.microsoft.java.debug.plugin-0.50.0.jar"]))))
(provide 'ary-dape)

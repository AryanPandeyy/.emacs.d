(require 'eglot)
(fset #'jsonrpc--log-event #'ignore)

;; https://github.com/xiaoxinghu/system/blob/main/m1/emacs/init.org
;; https://www.reddit.com/r/emacs/comments/11bqzvk/emacs29_and_eglot_inlay_hints/
(add-to-list
 'eglot-server-programs
 '((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode jsx-mode)
   "typescript-language-server" "--stdio"
   :initializationOptions
   (:preferences
    (
     ;; https://github.com/microsoft/TypeScript/blob/main/src/server/protocol.ts#L3410-L3539
     :disableSuggestions                                    :json-false     ;; boolean
     :quotePreference                                       "single"        ;; "auto" | "double" | "single"
     :includeCompletionsForModuleExports                    t               ;; boolean
     :includeCompletionsForImportStatements                 t               ;; boolean
     :includeCompletionsWithSnippetText                     t               ;; boolean
     :includeCompletionsWithInsertText                      t               ;; boolean
     :includeAutomaticOptionalChainCompletions              t               ;; boolean
     :includeCompletionsWithClassMemberSnippets             t               ;; boolean
     :includeCompletionsWithObjectLiteralMethodSnippets     t               ;; boolean
     :useLabelDetailsInCompletionEntries                    t               ;; boolean
     :allowIncompleteCompletions                            t               ;; boolean
     :importModuleSpecifierPreference                       "shortest"      ;; "shortest" | "project-relative" | "relative" | "non-relative"
     :importModuleSpecifierEnding                           "minimal"       ;; "auto" | "minimal" | "index" | "js"
     :allowTextChangesInNewFiles                            t               ;; boolean
     ;; :lazyConfiguredProjectsFromExternalProject                          ;; boolean
     :providePrefixAndSuffixTextForRename                   t               ;; boolean
     :provideRefactorNotApplicableReason                    :json-false     ;; boolean
     :allowRenameOfImportPath                               t               ;; boolean
     ;; :includePackageJsonAutoImports                                      ;; "auto" | "on" | "off"
     :jsxAttributeCompletionStyle                           "auto"          ;; "auto" | "braces" | "none"
     :displayPartsForJSDoc                                  t               ;; boolean
     :generateReturnInDocTemplate                           t               ;; boolean
     :includeInlayParameterNameHints                        "all"           ;; "none" | "literals" | "all"
     :includeInlayParameterNameHintsWhenArgumentMatchesName t               ;; boolean
     :includeInlayFunctionParameterTypeHints                t               ;; boolean,
     :includeInlayVariableTypeHints                         t               ;; boolean
     :includeInlayVariableTypeHintsWhenTypeMatchesName      t               ;; boolean
     :includeInlayPropertyDeclarationTypeHints              t               ;; boolean
     :includeInlayFunctionLikeReturnTypeHints               t               ;; boolean
     :includeInlayEnumMemberValueHints                      t               ;; boolean
     ;; :autoImportFileExcludePatterns                                      ;; string[]
     ;; :organizeImportsIgnoreCase                                          ;; "auto" | boolean
     ;; :organizeImportsCollation                                           ;; "ordinal" | "unicode"
     ;; :organizeImportsCollationLocale                                     ;; string
     ;; :organizeImportsNumericCollation                                    ;; boolean
     ;; :organizeImportsAccentCollation                                     ;; boolean
     ;; :organizeImportsCaseFirst                                           ;; "upper" | "lower" | false
     :disableLineTextInReferences                           :json-false))))


(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
;;  (add-hook 'java-ts-mode-hook 'eglot-ensure)
(set-default 'indent-tabs-mode nil)
(use-package apheleia
  :straight t)
(apheleia-global-mode t)
(setq java-ts-mode-indent-offset 2)
;; https://www.leemeichin.com/posts/my-emacs-config.html
(use-package asdf
  :straight (:type git :host github :repo "tabfugnic/asdf.el"))
(require 'asdf)
(asdf-enable)
(provide 'ary-eglot)

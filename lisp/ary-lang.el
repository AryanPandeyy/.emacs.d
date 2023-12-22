(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . js-ts-mode))

(add-to-list 'auto-mode-alist '("\\.java?\\'" . java-ts-mode))

(require 'eglot)
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


(use-package  prisma-ts-mode
  :straight (prisma-ts-mode :type git :host github :repo "nverno/prisma-ts-mode")
  :mode "\\.prisma\\'"
  :hook (prisma-ts-mode . eglot-ensure))
(add-to-list 'eglot-server-programs '(prisma-ts-mode . ("prisma-language-server" "--" "--stdio")))

(provide 'ary-lang)

(define-library (gypsum backend guile editor)
  (import
    (scheme base)
    (only (gypsum pretty) pretty print)
    (only (gypsum editor)
          current-editor
          editor-messages
          insert)
    (prefix (gypsum editor-impl) *impl/)
   )

  (export
   command-error-default-function
   )

  (include "editor.scm") 
  )

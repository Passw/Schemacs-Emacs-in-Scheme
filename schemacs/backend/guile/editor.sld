(define-library (schemacs backend guile editor)
  (import
    (scheme base)
    (only (schemacs pretty) pretty print)
    (only (schemacs editor)
          current-editor
          editor-messages
          insert)
    (prefix (schemacs editor-impl) *impl/)
   )

  (export
   command-error-default-function
   )

  (include "editor.scm") 
  )

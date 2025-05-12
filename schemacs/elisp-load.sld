(define-library (schemacs elisp-load)
  (import
    (scheme base)
    (scheme case-lambda)
    (only (scheme write)
          display
          write
          )
    (only (chibi match) match)
    (only (schemacs pretty)
          print-to-port
          pretty   print  qstr   line-break
          )
    (only (schemacs lens) view)
    (only (schemacs lens vector)
          mutable-vector-type?
          new-mutable-vector
          mutable-vector-length
          mutable-vector-append!
          =>mvector-index!
          )
    (only (schemacs elisp-eval pretty)
          elisp-print
          )
    (only (schemacs elisp-eval environment)
          new-empty-environment
          *elisp-input-port*
          *elisp-output-port*
          *elisp-error-port*
          )
    (only (schemacs elisp-eval)
          new-environment
          *the-environment*
          eval-iterate-forms
          elisp-eval!
          elisp-load!
          )
    )
  (export
   *verbose*
   elisp-form-buffer
   current-elisp-form-buffer
   elisp-view-form-buffer
   elisp-load-form-buffer!
   elisp-load!
   *the-environment*
   new-environment
   elisp-view-forms vf
   )
  (include "elisp-load.scm")
  )

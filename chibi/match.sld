(define-library (chibi match)
  (import
    (scheme base)
    )
  (export
   match match-lambda match-lambda*
   match-let match-letrec match-let*
   )
  (include "match.scm")
  )



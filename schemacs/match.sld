(define-library (schemacs match)
  (cond-expand
    (guile (import (chibi match)))
    (gambit (import (termite match)))
    (else (import (chibi match)))
    )
  (export
   match  match-lambda  match-lambda*
   match-let  match-let*  match-letrec
   ))

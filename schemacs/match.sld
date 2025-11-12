(define-library (schemacs match)
  (cond-expand
    (chez (import (chibi match)))
    (chicken (import (matchable)))
    (guile (import (chibi match)))
    (gambit (import (termite match)))
    (else (import (chibi match)))
    )
  (export
   match  match-lambda  match-lambda*
   match-let  match-let*  match-letrec
   ))

(define-library (gypsum test)
  ;; Re-export APIs from SRFI-64 that are used in unit tests across
  ;; this project after importing them from the appropriate library
  ;; module depending on which Scheme implementation is being used to
  ;; run this program.
  ;;------------------------------------------------------------------
  (cond-expand
    ((or guile gambit gauche) (import (srfi 64)))
    ((or mit rapid) (import (rapid test)))
    (chibi (import (chibi test)))
    ((library (srfi 64)) (import (srfi 64)))
    (else (import (rapid test)))
    )
  (export
   test-begin  test-end  test-skip
   test-error  test-assert
   test-equal  test-eqv  test-eq
   ))

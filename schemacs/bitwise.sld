(define-library (schemacs bitwise)
  (cond-expand
    ((or guile (library (srfi 60)))
     (import
       (only (srfi 60)
             bit-set?
             bitwise-ior  bitwise-and  bitwise-xor
             arithmetic-shift
             ))
     )
    (gambit
     ;; do nothing: SRFI 60 APIs are built-in to Gambit,
     ;; but the library (srfi 60) is not provided.
     )
    ((or chibi stklos (library (srfi 151)))
     (import
       (only (srfi 151)
             bit-set?
             bitwise-ior  bitwise-and  bitwise-xor
             arithmetic-shift
             ))
     ))
  (cond-expand
    (mit
     (export)
     )
    (guile
     ;; Guile BUG? Try commenting out this (guile ...) condition.
     ;; Even if you do, Guile does not evaluate "else" condition
     ;; below, although it should. This (guile ...) condition is a
     ;; copy of the (else ...) condition below pasted here since
     ;; Guile's `COND-EXPAND` implementation seems to be not working
     ;; according to spec. The `COND-EXPAND` implementation used in
     ;; `DEFINE-LIBRARY` seems to be broken, but the `COND-EXPAND`
     ;; implementation used elsewhere in Guile works just fine.
     (export
      bit-set?
      bitwise-ior  bitwise-and  bitwise-xor
      arithmetic-shift
      ))
    (else
     (export
      bit-set?
      bitwise-ior  bitwise-and  bitwise-xor
      arithmetic-shift
      ))))

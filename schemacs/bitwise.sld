(define-library (schemacs bitwise)
  (cond-expand
    ((or guile (library (srfi 60)))
     (import
       (only (srfi 60) ; Integers as Bits
             bit-set?
             bitwise-ior  bitwise-and  bitwise-xor
             arithmetic-shift
             ))
     )
    ((or mit gambit)
     ;; do nothing: SRFI 60 APIs are built-in to Gambit,
     ;; but the library (srfi 60) is not provided.
     )
    ((or chibi (library (srfi 151)))
     (import
       (only (srfi 151) ; Integers as Bits
             bit-set?
             bitwise-ior  bitwise-and  bitwise-xor
             arithmetic-shift
             ))
     ))
  (cond-expand
    ((or mit gambit)
     )
    (guile
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
      ))
    ))

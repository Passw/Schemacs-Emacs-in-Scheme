(define-library (schemacs bit-stack)
  ;; A stack of bits packed into a bytevector. This is used by Emacs
  ;; Lisp Environment objects when tracking flags on stack frames.
  ;;------------------------------------------------------------------

  (import
    (scheme base)
    (scheme case-lambda)
    )

  (cond-expand
    (mit
     ;; MIT/GNU Scheme should not import the (schemacs bitwise)
     ;; library because all of these APIs are built-in.
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
     (import
       (only (schemacs bitwise)
             bit-set?
             bitwise-and  bitwise-ior  bitwise-xor
             arithmetic-shift
             )))
    (else
     (import
       (only (schemacs bitwise)
             bit-set?
             bitwise-and  bitwise-ior  bitwise-xor
             arithmetic-shift
             )))
    )

  (export
   new-bit-stack
   bit-stack-count
   bit-stack-push!
   bit-stack-look
   bit-stack-pop!
   bit-stack-ref
   )

  (include "bit-stack.scm")
  )

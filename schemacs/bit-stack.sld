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

  (begin

    (define-record-type <bit-stack-type>
      (make<bit-stack> count bitvec)
      bit-stack-type?
      (count   bit-stack-count   set!bit-stack-count)
      (bitvec  bit-stack-bitvec  set!bit-stack-bitvec)
      )

    (define new-bit-stack
      (case-lambda
        (() (new-bit-stack 64))
        ((size) (make<bit-stack> 0 (make-bytevector size)))
        ))

    (define (%bit-stack-ref stack)
      (lambda (vec-ix byte-ix)
        (let*((vec (bit-stack-bitvec stack))
              (mask (arithmetic-shift 1 byte-ix))
              (cell (bytevector-u8-ref vec vec-ix))
              )
          (bitwise-and mask cell)
          )))

    (define (bitmap-index-pair i) (truncate/ i 8))

    (define (nonzero? i) (not (= 0 i)))

    (define (bit-stack-push! stack . bits)
      (let loop ((bits bits) (stack stack))
        (cond
         ((null? bits) stack)
         (else
          (let*-values
              (((bit) (car bits))
               ((vec) (bit-stack-bitvec stack))
               ((len) (bytevector-length vec))
               ((top) (bit-stack-count stack))
               ((vec-ix byte-ix) (bitmap-index-pair top))
               ((mask) (arithmetic-shift 1 byte-ix))
               ((realloced vec)
                (if (< vec-ix len)
                    (values #f vec)
                    (let*((newvec (make-bytevector (* 2 len))))
                      (bytevector-copy! newvec 0 vec)
                      (values #t newvec)
                      )))
               ((cell) (bitwise-ior mask (bytevector-u8-ref vec vec-ix)))
               ((cell) (if bit cell (bitwise-xor mask cell)))
               )
            (bytevector-u8-set! vec vec-ix cell)
            (when realloced (set!bit-stack-bitvec stack vec))
            (set!bit-stack-count stack (+ 1 top))
            (loop (cdr bits) stack)
            )))))

    (define (bit-stack-look stack)
      (nonzero?
       (let-values
           (((vec-ix byte-ix)
             (bitmap-index-pair (- (bit-stack-count stack) 1))))
         ((%bit-stack-ref stack) vec-ix byte-ix)
         )))

    (define (bit-stack-ref stack i)
      (let*-values
          (((vec) (bit-stack-bitvec stack))
           ((len) (bytevector-length vec))
           ((vec-ix byte-ix) (bitmap-index-pair i))
           )
        (nonzero? ((%bit-stack-ref stack) vec-ix byte-ix))
        ))

    (define (bit-stack-pop! stack)
      (let ((top (bit-stack-count stack)))
        (cond
         ((> top 0)
          (let ((bit (bit-stack-look stack)))
            (set!bit-stack-count stack (- top 1))
            bit
            ))
         (else (error "stack underflow" stack))
         )))

    ;;----------------------------------------------------------------
    ))

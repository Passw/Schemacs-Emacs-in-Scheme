(define-library (schemacs vector)

  ;; This library exists to provide a uniform interface to common APIs
  ;; that might be defined in different places depending on which
  ;; Scheme implementation is running this code. APIs exported here
  ;; can be imported exactly once per environment elsehwere in this
  ;; program without having to write a `COND-EXPAND` statement
  ;; everywhere one of these APIs are used.

  (import (scheme base))
  (export
   vector-fold
   )

  (cond-expand

    (guile

     (import
       (rename (srfi 43)
               (vector-fold old-vector-fold)))
     (begin
      (define (vector-fold kons knil . vecs)
        ;; Re-define `VECTOR-FOLD` such that it's API is identical to
        ;; that of SRFI-133. To do this, the `INDEX` argument is no
        ;; longer passed to the `KONS` procedure on each iteration.
        (apply old-vector-fold (lambda (_index accum . elems) (apply kons accum elems)) knil vecs)
        )
       ))

    ((or mit (library (srfi 133)))
     (import (only (srfi 133) vector-fold))
     )

    (else

     (begin

       (define (vector-fold kons knil . veclist)
         ;; This is an implementation of SRFI-133 `VECTOR-FOLD` written using
         ;; only APIs exposed by the `(SCHEME BASE)` library defined by the
         ;; R7RS Scheme standard.
         ;;------------------------------------------------------------------
         (let ((minlen
                (let loop ((veclist veclist) (minlen #f))
                  (cond
                   ((null? veclist) (if minlen minlen 0))
                   (else
                    (let ((thislen (vector-length (car veclist))))
                      (loop (cdr veclist)
                            (if minlen (min thislen minlen) thislen)))))))
               )
           (cond
            ((= 0 minlen) knil)
            (else
             (let loop ((i 0) (state knil))
               (cond
                ((>= i minlen) state)
                (else
                 (loop (+ 1 i)
                       (apply kons state
                              (map (lambda (vec) (vector-ref vec i))
                                   veclist))))))))
           ))

       ))))

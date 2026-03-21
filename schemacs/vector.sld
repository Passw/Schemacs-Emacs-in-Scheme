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
               (vector-fold old-vector-fold)
               ))
     (begin

      (define (vector-fold kons knil . vecs)
        ;; Re-define `VECTOR-FOLD` such that it's API is identical to
        ;; that of SRFI-133. To do this, the `INDEX` argument is no
        ;; longer passed to the `KONS` procedure on each iteration.
        (apply
         old-vector-fold
         (lambda (_index accum . elems) (apply kons accum elems))
         knil vecs
         ))

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
                      (loop
                       (cdr veclist)
                       (if minlen (min thislen minlen) thislen)
                       )))))))
           (cond
            ((= 0 minlen) knil)
            (else
             (let loop ((i 0) (state knil))
               (cond
                ((>= i minlen) state)
                (else
                 (loop
                  (+ 1 i)
                  (apply
                   kons state
                   (map (lambda (vec) (vector-ref vec i))
                        veclist
                        ))))))))))

       )))

  (cond-expand

    ((or (library (srfi 4))
         (library (srfi 160))
         )
     (import
       (only
        (srfi 4)
        s16vector? make-s16vector s16vector s16vector-length
        s16vector-ref s16vector-set! s16vector->list list->s16vector

        u16vector? make-u16vector u16vector u16vector-length
        u16vector-ref u16vector-set! u16vector->list list->u16vector

        s32vector? make-s32vector s32vector s32vector-length
        s32vector-ref s32vector-set! s32vector->list list->s32vector

        u32vector? make-u32vector u32vector u32vector-length
        u32vector-ref u32vector-set! u32vector->list list->u32vector

        s64vector? make-s64vector s64vector s64vector-length
        s64vector-ref s64vector-set! s64vector->list list->s64vector

        u64vector? make-u64vector u64vector u64vector-length
        u64vector-ref u64vector-set! u64vector->list list->u64vector

        f32vector? make-f32vector f32vector f32vector-length
        f32vector-ref f32vector-set! f32vector->list list->f32vector

        f64vector? make-f64vector f64vector f64vector-length
        f64vector-ref f64vector-set! f64vector->list list->f64vector
        )))

    (export
     s16vector? make-s16vector s16vector s16vector-length
     s16vector-ref s16vector-set! s16vector->list list->s16vector

     u16vector? make-u16vector u16vector u16vector-length
     u16vector-ref u16vector-set! u16vector->list list->u16vector

     s32vector? make-s32vector s32vector s32vector-length
     s32vector-ref s32vector-set! s32vector->list list->s32vector

     u32vector? make-u32vector u32vector u32vector-length
     u32vector-ref u32vector-set! u32vector->list list->u32vector

     s64vector? make-s64vector s64vector s64vector-length
     s64vector-ref s64vector-set! s64vector->list list->s64vector

     u64vector? make-u64vector u64vector u64vector-length
     u64vector-ref u64vector-set! u64vector->list list->u64vector

     f32vector? make-f32vector f32vector f32vector-length
     f32vector-ref f32vector-set! f32vector->list list->f32vector

     f64vector? make-f64vector f64vector f64vector-length
     f64vector-ref f64vector-set! f64vector->list list->f64vector
     )

    ))

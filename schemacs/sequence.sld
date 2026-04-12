(define-library (schemacs sequence)

  ;; A generic function dispatch interface for sequential data
  ;; structures such as lists and vectors.

  (import
    (scheme base)
    (scheme case-lambda)
    (schemacs vector)
    )

  (cond-expand
    ((library (srfi 160))
     (import (srfi 160))
     )
    ((library (srfi 4))
     (import (srfi 4))
     ))

  (export
   make-seq  seq-length  seq-ref  seq-set!
   seq-for-each  seq->list  list->seq
   typeof-vector?
   seq-step-forward  seq-step-forward/index
   seq-step-reverse  seq-step-reverse/index
   seq-min-max

   get-sequence-iface  sequence-iface-type?
   iface-sequence  iface-make-sequence
   iface-sequence-length  iface-sequence-ref
   iface-sequence-set!  iface-sequence-copy!
   iface-sequence->list  iface-list->sequence
   iface-sequence-for-each

   string-sequence-iface
   bytevector-sequence-iface
   vector-sequence-iface
   u8vector-sequence-iface
   u16vector-sequence-iface
   u32vector-sequence-iface
   u64vector-sequence-iface
   s8vector-sequence-iface
   s16vector-sequence-iface
   s32vector-sequence-iface
   s64vector-sequence-iface
   f32vector-sequence-iface
   f64vector-sequence-iface
   list-sequence-iface
   )

  (cond-expand
    ((library (srfi 4))
      (begin
        ;; SRFI-4 does not define the mutable `@vector-copy!`
        ;; procedures, so a generic implementation based on
        ;; `@vector-length`, `@vector-set!`, and `@vector-ref` is
        ;; defined here, though will not necessarily be efficient.

        (define (generic-copy! vlength vset! vref)
          (define copy!
            (case-lambda
              ((to at from) (copy! to at from #f #f))
              ((to at from start) (copy! to at from start #f))
              ((to at from start end)
               (let ((start (or start 0))
                     (end (or end (vlength from)))
                     )
                 (let loop ((start start) (at at))
                   (cond
                    ((< start end)
                     (vset! to at (vref from start))
                     (loop (+ 1 start) (+ 1 at))
                     )
                    (else (values))
                    ))))))
          copy!
          )

        (define u8vector-copy!
          (generic-copy! u8vector-length u8vector-set! u8vector-ref)
          )

        (define s8vector-copy!
          (generic-copy! s8vector-length s8vector-set! s8vector-ref)
          )

        (define u16vector-copy!
          (generic-copy! u16vector-length u16vector-set! u16vector-ref)
          )

        (define s16vector-copy!
          (generic-copy! s16vector-length s16vector-set! s16vector-ref)
          )

        (define u32vector-copy!
          (generic-copy! u32vector-length u32vector-set! u32vector-ref)
          )

        (define s32vector-copy!
          (generic-copy! s32vector-length s32vector-set! s32vector-ref)
          )

        (define u64vector-copy!
          (generic-copy! u64vector-length u64vector-set! u64vector-ref)
          )

        (define s64vector-copy!
          (generic-copy! s64vector-length s64vector-set! s64vector-ref)
          )

        (define f32vector-copy!
          (generic-copy! f32vector-length f32vector-set! f32vector-ref)
          )

        (define f64vector-copy!
          (generic-copy! f64vector-length f64vector-set! f64vector-ref)
          )

        ))
    (else)
    )

  (begin

    ;;----------------------------------------------------------------
    ;; Helper procedures

    (define (identity o) o)

    (define (fast-list? o) (or (pair? o) (null? o)))

    (define (list-overwrite! to from)
      (cond
       ((null? from) (values))
       ((null? to)   (values))
       (else
        (set-car! to (car from))
        (list-overwrite! (cdr to) (cdr from))
        )))

    (define (list-overwrite-until! to from index)
      (let loop ((to to) (from from) (i 0))
        (cond
         ((<= index i) (values))
         ((null? to)   (values))
         ((null? from) (values))
         (else
          (set-car! to (car from))
          (loop (cdr to) (cdr from) (+ 1 i))
          ))))

    (define list-copy!
      (case-lambda
        ((to at from)
         (let ((to-cell (list-ref to at)))
           (list-overwrite! to-cell from)
           ))
        ((to at from start)
         (let ((to-cell   (list-ref to at))
               (from-cell (list-ref from start))
               )
           (list-overwrite! to-cell from-cell)
           ))
        ((to at from start end)
         (let*((to-cell   (list-ref to at))
               (from-cell (list-ref from start))
               )
           (list-overwrite-until! to-cell from-cell (- end start))
           ))))

    (define (bytevector->list vec)
      (let ((len (bytevector-length vec)))
        (let loop ((i 0))
          (cond
           ((< i len)
            (cons (bytevector-u8-ref vec i) (loop (+ 1 i)))
            )
           (else '())
           ))))

    (define (list->bytevector lst)
      (let*((len (length lst))
            (vec (make-bytevector len))
            )
        (let loop ((i 0) (lst lst))
          (cond
           ((null? lst) vec)
           (else
            (bytevector-u8-set! vec i (car lst))
            (loop (+ 1 i) (cdr lst))
            )))))

    (define (generic-for-each length vector-ref)
      (lambda (proc . vecs)
        (let ((len (apply min (map length vecs))))
          (let loop ((i 0))
            (cond
             ((< i len)
              (apply proc (map (lambda (vec) (vector-ref vec i)) vecs))
              (loop (+ 1 i))
              )
             (else (values))
             )))))

    (define bytevector-for-each
      (generic-for-each bytevector-length bytevector-u8-ref)
      )

    (define u8vector-for-each
      (generic-for-each u8vector-length u8vector-ref)
      )

    (define s8vector-for-each
      (generic-for-each s8vector-length s8vector-ref)
      )

    (define u16vector-for-each
      (generic-for-each u16vector-length u16vector-ref)
      )

    (define s16vector-for-each
      (generic-for-each s16vector-length s16vector-ref)
      )

    (define u32vector-for-each
      (generic-for-each u32vector-length u32vector-ref)
      )

    (define s32vector-for-each
      (generic-for-each s32vector-length s32vector-ref)
      )

    (define u64vector-for-each
      (generic-for-each u64vector-length u64vector-ref)
      )

    (define s64vector-for-each
      (generic-for-each s64vector-length s64vector-ref)
      )

    (define f32vector-for-each
      (generic-for-each f32vector-length f32vector-ref)
      )

    (define f64vector-for-each
      (generic-for-each f64vector-length f64vector-ref)
      )

    ;;----------------------------------------------------------------
    ;; The "sequence" interface

    (define-record-type <sequence-iface-type>
      (make<sequence-iface>
       seq?  is-vec  make  len  ref  set  copy!  ->list  list->  foreach
       )
      sequence-iface-type?
      (seq?    iface-sequence?)
      (is-vec  iface-is-vector?)
      (make    iface-make-sequence)
      (len     iface-sequence-length)
      (ref     iface-sequence-ref)
      (set     iface-sequence-set!)
      (copy!   iface-sequence-copy!)
      (->list  iface-sequence->list)
      (list->  iface-list->sequence)
      (foreach iface-sequence-for-each)
      )

    (define vector-sequence-iface
      (make<sequence-iface>
       vector?  #t  make-vector  vector-length
       vector-ref  vector-set!  vector-copy!
       vector->list  list->vector
       vector-for-each
       ))

    (define string-sequence-iface
      (make<sequence-iface>
       string?  #f  make-string  string-length
       string-ref  string-set!  string-copy!
       string->list  list->string
       string-for-each
       ))

    (define bytevector-sequence-iface
      (make<sequence-iface>
       bytevector?  #t  make-bytevector  bytevector-length
       bytevector-u8-ref  bytevector-u8-set!  bytevector-copy!
       bytevector->list  list->bytevector
       bytevector-for-each
       ))

    (define u8vector-sequence-iface
      (make<sequence-iface>
       u8vector?  #t  make-u8vector u8vector-length
       u8vector-ref u8vector-set!  u8vector-copy!
       u8vector->list list->u8vector
       u8vector-for-each
       ))

    (define s8vector-sequence-iface
      (make<sequence-iface>
       s8vector?  #t  make-s8vector s8vector-length
       s8vector-ref  s8vector-set!  s8vector-copy!
       s8vector->list  list->s8vector
       s8vector-for-each
       ))

    (define u16vector-sequence-iface
      (make<sequence-iface>
       u16vector?  #t  make-u16vector u16vector-length
       u16vector-ref  u16vector-set!  u16vector-copy!
       u16vector->list  list->u16vector
       u16vector-for-each
       ))

    (define s16vector-sequence-iface
      (make<sequence-iface>
       s16vector?  #t  make-s16vector s16vector-length
       s16vector-ref s16vector-set!  s16vector-copy!
       s16vector->list  list->s16vector
       s16vector-for-each
       ))

    (define u32vector-sequence-iface
      (make<sequence-iface>
       u32vector?  #t  make-u32vector u32vector-length
       u32vector-ref  u32vector-set!  u32vector-copy!
       u32vector->list  list->u32vector
       u32vector-for-each
       ))

    (define s32vector-sequence-iface
      (make<sequence-iface>
       s32vector?  #t  make-s32vector s32vector-length
       s32vector-ref  s32vector-set!  s32vector-copy!
       s32vector->list  list->s32vector
       s32vector-for-each
       ))

    (define u64vector-sequence-iface
      (make<sequence-iface>
       u64vector?  #t  make-u64vector u64vector-length
       u64vector-ref  u64vector-set!  u64vector-copy!
       u64vector->list  list->u64vector
       u64vector-for-each
       ))

    (define s64vector-sequence-iface
      (make<sequence-iface>
       s64vector?  #t  make-s64vector s64vector-length
       s64vector-ref  s64vector-set!  s64vector-copy!
       s64vector->list  list->s64vector
       s64vector-for-each
       ))

    (define f32vector-sequence-iface
      (make<sequence-iface>
       f32vector?  #t  make-f32vector f32vector-length
       f32vector-ref  f32vector-set!  f32vector-copy!
       f32vector->list  list->f32vector
       f32vector-for-each
       ))

    (define f64vector-sequence-iface
      (make<sequence-iface>
       f64vector?  #t  make-f64vector f64vector-length
       f64vector-ref  f64vector-set!  f64vector-copy!
       f64vector->list  list->f64vector
       f64vector-for-each
       ))

    (define list-sequence-iface
      (make<sequence-iface>
       fast-list?  #f  make-list  length
       list-ref  list-set!  list-copy!
       identity  identity  for-each
       ))

    (define (get-sequence-iface seq)
      (cond
       ((procedure? seq)
        (cond
         ((eq? string      seq) string-sequence-iface)
         ((eq? bytevector  seq) bytevector-sequence-iface)
         ((eq? vector      seq) vector-sequence-iface)
         ((eq? u8vector    seq) u8vector-sequence-iface)
         ((eq? u16vector   seq) u16vector-sequence-iface)
         ((eq? u32vector   seq) u32vector-sequence-iface)
         ((eq? u64vector   seq) u64vector-sequence-iface)
         ((eq? s8vector    seq) s8vector-sequence-iface)
         ((eq? s16vector   seq) s16vector-sequence-iface)
         ((eq? s32vector   seq) s32vector-sequence-iface)
         ((eq? s64vector   seq) s64vector-sequence-iface)
         ((eq? f32vector   seq) f32vector-sequence-iface)
         ((eq? f64vector   seq) f64vector-sequence-iface)
         ((eq? list        seq) list-sequence-iface)
         ((eq? string?     seq) string-sequence-iface)
         ((eq? bytevector? seq) bytevector-sequence-iface)
         ((eq? vector?     seq) vector-sequence-iface)
         ((eq? u8vector?   seq) u8vector-sequence-iface)
         ((eq? u16vector?  seq) u16vector-sequence-iface)
         ((eq? u32vector?  seq) u32vector-sequence-iface)
         ((eq? u64vector?  seq) u64vector-sequence-iface)
         ((eq? s8vector?   seq) s8vector-sequence-iface)
         ((eq? s16vector?  seq) s16vector-sequence-iface)
         ((eq? s32vector?  seq) s32vector-sequence-iface)
         ((eq? s64vector?  seq) s64vector-sequence-iface)
         ((eq? f32vector?  seq) f32vector-sequence-iface)
         ((eq? f64vector?  seq) f64vector-sequence-iface)
         ((eq? list?       seq) list-sequence-iface)
         (else (error "not a sequence constructor or predicate" seq))
         ))
       ((string?     seq) string-sequence-iface)
       ((bytevector? seq) bytevector-sequence-iface)
       ((vector?     seq) vector-sequence-iface)
       ((u8vector?   seq) u8vector-sequence-iface)
       ((u16vector?  seq) u16vector-sequence-iface)
       ((u32vector?  seq) u32vector-sequence-iface)
       ((u64vector?  seq) u64vector-sequence-iface)
       ((s8vector?   seq) s8vector-sequence-iface)
       ((s16vector?  seq) s16vector-sequence-iface)
       ((s32vector?  seq) s32vector-sequence-iface)
       ((s64vector?  seq) s64vector-sequence-iface)
       ((f32vector?  seq) f32vector-sequence-iface)
       ((f64vector?  seq) f64vector-sequence-iface)
       ((fast-list?  seq) list-sequence-iface)
       (else (error "not a sequence" seq))
       ))

    (define (seq-dispatch object operator . args)
      (apply (operator (get-sequence-iface object)) args)
      )

    ;;----------------------------------------------------------------
    ;; Generic `seq-*` functions dispatched by type.

    (define (seq-length o) (seq-dispatch o iface-sequence-length o))

    (define make-seq
      (case-lambda
        ((typ size) (seq-dispatch typ iface-make-sequence size))
        ((typ size init) (seq-dispatch typ iface-make-sequence size init))
        ))

    (define (seq-ref o i) (seq-dispatch o iface-sequence-ref o i))

    (define (seq-set! o i val) (seq-dispatch o iface-sequence-set! o i val))

    (define (seq-copy! o . args)
      (apply seq-dispatch o iface-sequence-copy! o args)
      )

    (define (seq->list o) (seq-dispatch o iface-sequence->list o))

    (define (list->seq o) (seq-dispatch o iface-list->sequence o))

    (define seq-for-each
      (generic-for-each seq-length seq-ref)
      )

    (define (typeof-vector? o) (seq-dispatch o iface-is-vector? o))

    (define (%seq-step proc)
      (lambda (subproc iface from to . seqs)
        (cond
         ((eq? iface list-sequence-iface)
          (error "cannot step forward on list types")
          )
         (else
          (let-values
              (((from to)
                (if (< from to) (values from to) (values to from))
                ))
            (apply proc subproc (iface-sequence-ref iface) from to seqs)
            )))))

    (define seq-step-forward/index
      ;; Similar to `vector-for-each`, except iterates only on
      ;; indicies starting at `FROM` and incrementing the index on
      ;; each iteration step stopping at index `TO`.  Raises an error
      ;; if `IFACE` is `list-sequence-iface`.
      ;;--------------------------------------------------------------
      (%seq-step
       (lambda (proc ref lo hi seqs)
         (let loop ((i lo))
           (cond
            ((< i hi)
             (apply proc i (map (lambda (vec) (ref vec i)) seqs))
             (loop (+ 1 i))
             )
            (else (values))
            )))))

    (define seq-step-reverse/index
      ;; Similar to `vector-for-each`, except iterates only on
      ;; indicies starting at `FROM` and decrementing the index on
      ;; each iteration step stopping at index `TO`.  Raises an error
      ;; if `IFACE` is `list-sequence-iface`.
      ;;--------------------------------------------------------------
      (%seq-step
       (lambda (proc ref lo hi seqs)
         (let loop ((i0 hi))
           (let ((i (- i0 1)))
             (cond
              ((< lo i0)
               (apply proc i (map (lambda (vec) (ref vec i)) seqs))
               (loop i)
               )
              (else (values))
              ))))))

    (define (%apply-without-index op)
      (lambda (proc . args)
        (apply op (lambda (_i . args) (apply proc args)) args)
        ))

    (define seq-step-forward (%apply-without-index seq-step-forward/index))
    (define seq-step-reverse (%apply-without-index seq-step-reverse/index))

    ))

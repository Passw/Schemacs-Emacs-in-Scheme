(define-library (schemacs gap-buffer)
  (import
    (scheme base)
    (scheme case-lambda)
    (only (schemacs sequence)
          get-sequence-iface
          iface-make-sequence
          iface-sequence-length
          iface-sequence-ref
          iface-sequence-set!
          iface-sequence-copy!
          typeof-vector?
          )
    )
  (export
   gap-buffer-type?  new-gap-buffer

   gap-buffer-grow!  *gap-buffer-grow-size-function*
   gap-buffer-end-of-line?      gap-buffer-start-of-line?
   gap-buffer-for-each          gap-buffer-for-each/index
   gap-buffer-for-each-before   gap-buffer-for-each-before/index
   gap-buffer-for-each-after    gap-buffer-for-each-after/index
   gap-buffer-map!              gap-buffer-map/index!
   gap-buffer-map               gap-buffer-map/index
   
   gap-buffer-update-min-max!
   gap-buffer-cursor   gap-buffer-weight   gap-buffer-free-space

   gap-buffer-ref
   gap-buffer-ref-before        gap-buffer-ref-after
   gap-buffer-cursor-to-start!  gap-buffer-cursor-to-end!
   gap-buffer-insert-before     gap-buffer-insert-after

   gap-buffer-minimum    set!gap-buffer-minimum
   gap-buffer-maximum    set!gap-buffer-maximum
   )

  (begin

    (define-record-type <gap-buffer-type>
      (make<gap-buffer> vec weight cursor min max)
      gap-buffer-type?
      (vec     gap-buffer-vector  set!gap-buffer-vector)
      ;; ^ The backing vector. This may be an ordinary vector
      ;; (satisfying `vector?`), or a `bytevector?`, or it may be a
      ;; homogeneous (unboxed) vector such as data which satisfy
      ;; the `u8vector?` or `u32vector?` predicates.
      (weight  gap-buffer-weight   set!gap-buffer-weight)
      ;; ^ The number of characters that have been inserted so
      ;; far. This may be any number from zero up to one minus the
      ;; length of the `gap-buffer-vector`.
      (cursor  gap-buffer-cursor   set!gap-buffer-cursor)
      ;; ^ The position where the gap begins, or you could think of it
      ;; as the position where the next element will be inserted by
      ;; `gap-buffer-insert!`.
      (min     gap-buffer-minimum  set!gap-buffer-minimum)
      (max     gap-buffer-maximum  set!gap-buffer-maximum)
      ;; ^ Tracks the lowest and highest valued element inserted,
      ;; useful only if the elements being buffered have some kind of
      ;; ordering.
      )

    (define (gap-buffer-update iface gb proc)
      ;; Most gap buffer updating functions need to have the gap
      ;; buffer deconstructed a bit, this function does that
      ;; deconstruction, mostly to save you from typing too much.
      ;;
      ;; The `proc` is applied 4 values:
      ;;
      ;;  1. the gap buffer vector
      ;;  2. the length of the gap buffer
      ;;  3. the weight, i.e. number of items in the buffer
      ;;  4. the cursor
      ;;--------------------------------------------------------------
      (let*((vec (gap-buffer-vector gb))
            (len ((iface-sequence-length iface) vec))
            )
        (proc
         vec len
         (gap-buffer-weight gb)
         (gap-buffer-cursor gb)
         )))

    (define (new-gap-buffer make-vector store-size . fill-val)
      ;; Construct a new gap buffer. The `MAKE-VECTOR` argument must
      ;; be a procedure which constructs a the backing vector such as
      ;; `make-vector` or `make-bytevector` or `make-u32vector`. The
      ;; backing vector constructor must take the `STORE-SIZE`
      ;; argument, and must optionally take the `fill-val` (value used
      ;; to initialize vector cells), these two arguments
      ;; (`STORE-SIZE` and `FILL-VAL` if provided) are applied to
      ;; procedure passed as the `MAKE-VECTOR` argument.
      ;;---------------------------------------------------------------
      (make<gap-buffer>
       (apply make-vector store-size fill-val)
       0 0 #f #f
       ))

    (define (gap-buffer-length iface gb)
      ((iface-sequence-length iface) (gap-buffer-vector gb))
      )

    (define (gap-buffer-end-of-line? gb)
      (= (gap-buffer-weight gb) (gap-buffer-cursor gb))
      )

    (define (gap-buffer-star-of-line? gb) (= 0 (gap-buffer-cursor gb)))

    (define *gap-buffer-grow-size-function*
      ;; A parameter which defines the function that should be used to
      ;; compute a new size for a gap buffer when it needs to be grown
      ;; to fit more elements than it has room to hold. The default is
      ;; to simply double the size of the current allocation.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (len weight +size)
         (let ((request (+ weight +size)))
           (let loop ((len len))
             (if (< len request) (loop (* 2 len)) len)
             )))))

    (define (gap-buffer-grow! iface gb +size)
      (gap-buffer-update
       iface gb
       (lambda (old-vec old-len weight cursor)
         (let*((new-len
                ((*gap-buffer-grow-size-function*)
                 old-len weight +size
                 )))
           (when (< old-len new-len)
             (let ((new-vec ((iface-make-sequence iface) new-len))
                   (above (- weight cursor))
                   (copy! (iface-sequence-copy! iface))
                   )
               (when (< 0 cursor)
                 (copy! new-vec 0 old-vec 0 cursor)
                 )
               (when (< 0 above)
                 (copy!
                  new-vec (- new-len above)
                  old-vec (- old-len above) old-len
                  ))
               (set!gap-buffer-vector gb new-vec)
               ))
           gb
           ))))

    (define (gap-buffer-free-space iface gb)
      (- ((iface-sequence-length iface) (gap-buffer-vector gb))
         (gap-buffer-weight gb)
         ))

    (define (gap-buffer-full? iface gb) 
      (= 0 (gap-buffer-free-space iface gb)))

    (define (%gap-buffer-for-each proc iface gb)
      (gap-buffer-update
       iface gb
       (lambda (vec len weight cursor)
         (cond
          ((not vec) (values))
          (else
           (let*((after  (- weight cursor))
                 (offset (- len weight))
                 (ref    (iface-sequence-ref iface))
                 )
             (proc ref vec len cursor after offset)
             ))))))

    (define (%gap-buffer-for-each-before proc ref vec cursor)
      (let loop ((i 0))
        (cond
         ((< i cursor) (proc i (ref vec i)) (loop (+ 1 i)))
         (else (values))
         )))

    (define (%gap-buffer-for-each-after proc ref vec len after offset)
      (let loop ((i (- len 1 after)))
        (cond
         ((< i len) (proc (- i offset) (ref vec i)) (loop (+ 1 i)))
         (else (values))
         )))

    (define (%gap-buffer-without-index op)
      (lambda (proc iface gb)
        (op (lambda (_i . args) (apply proc args)) iface gb)
        ))

    (define (gap-buffer-for-each/index proc iface gb)
      ;; Apply an index and element value for each valid index in the
      ;; gap buffer (indicies in the gap are skipped). Similar to the
      ;; `gap-buffer-for-each` except the `PROC` applied to each
      ;; element is also applied an index. The return values of this
      ;; `PROC` are ignored.
      ;;
      ;; Takes two or three arguments:
      ;;
      ;;  1. `PROC` is a procedure which takes an index and a
      ;;     value. It is evaluated once for each value and it's
      ;;     associated index in the gap buffer.
      ;;
      ;;  2. `GB` is the gap buffer object.
      ;;
      ;;  3. Optional vector interface for the type of vector used to
      ;;     store the gap buffer object's elements. If not provided,
      ;;     it is inferred from the gap buffer's `gap-buffer-vector`
      ;;     field.
      (%gap-buffer-for-each
       (lambda (ref vec len cursor after offset)
         (%gap-buffer-for-each-before proc ref vec cursor)
         (%gap-buffer-for-each-after proc ref vec len after offset)
         )
       iface gb
       ))

    (define (gap-buffer-for-each proc iface gb)
      ;; Apply a element value for each valid index in the gap buffer
      ;; (elements in the gap are skipped).
      ;;
      ;; Takes three arguments:
      ;;
      ;;  1. `PROC` is a procedure which takes an element. It is
      ;;     evaluated once for each valid element in the gap buffer
      ;;     (indicies in the gap are skipped). The return values of
      ;;     this `PROC` are ignored.
      ;;
      ;;  2. Sequence interface for the type of vector used to store
      ;;     the gap buffer object's elements. If not provided, it is
      ;;     inferred from the gap buffer's `gap-buffer-vector` field.
      ;;
      ;;  3. `GB` is the gap buffer object.
      (%gap-buffer-without-index gap-buffer-for-each/index)
      )

    (define (gap-buffer-for-each-before/index proc iface gb)
      ;; Like `gap-buffer-for-each/index`, but only operates on
      ;; elements before the cursor.
      (gap-buffer-update
       (lambda (vec _len _weight cursor)
         (%gap-buffer-for-each-before
          proc (iface-sequence-ref iface) vec cursor
          ))
       iface gb
       ))

    (define (gap-buffer-for-each-before proc iface gb)
      ;; Like `gap-buffer-for-each-before/index` but the `PROC`
      ;; argument only takes a single argument: the buffer elements,
      ;; `PROC` does not take an index as an argument.
      (%gap-buffer-without-index gap-buffer-for-each-before/index)
      )

    (define (gap-buffer-for-each-after/index proc iface gb)
      ;; Like `gap-buffer-for-each/index`, but only operates on
      ;; elements after the cursor.
      (%gap-buffer-for-each
       (lambda (ref vec len _cursor after offset)
         (%gap-buffer-for-each-after proc ref vec len after offset)
         )
       iface gb
       ))

    (define gap-buffer-for-each-after
      ;; Like `gap-buffer-for-each-after/index` but the `PROC` only
      ;; takes a single argument: the buffer elements. `PROC` does not
      ;; take an index as an argument.
      (%gap-buffer-without-index gap-buffer-for-each-after/index)
      )

    (define (%gap-buffer-map/index! proc iface gb to-vec)
      (let ((setter (iface-sequence-set! iface)))
        (gap-buffer-for-each/index
         (lambda (i elem) (setter to-vec i (proc i elem)))
         gb iface
         )))

    (define (gap-buffer-map/index! proc iface gb)
      ;; Similar to `gap-buffer-for-each/index`, except that the
      ;; return value of the `PROC` procedure is used to update each
      ;; element in the gap buffer in place.
      (%gap-buffer-map/index! proc iface gb (gap-buffer-vector gb))
      )

    (define (gap-buffer-map! proc iface gb)
      ;; Like `gap-buffer-map/index!` but the `PROC` only takes a
      ;; single argument: the buffer elements. `PROC` does not take an
      ;; index as an argument.
      (%gap-buffer-without-index gap-buffer-map/index!)
      )

    (define (gap-buffer-map/index proc iface gb)
      ;; Like `gap-buffer-map/index!` except instead of updating each
      ;; element in place, a new gap buffer object is created and
      ;; updated with the elements returned by `PROC`. The `PROC`
      ;; procedure takes an index of the current element, and the
      ;; current element.
      (let*((vec (gap-buffer-vector gb))
            (new-vec
             ((iface-make-sequence iface)
              ((iface-sequence-length iface) vec)
              ))
            (new-gb
             (make<gap-buffer>
              new-vec
              (gap-buffer-weight gb)
              (gap-buffer-cursor gb)
              (gap-buffer-minimum gb)
              (gap-buffer-maximum gb)
              )))
        (%gap-buffer-map/index! proc iface gb new-vec)
        new-gb
        ))

    (define (gap-buffer-update-min-max! gb iface)
      (gap-buffer-update
       iface gb
       (lambda (_vec len weight cursor)
         (let*((lo  (gap-buffer-minimum gb))
               (hi  (gap-buffer-maximum gb))
               (ref (iface-sequence-ref iface))
               )
           (cond
            ((and (< 0 weight) (not (and lo hi)))
             ;; Update only if there are elements, and if the lo or hi
             ;; value are invalid. The init value is the first or last
             ;; element depending on cursor position.
             (let*((i (if (< 0 cursor) 0 (- len 1)))
                   (init-val (ref i))
                   )
               (set! lo init-val)
               (set! hi init-val)
               (gap-buffer-for-each
                (lambda (_i elem)
                  (cond
                   ((< elem lo) (set! lo elem))
                   ((< hi elem) (set! hi elem))
                   (else (values))
                   ))
                iface gb
                )
               (set!gap-buffer-minimum gb lo)
               (set!gap-buffer-maximum gb hi)
               gb
               ))
            (else gb)
            )))))

    (define (gap-buffer-ref-before iface gb)
      (gap-buffer-update
       iface gb
       (lambda (vec len weight cursor)
         (cond
          ((< 0 weight)
           ((iface-sequence-ref iface) vec cursor)
           )
          (else
           (error "cannot reference empty gap buffer" gb)
           )))))


    (define (gap-buffer-ref-after iface gb)
      (gap-buffer-update
       iface gb
       (lambda (vec len weight cursor)
         (cond
          ((< 0 weight)
           ((iface-sequence-ref iface) vec (- len 1 (- weight cursor)))
           )
          (else
           (error "cannot reference empty gap buffer" gb)
           )))))

    (define (gap-buffer-ref iface gb i)
      (cond
       ((< i (gap-buffer-cursor gb))
        (gap-buffer-ref-before iface gb)
        )
       (else
        (gap-buffer-ref-after iface gb)
        )))

    (define (%gapbuf-get-index-before cur _wt _len) cur)
    (define (%gapbuf-get-index-after  cur  wt  len) (- len 1 (- wt cur)))

    (define (%gap-buffer-insert get-index)
      (lambda (iface gb elem)
        (let ((weight (gap-buffer-weight gb)))
          (gap-buffer-grow! iface gb (+ 1 weight))
          (gap-buffer-update
           iface gb
           (lambda (vec len weight cursor)
             ((iface-sequence-set! iface) vec (get-index cursor weight len) elem)
             (set!gap-buffer-weight gb (+ 1 weight))
             elem
             )))))

    (define gap-buffer-insert-after (%gap-buffer-insert %gapbuf-get-index-after))

    (define (gap-buffer-insert-before iface gb elem)
      ((%gap-buffer-insert %gapbuf-get-index-before) iface gb elem)
      (set!gap-buffer-cursor gb (+ 1 (gap-buffer-cursor gb)))
      )

    (define (gap-buffer-cursor-to-end! iface gb)
      (gap-buffer-update
       iface gb
       (lambda (vec len weight cursor)
         (let ((after (- weight cursor)))
           ((iface-sequence-copy! iface)
            vec (- len after) vec cursor after
            )
           weight
           ))))

    (define (gap-buffer-cursor-to-start! iface gb)
      (gap-buffer-update
       iface gb
       (lambda (vec len weight cursor)
         ((iface-sequence-copy! iface)
          vec (- len weight) vec 0 cursor
          ))))

    ))

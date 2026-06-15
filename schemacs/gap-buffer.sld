(define-library (schemacs gap-buffer)
  (import
    (scheme base)
    (scheme case-lambda)
    (only (scheme write) display write);;DEBUG
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
   gap-buffer-type?             new-gap-buffer
   gap-buffer-insert-before     gap-buffer-insert-after
   gap-buffer-move-cursor       gap-buffer-set-cursor
   gap-buffer-delete            gap-buffer-clear

   gap-buffer-end-of-line?      gap-buffer-start-of-line?
   gap-buffer-for-each          gap-buffer-for-each/index
   gap-buffer-for-each-before   gap-buffer-for-each-before/index
   gap-buffer-for-each-after    gap-buffer-for-each-after/index
   gap-buffer-map!              gap-buffer-map/index!
   gap-buffer-map               gap-buffer-map/index

   gap-buffer-length            gap-buffer-weight
   gap-buffer-cursor            gap-buffer-free-space
   gap-buffer-update-min-max    gap-buffer-insert-min-max

   gap-buffer-ref
   gap-buffer-ref-before        gap-buffer-ref-after
   gap-buffer-cursor-to-start   gap-buffer-cursor-to-end

   *gap-buffer-grow-size-function*
   gap-buffer-grow              gap-buffer-allocate
   gap-buffer-minimum           set!gap-buffer-minimum
   gap-buffer-maximum           set!gap-buffer-maximum
   )

  (begin

    (define-record-type <gap-buffer-type>
      (make<gap-buffer> iface vec weight cursor min max)
      gap-buffer-type?
      (iface   gap-buffer-seq-iface)
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

    (define (gap-buffer-update gb proc)
      ;; Most gap buffer updating functions need to have the gap
      ;; buffer deconstructed a bit, this function does that
      ;; deconstruction, mostly to save you from typing too much.
      ;;
      ;; The `proc` is applied 5 values:
      ;;
      ;;  1. the vector interface
      ;;  2. the buffer vector
      ;;  3. the length of the buffer
      ;;  4. the weight, i.e. number of items in the buffer
      ;;  5. the cursor
      ;;--------------------------------------------------------------
      (let*((vec (gap-buffer-vector gb))
            (iface (gap-buffer-seq-iface gb))
            (len ((iface-sequence-length iface) vec))
            )
        (proc
         iface vec len
         (gap-buffer-weight gb)
         (gap-buffer-cursor gb)
         )))

    (define (new-gap-buffer iface store-size . fill-val)
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
       iface
       (apply (iface-make-sequence iface) store-size fill-val)
       0 0 #f #f
       ))

    (define (gap-buffer-length gb)
      ((iface-sequence-length (gap-buffer-seq-iface gb)) (gap-buffer-vector gb))
      )

    (define (gap-buffer-end-of-line? gb)
      (= (gap-buffer-weight gb) (gap-buffer-cursor gb))
      )

    (define (gap-buffer-start-of-line? gb) (= 0 (gap-buffer-cursor gb)))

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

    (define (gap-buffer-grow gb +size)
      (gap-buffer-update
       gb
       (lambda (iface old-vec old-len weight cursor)
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

    (define (gap-buffer-allocate gb new-size)
      ;; Similar to `gap-buffer-grow` but ensures the gap buffer is at
      ;; least `NEW-SIZE` elements large and grows the gap buffer
      ;; allocation if it is not big enough. If the requested
      ;; `NEW-SIZE` is smaller than the current allocation, no change
      ;; is made.
      ;;--------------------------------------------------------------
      (let*((vec (gap-buffer-vector gb))
            (old-size (vector-length vec))
            )
        (when (< old-size new-size)
          (gap-buffer-grow gb (- new-size old-size))
          )))

    (define (gap-buffer-free-space gb)
      (- ((iface-sequence-length (gap-buffer-seq-iface gb)) (gap-buffer-vector gb))
         (gap-buffer-weight gb)
         ))

    (define (gap-buffer-full? gb) 
      (= 0 (gap-buffer-free-space gb))
      )

    (define (%gap-buffer-for-each proc gb)
      (gap-buffer-update
       gb
       (lambda (iface vec len weight cursor)
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
      (let loop ((i (- len after)))
        (cond
         ((< i len) (proc (- i offset) (ref vec i)) (loop (+ 1 i)))
         (else (values))
         )))

    (define (%gap-buffer-without-index op)
      (lambda (proc gb)
        (op (lambda (_i . args) (apply proc args)) gb)
        ))

    (define (gap-buffer-for-each/index proc gb)
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
       gb
       ))

    (define gap-buffer-for-each
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

    (define (gap-buffer-for-each-before/index proc gb)
      ;; Like `gap-buffer-for-each/index`, but only operates on
      ;; elements before the cursor.
      (gap-buffer-update
       gb
       (lambda (iface vec _len _weight cursor)
         (%gap-buffer-for-each-before
          proc (iface-sequence-ref iface) vec cursor
          ))
       ))

    (define gap-buffer-for-each-before
      ;; Like `gap-buffer-for-each-before/index` but the `PROC`
      ;; argument only takes a single argument: the buffer elements,
      ;; `PROC` does not take an index as an argument.
      (%gap-buffer-without-index gap-buffer-for-each-before/index) 
      )

    (define (gap-buffer-for-each-after/index proc gb)
      ;; Like `gap-buffer-for-each/index`, but only operates on
      ;; elements after the cursor.
      (%gap-buffer-for-each
       (lambda (ref vec len _cursor after offset)
         (%gap-buffer-for-each-after proc ref vec len after offset)
         )
       gb
       ))

    (define gap-buffer-for-each-after
      ;; Like `gap-buffer-for-each-after/index` but the `PROC` only
      ;; takes a single argument: the buffer elements. `PROC` does not
      ;; take an index as an argument.
      (%gap-buffer-without-index gap-buffer-for-each-after/index)
      )

    (define (%gap-buffer-map/index! proc gb to-vec)
      (let ((setter (iface-sequence-set! (gap-buffer-seq-iface gb))))
        (gap-buffer-for-each/index
         (lambda (i elem) (setter to-vec i (proc i elem)))
         gb
         )))

    (define (gap-buffer-map/index! proc gb)
      ;; Similar to `gap-buffer-for-each/index`, except that the
      ;; return value of the `PROC` procedure is used to update each
      ;; element in the gap buffer in place.
      (%gap-buffer-map/index! proc gb (gap-buffer-vector gb))
      )

    (define gap-buffer-map! 
      ;; Like `gap-buffer-map/index!` but the `PROC` only takes a
      ;; single argument: the buffer elements. `PROC` does not take an
      ;; index as an argument.
      (%gap-buffer-without-index gap-buffer-map/index!)
      )

    (define (gap-buffer-map/index proc gb)
      ;; Like `gap-buffer-map/index!` except instead of updating each
      ;; element in place, a new gap buffer object is created and
      ;; updated with the elements returned by `PROC`. The `PROC`
      ;; procedure takes an index of the current element, and the
      ;; current element.
      (let*((iface (gap-buffer-seq-iface gb))
            (vec (gap-buffer-vector gb))
            (new-vec
             ((iface-make-sequence iface)
              ((iface-sequence-length iface) vec)
              ))
            (new-gb
             (make<gap-buffer>
              iface  new-vec
              (gap-buffer-weight gb)
              (gap-buffer-cursor gb)
              (gap-buffer-minimum gb)
              (gap-buffer-maximum gb)
              )))
        (%gap-buffer-map/index! proc gb new-vec)
        new-gb
        ))

    (define (gap-buffer-insert-min-max gb new-val)
      (let ((old-min (gap-buffer-minimum gb))
            (old-max (gap-buffer-maximum gb))
            )
        (set!gap-buffer-minimum
         gb (or (and old-min (min new-val old-min)) new-val)
         )
        (set!gap-buffer-maximum
         gb (or (and old-max (max new-val old-max)) new-val)
         )
        new-val
        ))

    (define (gap-buffer-update-min-max gb)
      (gap-buffer-update
       gb
       (lambda (iface _vec len weight cursor)
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
                (lambda (elem)
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

    (define (gap-buffer-ref-before gb nothing)
      ;; Get the item just before the cursor
      (gap-buffer-update
       gb
       (lambda (iface vec len weight cursor)
         (cond
          ((> cursor 0)
           ((iface-sequence-ref iface) vec (- cursor 1))
           )
          (else
           ;;(error "cannot reference empty gap buffer" gb)
           nothing
           )))))


    (define (gap-buffer-ref-after gb nothing)
      ;; Get the item just after the cursor
      (gap-buffer-update
       gb
       (lambda (iface vec len weight cursor)
         (cond
          ((< 0 weight)
           ((iface-sequence-ref iface) vec (- len 1 (- weight cursor)))
           )
          (else
           ;;(error "cannot reference empty gap buffer" gb)
           nothing
           )))))

    (define (gap-buffer-ref gb i)
      (gap-buffer-update
       gb
       (lambda (iface vec len weight cursor)
	 (cond
	  ((< i (gap-buffer-cursor gb))
           (vector-ref vec (%gapbuf-get-index-before cursor weight len))
	   )
	  (else (vector-ref vec (%gapbuf-get-index-after cursor weight len)))
	  ))))

    (define (%gapbuf-get-index-before cur _wt _len) cur)
    (define (%gapbuf-get-index-after  cur  wt  len) (- len 1 (- wt cur)))

    (define (%gap-buffer-insert get-index)
      (lambda (gb elem)
        (let ((iface (gap-buffer-seq-iface gb)))
          (gap-buffer-grow gb 1)
          (gap-buffer-update
           gb
           (lambda (iface vec len weight cursor)
             ((iface-sequence-set! iface) vec (get-index cursor weight len) elem)
             (set!gap-buffer-weight gb (+ 1 weight))
             elem
             )))))

    (define gap-buffer-insert-after (%gap-buffer-insert %gapbuf-get-index-after))

    (define (gap-buffer-insert-before gb elem)
      ((%gap-buffer-insert %gapbuf-get-index-before) gb elem)
      (set!gap-buffer-cursor gb (+ 1 (gap-buffer-cursor gb)))
      )

    (define (gap-buffer-cursor-to-start gb)
      (gap-buffer-move-cursor gb (- (gap-buffer-cursor gb)))
      )

    (define (gap-buffer-cursor-to-end gb)
      (gap-buffer-move-cursor
       gb (- (gap-buffer-weight gb) (gap-buffer-cursor gb))
       ))

    (define (gap-buffer-move-cursor gb n)
      ;; Negative `N` moves the cursor toward the beginning of the
      ;; buffer, moves characters toward the end of the
      ;; buffer. Positive `N` moves the cursor toward the end of the
      ;; buffer, moves characters toward the beginning of the buffer.
      ;; ------------------------------------------------------------

      ;; NOTE: I believe Guile's implementation of `vector-copy!` for
      ;; (SRFI-4) is incorrect. The lines marked ";;NOTE" below are
      ;; commented out, but when uncommenting them you get log output
      ;; like this:
      ;;
      ;;     move: len=4, weight=3, cursor=3, after=4, n=-3
      ;;     pre: #u16(30 10 20 20)
      ;;     vector-copy! at=1, start=0, end=3
      ;;     post: #u16(30 30 30 30)
      ;;
      ;; The "post:" vector should have been #u16(30 30 10 20)
      (gap-buffer-update
       gb
       (lambda (iface vec len weight cursor)
         (cond
          ((= n 0) 0)     ;; no movement
          ((= len weight) ;; no gap
           (set!gap-buffer-cursor gb (max 0 (min len (+ n cursor))))
           )
          (else
           (let ((limit (min weight (max 0 (+ cursor n))))
                 (after (+ (- len weight) cursor))
                 )
             ;;(display "move: len=") (write len) (display ", weight=") (write weight) ;;NOTE
             ;;(display ", cursor=") (write cursor) (display ", after=") (write after) ;;NOTE
             ;;(display ", n=") (write n) (newline) ;;DEBUG
             (cond
              ((< n 0)
               (let ((n (max n (- cursor))))
                 ;;(display "pre: ") (write vec) (newline) ;;DEBUG
                 ;;(display "vector-copy! at=") (write (+ after n)) (display ", start=");;NOTE
                 ;;(write (+ cursor n)) (display ", end=") (write cursor) (newline) ;;NOTE
                 (cond-expand
                   (guile
                    ;; Guile's implementation of `vector-copy!` for
                    ;; SRFI-4 probably has a bug.
                    (let loop ((lo (- cursor 1)) (hi (- after 1)))
                      (cond
                       ((<= limit lo)
                        ((iface-sequence-set! iface)
                         vec hi ((iface-sequence-ref iface) vec lo)
                         )
                        (loop (- lo 1) (- hi 1))
                        )
                       (else (values))
                       ))
                    ) ;; end guile cond-expand
                   (else
                    ((iface-sequence-copy! iface)
                     vec (+ after n)
                     vec (+ cursor n) cursor
                     )
                    ;; end cond-expand
                    ))
                 ;;(display "post: ") (write vec) (newline) ;;DEBUG
                 ))
              (else
               (let ((n (min n (- weight cursor))))
                 ;;(display "pre: ") (write vec) (newline) ;;DEBUG
                 ;;(display "vector-copy! at=") (write cursor) (display ", start=") (write after);;DEBUG
                 ;;(display ", end=") (write (+ after n)) (newline) ;;DEBUG
                 (cond-expand
                   (guile
                    ;; Guile's implementation of `vector-copy!` for
                    ;; SRFI-4 probably has a bug.
                    (let loop ((lo cursor) (hi after))
                      (cond
                       ((< lo limit)
                        ((iface-sequence-set! iface)
                         vec lo ((iface-sequence-ref iface) vec hi)
                         )
                        (loop (+ 1 lo) (+ 1 hi))
                        )
                       (else (values))
                       ))
                    ) ;; end guile cond-expand
                   (else
                    ((iface-sequence-copy! iface)
                     vec cursor
                     vec after (+ after n)
                     )
                    ;; end cond-expand
                    ))
                 ;;(display "post: ") (write vec) (newline) ;;DEBUG
                 )))
             (set!gap-buffer-cursor gb limit)
             limit
             ))))))

    (define (gap-buffer-set-cursor gb index)
      ;; Move the gap buffer cursror to a given `INDEX`. The index
      ;; must be greater than or equal to 0 and less than the
      ;; `gap-buffer-weight` value. This function calls
      ;; `gap-buffer-move-cursor` after computing the difference of
      ;; the current `gap-buffer-cursor` and the given `INDEX`
      ;; argument.
      ;;--------------------------------------------------------------
      (gap-buffer-move-cursor gb (- index (gap-buffer-cursor gb)))
      )

    (define gap-buffer-delete
      ;; Delete N characters after the cursor. If N is negative,
      ;; delete N characters before the cursor. As an optional third
      ;; argument, you can pass a deletion function which maps over
      ;; all the elements that are about to be deleted. The deletion
      ;; function should take an element as input, and whatever is
      ;; returned is stored back to the vector before the cursor is
      ;; moved and makes those elements inaccessable. It allows you by
      ;; replacing values with `#f` for example, you can mark them for
      ;; removal by the garbage collector more immediately than they
      ;; would be if the cursor was simply moved and allowed those
      ;; inaccessible elements to linger in the storage vector. Note
      ;; there is no guarantee on the ordering in which the
      ;; deletion function is applied to the elements, if ordering is
      ;; important, please perform your own mapping and then call this
      ;; function with no third argument.
      ;;--------------------------------------------------------------
      (case-lambda
        ((gb n) (gap-buffer-delete gb n #f))
        ((gb n del)
         (gap-buffer-update
          gb
          (lambda (iface vec len weight cursor)
            (let*((n (max (- cursor) (min n (- weight cursor))))
                  (on-range
                   (lambda (from to)
                     (let loop ((i from))
                       (cond
                        ((< i to)
                         ((iface-sequence-set! iface)
                          vec i (del ((iface-sequence-ref iface) vec i))
                          )
                         (loop (+ 1 i))
                         )
                        (else (values))
                        )))))
              (cond
               ((< n 0)
                (let ((new-cursor (+ cursor n)))
                  (set!gap-buffer-cursor gb new-cursor)
                  (set!gap-buffer-weight gb (+ weight n))
                  (when del (on-range new-cursor cursor))
                  n))
               ((> n 0)
                (let*((new-weight (- weight n))
                      (after (+ (- len weight) cursor))
                      )
                  (set!gap-buffer-weight gb new-weight)
                  (when del (on-range after (+ after n)))
                  n))
               (else 0) ;; nothing to do
               )))))))

    (define (gap-buffer-clear gb)
      ;; Reset the cursor and weight to zero, but otherwise do not
      ;; change the allocation of the gap buffer.
      ;;--------------------------------------------------------------
      (set!gap-buffer-weight  gb 0)
      (set!gap-buffer-cursor  gb 0)
      (set!gap-buffer-minimum gb #f)
      (set!gap-buffer-maximum gb #f)
      )

    ))

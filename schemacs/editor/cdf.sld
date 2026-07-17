(define-library (schemacs editor cdf)
  ;; A Cumulative Distribution Function (CDF) data type for which APIs
  ;; that can lazily construct and alter the CDF have been defined.
  ;; This data structure is specifically designed for use with the
  ;; Schemacs text editor engine, but may be of use in other places as
  ;; well, so it is provided as a library.
  ;;------------------------------------------------------------------

  (import
   (scheme base)
   (scheme case-lambda)
   (scheme write) ;;DEBUG
   (only (schemacs sequence)
         *sequence-allocate-function*
         u64vector-sequence-iface
         iface-make-sequence
         iface-sequence-length
         iface-sequence-ref
         iface-sequence-set!
         sequence-allocate
         )
   )

  (export
   new-cdf  cdf-vector-type?
   cdf-vector-iface  cdf-vector  cdf-cursor  cdf-maximum
   cdf-ref  cdf-fill  cdf-invalidate!  cdf-push  cdf-pop
   cdf-find
   )

  (begin

    (define-record-type <cdf-vector-type>
      ;; This is a cumultative distribution function (CDF) that is
      ;; designed to grow and shrink with a dynamic probability
      ;; distribution function (PDF) that can change over time. The
      ;; CDF is re-computed lazily, only recomputed when `CDF-FIND` is
      ;; called on a `CDF` for which it's associated PDF has changed.
      (make<cdf-vector> iface vec cur max)
      cdf-vector-type?
      (iface  cdf-vector-iface)
      (vec    cdf-vector   set!cdf-vector)
      (cur    cdf-cursor   set!cdf-cursor)
      (max    cdf-maximum  set!cdf-maximum)
      )

    (define new-cdf
      ;; Construct a cumulative distribution function (CDF) of type
      ;; `<cdf-vector-type>` of a given `SIZE` and (optionally) using
      ;; a given sequence interface `IFACE`. If `IFACE` is not provided
      ;; then the `u64vector-sequence-iface` is selected by default.
      (case-lambda
       ((size) (new-cdf u64vector-sequence-iface size))
       ((iface size)
        (make<cdf-vector> iface ((iface-make-sequence iface) size) 0 0)
        )))

    (define (cdf-ref cdf i)
      ((iface-sequence-ref (cdf-vector-iface cdf)) (cdf-vector cdf) i)
      )

    (define (cdf-fill cdf generate)
      ;; Internally, the CDF is modeled as vector and as a stack. The
      ;; stack is modeled by keeping a `cdf-cursor` value pointing at
      ;; the index that represents the top of the stack. You can push
      ;; or pop integers onto or off-of the stack, this stores
      ;; integers into the vector and moves the cursor.
      ;;
      ;; The `GENERATE` function must take two values, (1) `INDEX` is
      ;; the current index of the CDF vector, and (2) `ACCUM`, which
      ;; will be the value of the current top of the CDF stack when
      ;; `GENERATE` is applied. The `GENERATE` function must return
      ;; the integer value to be pushed to the CDF stack, or if the
      ;; generator is out of values, it must return `#f`.
      ;;
      ;; This function generates integers from the `GENERATE` function
      ;; given as an argument and pushes to the top of the CDF stack
      ;; the sum of each generated integer with the previous top of
      ;; the stack.  The last value pushed to the CDF stack is the
      ;; value returned by this function.
      ;;--------------------------------------------------------------
      (let*((iface  (cdf-vector-iface cdf))
            (vec    (cdf-vector cdf))
            (len    ((iface-sequence-length iface) vec))
            (cursor (cdf-cursor cdf))
            (accum
             (if (< 0 cursor)
                 ((iface-sequence-ref iface) vec (- cursor 1))
                 0)))
        (let loop ((cursor cursor) (accum accum) (vec vec) (len len))
          (let ((next (generate cursor accum)))
            (cond
             (next
              (let*-values
                  (((vec len)
                    (let*((new-vec (sequence-allocate iface vec (+ 1 cursor)))
			  (new-len ((iface-sequence-length iface) new-vec))
                          )
                      (cond
                       ((not (eq? vec new-vec))
                        (set!cdf-vector cdf new-vec)
                        (values new-vec new-len)
                        )
                       (else (values vec len))
                       )))
                   ((accum) (+ accum next))
                   )
                ((iface-sequence-set! iface) vec cursor accum)
                (loop (+ 1 cursor) accum vec len)
                ))
             (else
              (set!cdf-cursor cdf cursor)
              (set!cdf-maximum cdf accum)
              accum
              ))))))

    (define (cdf-invalidate! cdf cursor)
      ;; Set the new `cursor` for the CDF. If the cursor is less than
      ;; the current `cdf-cursor` value, this means every element
      ;; after the `cursor` is invalid and needs to be
      ;; recomputed. This function simply sets the `cdf-cursor` and
      ;; returns the value of the CDF at the new cursor position. If
      ;; `cursor` is greater than the current `cdf-cursor`, then the
      ;; `cdf-cursor` is not changed and `#f` is returned.
      ;;--------------------------------------------------------------
      (let ((iface (cdf-vector-iface cdf))
            (old-cursor (cdf-cursor cdf))
            )
        (cond
         ((< old-cursor cursor) #f)
         (else
          (set!cdf-cursor cdf cursor)
          (cond
           ((< 0 cursor)
            (let ((maximum
                   ((iface-sequence-ref iface)
                    (cdf-vector cdf)
                    (- cursor 1)
                    )))
              (set!cdf-maximum cdf maximum)
              maximum
              ))
           (else
            (set!cdf-maximum cdf 0)
            0))))))

    (define (cdf-push cdf . elems)
      (cdf-fill
       cdf
       (lambda (cursor accum)
         (cond
          ((null? elems) #f)
          (else
           (let ((next (car elems)))
             (set! elems (cdr elems))
             next
             ))))))

    (define cdf-pop
      (case-lambda
       ((cdf) (cdf-pop cdf 1))
       ((cdf n) (cdf-invalidate! cdf (max 0 (- (cdf-cursor cdf) n))))
       ))

    (define cdf-find
      ;; Binary search returning which "bucket" I in a PDF does the
      ;; integer argument `N` fall into given a CDF computed for the
      ;; PDF. Takes two or three arguments
      ;;
      ;;   - `CDF` is the <cdf-vector-type>
      ;;
      ;;   - `INIT` (optional) from which index should the search begin.
      ;;      Defaults to the middle index.
      ;;
      ;;   - `N` the number to search for, and return which bucket
      ;;      into which it would be placed.
      ;;
      ;; In simpler terms, a discrete probability distribution
      ;; function (PDF) can be thought of as a sequence of buckets of
      ;; varying sizes modeled by a vector of integers where the index
      ;; of the bucket in the vector describes it's "address". A PDF
      ;; has a discrete cumulative distribution function (CDF) which
      ;; is a vector of integers, but at each address in the CDF we
      ;; store the sum of all bucket sizes before that address in the
      ;; PDF. When a random integer `N` is "dropped" onto the field of
      ;; buckets (modeled by the PDF), we may want to know the address
      ;; into which bucket the random integer N will fall. This
      ;; function computes the address of the bucket using a binary
      ;; search.
      ;;
      ;; This procedure returns two values:
      ;;
      ;;  1. the index `I` of the bucket into which the value `N`
      ;;     falls. In the context of a text editor, if `N` is the
      ;;     index of a character (from the start of the whole buffer
      ;;     of characters) this function returns the line number on
      ;;     which that index is placed.
      ;;
      ;;  2. the sum `S` of the sizes of all buckets before that
      ;;     index. In the context of a text editor, this is the
      ;;     number of all characters in the buffer prior to the start
      ;;     of the line on which the index `N` is placed.
      ;;
      ;; If `N` is out-of-bounds, that is, does not fall into any
      ;; bucket (too far to the negative or positive ends of the
      ;; field) then `(values #f #f)` is the result.
      ;;
      ;; One property of the return values of the function is that the
      ;; following expression is always true so long as `N` is in bounds:
      ;; 
      ;; ```
      ;; (let-values (((i s) (cdf-find buckets n)))
      ;;   (and (<= s n) (< n (cdf-ref buckets i)))
      ;;   )
      ;; ```
      (case-lambda
        ((cdf n) (cdf-find cdf #f n))
        ((cdf init n)
	 (let*((cursor (cdf-cursor cdf)))
           (cond
	    ((< 0 cursor)
	     (let*((iface (cdf-vector-iface cdf))
		   (ref   (iface-sequence-ref iface))
		   (vec   (cdf-vector cdf))
		   (half  (floor-quotient cursor 2))
		   (init  (or (and init (max 0 (min init (- cursor 1)))) half))
		   )
	       (display "n = ") (write n);;DEBUG
	       (display ", init = ") (write init);;DEBUG
	       (display ", cursor = ") (write cursor);;DEBUG
	       (display ", half = ") (write half) (newline);;DEBUG
	       (let loop ((interval half) (i0 init))
		 ;; Here we have a cursor i which selects the current and next
                 ;; element in the CDF vector. We want to check if the given
                 ;; value `n` is somewhere in between.
		 (let*((i0 (min i0 (- cursor 2)))
		       (i1 (+ 1 i0))
		       (lo (if (<= 0 i0) (ref vec i0) #f))
		       (hi (if (<= cursor i1) #f (ref vec i1)))
		       )
		   (display "interval = ") (write interval);;DEBUG
		   (display ", i0 = ") (write i0);;DEBUG
		   (display ", i1 = ") (write i1);;DEBUG
		   (display ", lo = ") (write lo);;DEBUG
		   (display ", hi = ") (write hi) (newline);;DEBUG
		   (cond
		    ((and (or (not lo) (<= lo n)) (< n hi)) (values i0 lo))
		    ((< 0 interval)
		     (cond
		      ((< n lo)
		       (let ((interval (floor-quotient interval 2)))
			 (loop interval (- i0 interval))
			 ))
		      ((<= hi n)
		       (let ((interval (floor-quotient (- cursor i0) 2)))
			 (loop interval (+ i0 interval))
			 ))
		      (else (values i0 lo))
		      ))
		    (else (values #f #f))
		    )))))
	    (else (values #f #f))
	    )))))

    ))

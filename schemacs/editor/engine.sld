(define-library (schemacs editor engine)
  ;; This library defines a text editor engine for buffering and
  ;; editing files. This library is intended to be used as a fallback
  ;; for Scheme platforms that do not already provide a text editor,
  ;; or at least platforms for which their text editor API is not well
  ;; suited as an implementation for the `(schemacs editor-impl)` API.
  ;; Whenever possible, Schemacs implementations should make use of
  ;; the platform-specific file editor infrastructure instead of this
  ;; library.
  ;;------------------------------------------------------------------
  (import
    (scheme base)
    (scheme case-lambda)
    (only (scheme write)  write-char  write-string)
    (only (schemacs vector)
          u32vector?  make-u32vector  u32vector-ref  u32vector-set!
          )
    (only (schemacs vbal)
          vbal-type?  vbal->alist  alist->vbal
          )
    (prefix (schemacs ui text-buffer-impl) impl/)
    (only (schemacs lens) record-unit-lens view lens-set update)
    (only (schemacs sequence)
          u8vector-sequence-iface
          u16vector-sequence-iface
          u32vector-sequence-iface
          bytevector-sequence-iface
          get-sequence-iface
          iface-make-sequence
          iface-sequence-length
          iface-sequence-ref
          iface-sequence-set!
          iface-sequence-copy!
          iface-sequence-for-each
          typeof-vector?
          )
    )
  (export
   text-line-type?  new-text-line  write-text-line
   gap-buffer-type?  new-gap-buffer
   gap-buffer-for-each  gap-buffer-for-each/index
   gap-buffer-map/index!  gap-buffer-map!
   gap-buffer-map/index   gap-buffer-map
   gap-buffer-update-min-max!

   new-text-editor  text-editor-insert

   run-editor-engine
   ;; ^ This procedure is called in the same way as the Scheme
   ;; `apply` procedure, except that it parameterizes all of the
   ;; relevant parameter variables in the
   ;; `(schemacs ui text-buffer-impl)` library.
   )

  (begin

    (define-record-type <text-line-type>
      ;; This is the type for lines of text in the buffer. Once a line
      ;; it is done being editied, it is "frozen" to this type and
      ;; placed somewhere into the buffer where the line cursor is.
      ;;--------------------------------------------------------------
      (make<text-line> string props parser offset iface)
      text-line-type?
      (string   text-line-string   set!text-line-string)
      ;; ^ This defines the actual string content.
      (props    text-line-props    set!text-line-props)
      ;; ^ This stores aribtrary information about the text properties
      ;; of the string. It should be a Vector-Backed Association list
      ;; (VBAL) type.
      (parser   text-line-parser  set!text-line-parser)
      ;; ^ When parsing a large files it is sometimes faster to keep a
      ;; continuation with the current parser state that was captured
      ;; when the end of the input lines was reached by the parser.
      ;; This allows the continuation to resume from this text line
      ;; when a change is made to lines coming after it.
      (offset   text-line-char-offset  set!text-line-char-offset)
      ;; ^ A simple way to compress data using an unboxed vector is to
      ;; keep track of the lowest and highest value in the sequence and
      ;; offset them so they fit into a fewer number of bits.
      (iface    text-line-sequence-iface)
      ;; ^ a reference to the vector interface for the
      ;; `text-line-string` field of this value.
      )

    (define (new-text-line string)
      (make<text-line> string #f #f 0 (get-sequence-iface string))
      )

    (define (text-line-for-each proc line)
      (let ((str (text-line-string line))
            (iface (text-line-sequence-iface))
            (foreach (iface-sequence-for-each iface))
            (offset (text-line-char-offset line))
            )
        (cond
         ((string? str) (string-for-each proc str))
         ((and offset (= offset 0))
          (foreach (lambda (i) (proc (integer->char i))) str)
          )
         (else
          (foreach
           (lambda (i) (proc (integer->char (+ i offset))))
           str
           )))))

    (define write-text-line
      (case-lambda
        ((line) (write-text-line line (current-output-port)))
        ((line port)
         (cond
          ((string? str) (write-string str port))
          (else
           (text-line-for-each (lambda (i) (write-char (integer->char i))) line)
           )))))

    ;;----------------------------------------------------------------

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

    (define =>gap-buffer-minimum*!
      (record-unit-lens
       gap-buffer-minimum
       set!gap-buffer-minimum
       '=>gap-buffer-miniumum*!
       ))

    (define =>gap-buffer-maximum*!
      (record-unit-lens
       gap-buffer-maximum
       set!gap-buffer-maximum
       '=>gap-buffer-maxiumum*!
       ))

    (define =>gap-buffer-weight*!
      (record-unit-lens
       gap-buffer-weight
       set!gap-buffer-weight
       '=>gap-buffer-weight*!
       ))

    (define =>gap-buffer-cursor*!
      (record-unit-lens
       gap-buffer-cursor
       set!gap-buffer-cursor
       '=>gap-buffer-cursor*!
       ))

    (define (%gap-buffer-update iface gb proc)
      ;; Most gap buffer updating functions need to have the gap
      ;; buffer deconstructed a bit, this function does that
      ;; deconstruction, mostly to save me from typing too much.
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
             (if (<= len request) (loop (* 2 len)) len)
             )))))

    (define (gap-buffer-grow iface gb +size)
      (%gap-buffer-update
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

    (define gap-buffer-full? 
      (case-lambda
        ((gb) (gap-buffer-full? (get-sequence-iface gb) gb))
        ((iface gb)
         (= (gap-buffer-weight gb)
            ((iface-sequence-length iface)
             (gap-buffer-vector gb)
             )))))

    (define gap-buffer-for-each/index
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
      (case-lambda
        ((proc gb)
         (gap-buffer-for-each
          proc gb (get-sequence-iface (gap-buffer-vector gb))
          ))
        ((proc iface gb)
         (let ((vec (gap-buffer-vector gb)))
           (cond
            ((typeof-vector? vec)
             (let*((weight (gap-buffer-weight gb))
                   (cursor (gap-buffer-cursor gb))
                   (after  (- weight cursor))
                   (ref    (iface-sequence-ref iface))
                   (len    ((iface-sequence-length iface) vec))
                   )
               (let loop ((i 0))
                 (cond
                  ((< i cursor) (proc (ref vec i)) (loop (+ 1 i)))
                  (else (values))
                  ))
               (let loop (i (- len after))
                 (cond
                  ((< i len) (proc (ref vec i)) (loop (+ 1 i)))
                  (else weight)
                  ))))
            (else (error "not a type of vector" vec 'in-gap-buffer: gb))
            )))))

    (define gap-buffer-for-each
      ;; Apply a element value for each valid index in the gap buffer
      ;; (elements in the gap are skipped).
      ;;
      ;; Takes two or three arguments:
      ;;
      ;;  1. `PROC` is a procedure which takes an element. It is
      ;;     evaluated once for each valid element in the gap buffer
      ;;     (indicies in the gap are skipped). The return values of
      ;;     this `PROC` are ignored.
      ;;
      ;;  2. `GB` is the gap buffer object.
      ;;
      ;;  3. Optional vector interface for the type of vector used to
      ;;     store the gap buffer object's elements. If not provided,
      ;;     it is inferred from the gap buffer's `gap-buffer-vector`
      ;;     field.
      (case-lambda
        ((proc gb)
         (gap-buffer-for-each
          proc (get-sequence-iface (gap-buffer-vector gb)) gb
          ))
        ((proc iface gb)
         (gap-buffer-for-each/index
          (lambda (_i elem) (proc elem)) iface gb
          ))))

    (define (%gap-buffer-map/index! proc iface gb to-vec)
      (let ((setter (iface-sequence-set! iface)))
        (gap-buffer-for-each/index
         (lambda (i elem) (setter to-vec i (proc i elem)))
         gb iface
         )))

    (define gap-buffer-map/index!
      ;; Similar to `gap-buffer-for-each/index`, except that the
      ;; return value of the `PROC` procedure is used to update each
      ;; element in the gap buffer in place.
      (case-lambda
        ((proc gb)
         (gap-buffer-map/index!
          (get-sequence-iface (gap-buffer-vector gb)) gb
          ))
        ((proc iface gb)
         (%gap-buffer-map/index! proc iface gb (gap-buffer-vector gb))
         )))

    (define gap-buffer-map!
      (case-lambda
        ((proc gb)
         (gap-buffer-map! proc (get-sequence-iface (gap-buffer-vector gb)) gb)
         )
        ((proc iface gb)
         (gap-buffer-map/index!
          (lambda (_i elem) (proc elem)) iface gb
          ))))

    (define gap-buffer-map/index
      (case-lambda
        ((proc gb)
         (gap-buffer-map
          proc gb (get-sequence-iface (gap-buffer-vector gb))
          ))
        ((proc iface gb)
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
           ))))

    (define (gap-buffer-update-min-max! gb iface)
      (%gap-buffer-update
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
                gb)
               (set!gap-buffer-minimum gb lo)
               (set!gap-buffer-maximum gb hi)
               gb
               ))
            (else gb)
            )))))

    (define (%gap-buffer-insert iface gb elem move index)
      (%gap-buffer-update
       iface gb
       (lambda (vec len weight cursor)
         (when (gap-buffer-full? iface gb)
           (gap-buffer-grow iface gb (+ 1 weight))
           )
         ((iface-sequence-set! iface) vec (index len weight cursor) elem)
         (when move (set!gap-buffer-cursor gb (+ 1 cursor)))
         (set!gap-buffer-weight gb (+ 1 weight))
         elem
         )))

    (define gap-buffer-insert-before
      (case-lambda
        ((gb elem)
         (gap-buffer-insert-before
          (get-sequence-iface (gap-buffer-vector gb))
          gb elem
          ))
        ((iface gb elem)
         (%gap-buffer-insert
          iface gb elem #t
          (lambda (len weight cursor) cursor)
          ))))

    (define gap-buffer-insert-after
      (case-lambda
        ((gb elem)
         (gap-buffer-insert-after
          (get-sequence-iface (gap-buffer-vector gb))
          gb elem
          ))
        ((iface gb elem)
         (%gap-buffer-insert
          iface gb elem #f
          (lambda (len weight cursor)
            (let*((after (- weight cursor)))
              (- len 1 after)
              ))))))

    ;;----------------------------------------------------------------

    (define-record-type <text-editor-type>
      (make<text-editor> lines line-ed cdf lbrkf textprops)
      text-editor?
      (lines      text-editor-lines         set!text-editor-lines)
      ;; ^ A <gap-buffer-type> which buffers <text-line-type> values.
      (line-ed    text-editor-line-editor   set!text-editor-line-editor)
      ;; ^ A <gap-buffer-type> which buffers characters, edits the
      ;; current line under the cursor.
      (cdf        text-editor-cdf           set!text-editor-cdf)
      ;; ^ The "Cumulative Distribution Function" kept up to date for
      ;; faster random access to arbitrary characters in the buffer, a
      ;; <u32vector> which records the total number of characters that
      ;; exist before each <text-line-type> in the <gap-buffer-type>
      ;; of the `TEXT-EDITOR-LINES` field of this data structure. For
      ;; example, if you lookup index 5 in the CDF, this will be an
      ;; unsigned integer that counts how many characters exist before
      ;; and including line 5 in the line editor.
      (lbrk       text-editor-line-break    set!text-editor-line-break)
      ;; ^ A function which inserts line break characters into the editor.
      (textprops  text-editor-text-props    set!text-editor-text-props)
      ;; A VBAL that contains text properties for ranges of text that
      ;; span multiple <text-line-type> values. This is useful for
      ;; syntax coloring as you can declare all characters between any
      ;; two (line,colunm) coordinates to have a particular tag.
      )

    (define line-ed-iface u32vector-sequence-iface)

    (define =>text-editor-line-editor*!
      (record-unit-lens
       text-editor-line-editor
       set!text-editor-line-editor
       '=>text-editor-line-editor*!
       ))

    (define init-text-editor-line-count (make-parameter 4096))

    (define new-text-editor
      (case-lambda
        (() (new-text-editor "\n"))
        ((lbrk) (new-text-editor lbrk #f))
        ((lbrk props)
         (let*((size (init-text-editor-line-count))
               (line (new-gap-buffer make-u32vector size))
               )
           (set!gap-buffer-minimum line #xFFFFFFFF)
           (set!gap-buffer-maximum line 0)
           (make<text-editor>
            (new-gap-buffer make-vector size)
            line lbrk (make-u32vector size) props
            )))))

    (define (text-editor-cursor-to-end ed)
      (let ((gb (text-editor-line-editor ed)))
        (%gap-buffer-update
         line-ed-iface ed
         (lambda (vec len weight cursor)
           (let ((after (- weight cursor)))
             ((iface-sequence-copy! line-ed-iface)
              vec (- len after) vec cursor after
              )
             weight
             )))))

    (define (text-editor-cursor-to-start end)
      (let ((gb (text-editor-line-editor ed)))
        (%gap-buffer-update
         line-ed-iface ed
         (lambda (vec len weight cursor)
           ((iface-sequence-copy! line-ed-iface)
            vec (- len weight) vec 0 cursor
            )))))

    (define (%text-editor-insert-char-before ed +size set!indices)
      (let*((line-ed (gap-buffer-grow (text-editor-line-editor ed) +size))
            (cur     (gap-buffer-cursor line-ed))
            (weight  (gap-buffer-weight line-ed))
            (vec     (gap-buffer-vector line-ed))
            (vlen    (u32vector-length vec))
            )
        (set!indicies line-ed vec vlen cur weight)
        ))

    (define (%text-editor-insert-char =>index*! ed char)
      (let ((int (char->integer char))
            (lo (gap-buffer-minimum line-ed))
            (hi (gap-buffer-maximum line-ed))
            )
        (u32vector-set! (gap-buffer-vector ed) at-index int)
        (set!gap-buffer-minimum line-ed (min int lo))
        (set!gap-buffer-maximum line-ed (max int hi))
        ))

    (define (text-editor-freeze-line ed lbrk)
      ;; Freeze the current line editor into an unboxed vector of the
      ;; exact correct size. Returns a `<text-line-type>`.
      ;;--------------------------------------------------------------
      (let*((line-ed (text-editor-line-editor ed)))
        (gap-buffer-update-min-max! line-ed-iface line-ed)
        (%gap-buffer-update
         line-ed-iface line-ed
         (lambda (vec len weight cursor)
           (let*((lo    (gap-buffer-minimum line-ed))
                 (hi    (gap-buffer-maximum line-ed))
                 (range (and lo hi (abs (- hi lo))))
                 (iface
                  (cond
                   ((<= range 0xFF) bytevector-sequence-iface)
                   ((<= range 0xFFFF) u16vector-sequence-iface)
                   (else u32vector-sequence-iface)
                   ))
                 (vec
                  ((iface-make-sequence iface)
                   (+ weight (or (and lbrk (string-length lbrk)) 0))
                   ))
                 (setter (iface-sequence-set! iface))
                 )
             (let ((i 0))
               (gap-buffer-for-each
                (lambda (n)
                  (setter vec i (- n lo))
                  (set! i (+ 1 i))
                  )
                line-ed-iface line-ed
                )
               (cond
                ((string? lbrk)
                 (string-for-each
                  (lambda (c)
                    (setter vec i (- (char->integer c) lo))
                    (set! i (+ 1 i))
                    )
                  lbrk
                  ))
                ((char? lbrk)
                 (setter vec i (- (char->integer c) lo))
                 )
                ((not lbrk) (values))
                (else (error "unknown line break value" lbrk))
                ))
             (make<text-line> vec #f #f iface lo)
             )))))

    (define text-editor-insert
      (case-lambda
        ((ed chars) (text-editor-insert #f ed chars))
        ((before-or-after ed chars)
         (let*((d before-or-after)
               (after  (or (eq? d 'after)  (eq? d '>) (eq? d >)))
               (before (or (eq? d 'before) (eq? d '<) (eq? d <)))
               )
           ;; TODO: handle insertion of #\newline characters.
           (cond
            ((char? chars)
             (%text-editor-insert
              ed 1
              (cond
               ((and before (not after))
                (lambda (line-ed vec vlen cur weight)
                  (set!gap-buffer-weight line-ed (+ weight 1))
                  (set!gap-buffer-cursor line-ed (+ cur 1))
                  (%text-editor-insert-char line-ed cur chars)
                  ))
               ((and after (not before))
                (lambda (line-ed vec vlen cur weight)
                  (%text-editor-insert-char line-ed (- vlen weight 1) chars)
                  ))
               (else (error "expecting 'before or 'after" d))
               )))
            ((string? chars)
             (let ((strlen (string-length chars)))
               (%text-editor-insert
                ed strlen
                (cond
                 ((and before (not after))
                  (lambda (line-ed vec vlen cur weight)
                    (set!gap-buffer-weight line-ed (+ weight strlen))
                    (set!gap-buffer-cursor line-ed (+ cur strlen))
                    (string-for-each
                     (lambda (char)
                       (%text-editor-insert-char line-ed cur char)
                       (set! cur (+ 1 cur))
                       )
                     chars
                     )))
                 ((and after (not before))
                  (lambda (line-ed vec vlen cur weight)
                    (let ((cur (- vlen weight strlen)))
                      (string-for-each
                       (lambda (char)
                         (%text-editor-insert-char line-ed cur char)
                         (set! cur (+ 1 cur))
                         )
                       (%text-editor-insert-char line-ed (- vlen weight 1) chars)
                       ))))
                 (else (error "expecting 'before or 'after" d))
                 ))))
            (else (error "cannot insert value into text editor" chars))
            )
           chars
           ))))

    ;;----------------------------------------------------------------

    (define (run-editor-engine proc . args)
      (parameterize
          ((impl/new-buffer*      new-text-editor)
           (impl/buffer-type?     text-editor?)
           )
        (apply proc args)
        ))

    ))

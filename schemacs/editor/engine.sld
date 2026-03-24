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
    (only (schemacs vector)
          u32vector?  make-u32vector  u32vector-ref  u32vector-set!
          )
    (only (schemacs vbal)
          vbal-type?  vbal->alist  alist->vbal
          )
    (prefix (schemacs ui text-buffer-impl) impl/)
    )
  (export
   (text-line-type?  new-text-line
                     gap-buffer-type?  new-gap-buffer

                     run-editor-engine
                     ;; ^ This procedure is called in the same way as the Scheme
                     ;; `apply` procedure, except that it parameterizes all of the
                     ;; relevant parameter variables in the
                     ;; `(schemacs ui text-buffer-impl)` library.
                     ))
  (begin

    (define-record-type <text-line-type>
      (make<text-line> string props parser)
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
      )

    (define (new-text-line string) (make<text-line> string #f #f))

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

    (define gap-buffer-grow-size-function
      (make-parameter
       (lambda (len weight +size)
         (let ((request (+ weight +size)))
           (let loop ((len len))
             (if (<= len request) (loop (* 2 len)) len)
             )))))

    (define (gap-buffer-grow length make copy gb +size)
      (let*((weight (gap-buffer-weight gb))
            (vec (gap-buffer-vector gb))
            (cur (gap-buffer-cursor gb))
            (len0 (length vec))
            (len1 ((gap-buffer-grow-size-function) len0 weight +size))
            )
        (when (< len0 len1)
          (let ((new-vec (make len1))
                (above (- weight cur))
                )
            (set!gap-buffer-vector gb new-vec)
            (when (< 0 cur)
              (copy new-vec 0 old-vec 0 cursor)
              )
            (when (< 0 above)
              (copy new-vec (- len1 above 1) vec (- len0 above 1) above)
              )))
        gb
        ))

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

    (define (%text-editor-insert ed +size set!indices)
      (let*((line-ed (gap-buffer-grow (text-editor-line-editor ed) +size))
            (cur     (gap-buffer-cursor line-ed))
            (weight  (gap-buffer-weight line-ed))
            (vec     (gap-buffer-vector line-ed))
            (vlen    (u32vector-length vec))
            )
        (set!indicies line-ed vec vlen cur weight)
        ))

    (define (%text-editor-insert-char ed at-index char)
      (let ((int (char->integer char)))
        (u32vector-set! (gap-buffer-vector ed) at-index int)
        (set!gap-buffer-minimum line-ed (min int (gap-buffer-minimum line-ed)))
        (set!gap-buffer-maximum line-ed (max int (gap-buffer-maximum line-ed)))
        ))

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

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
    (only (schemacs sequence)
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
          seq-step-forward/index
          typeof-vector?
          )
    (only (schemacs gap-buffer)
           new-gap-buffer
           gap-buffer-end-of-line?  gap-buffer-start-of-line?
           gap-buffer-for-each  gap-buffer-for-each/index
           gap-buffer-for-each-after
           gap-buffer-for-each-after/index
           gap-buffer-map/index!  gap-buffer-map!
           gap-buffer-map/index   gap-buffer-map
           gap-buffer-update-min-max!
           gap-buffer-cursor-to-start!
           gap-buffer-cursor-to-end!
           gap-buffer-insert-before
           gap-buffer-insert-after
           gap-buffer-minimum    set!gap-buffer-minimum
           gap-buffer-maximum    set!gap-buffer-maximum
           gap-buffer-cursor
           gap-buffer-ref-after
           gap-buffer-weight
           )
    )
  (export
   text-line-type?  new-text-line  write-text-line

   line-break-newline  line-break-return
   line-break-null  line-break-crlf  line-break-lfcr

   new-text-editor  text-editor-insert
   *init-text-editor-line-count*

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
      (make<text-line> string props parser offset maxval lbrk iface)
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
      (maxval   text-line-char-max     set!text-line-char-max)
      (lbrk     text-line-break        set!text-line-break)
      ;; ^ The line breaking symbol (or char or string) used to
      ;; delimit this line from the next in a sequence of lines. This
      ;; is usually the procedure `line-break-crlf`,
      ;; `line-break-lfcr`, `line-break-null`, `line-break-newline`,
      ;; or `line-break-return`.
      (iface    text-line-sequence-iface)
      ;; ^ a reference to the vector interface for the
      ;; `text-line-string` field of this value.
      )

    (define (line-break-crlf port)
      (write-char #\newline port)
      (write-char #\return  port)
      )

    (define (line-break-lfcr port)
      (write-char #\return  port)
      (write-char #\newline port)
      )

    (define (line-break-newline port) (write-char #\newline port))
    (define (line-break-return  port) (write-char #\return  port))
    (define (line-break-null    port) (write-char #\null    port))

    (define (new-text-line string)
      (make<text-line> string #f #f #f #f #f (get-sequence-iface string))
      )

    (define (text-line-for-each proc line)
      (let ((str (text-line-string line)))
        (cond
         ((string? str) (string-for-each proc str))
         ((not     str) (values))
         (else
          (let*((iface   (text-line-sequence-iface line))
                (foreach (iface-sequence-for-each iface))
                (offset  (text-line-char-offset line))
                )
            (cond
             ((and offset (= offset 0))
              (foreach (lambda (i) (proc (integer->char i))) str)
              )
             (else
              (foreach
               (lambda (i) (proc (integer->char (+ i offset))))
               str
               ))))))))

    (define write-text-line
      (case-lambda
        ((line) (write-text-line line (current-output-port)))
        ((line port)
         (let ((lbrk (text-line-break line))
               (str  (text-line-string line))
               )
           (cond
            ((string? str) (write-string str port))
            (else
             (text-line-for-each
              (lambda (i) (write-char (integer->char i)))
              line
              )))
           (when lbrk (lbrk port))
           ))))

    ;;----------------------------------------------------------------

    (define-record-type <text-editor-type>
      (make<text-editor> lines line-ed cdf lbrk textprops)
      text-editor-type?
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

    (define init-text-editor-line-count (make-parameter 4096))

    (define new-text-editor
      (case-lambda
        (() (new-text-editor "\n"))
        ((lbrk) (new-text-editor lbrk #f))
        ((lbrk props)
         (let*((size (*init-text-editor-line-count*))
               (line (new-gap-buffer make-u32vector size))
               )
           (set!gap-buffer-minimum line #xFFFFFFFF)
           (set!gap-buffer-maximum line 0)
           (make<text-editor>
            (new-gap-buffer make-vector size)
            line lbrk (make-u32vector size) props
            )))))

    ;;----------------------------------------------------------------
    ;; Line editor procedures

    (define (%line-editor-insert-char op)
      ;; This is the logic that is common to both insert before and
      ;; insert after, and assuming the `CHAR` is not a line break. If
      ;; you do insert a `#\newline` character, it will be written
      ;; into the buffer without actually performing a line break.
      ;;--------------------------------------------------------------
      (lambda (line-ed char)
        (let*((int (char->integer char))
              (lo (gap-buffer-minimum line-ed))
              (hi (gap-buffer-maximum line-ed))
              )
          (op line-ed-iface line-ed int)
          (when (< int lo) (set!gap-buffer-minimum line-ed int))
          (when (< hi int) (set!gap-buffer-maximum line-ed int))
          )))

    (define (line-editor-cursor-to-end! line-ed)
      (gap-buffer-cursor-to-end! line-ed-iface line-ed)
      )

    (define (line-editor-cursor-to-start! line-ed)
      (gap-buffer-cursor-to-start! line-ed-iface line-ed)
      )

    (define line-editor-insert-char-before 
      (%line-editor-insert-char gap-buffer-insert-before)
      )

    (define line-editor-insert-char-after 
      (%line-editor-insert-char gap-buffer-insert-after)
      )

    (define (%line-editor-pre-freeze lo hi)
      (let*((range (abs (- hi lo))))
        (cond
         ((<= range #xFF) bytevector-sequence-iface)
         ((<= range #xFFFF) u16vector-sequence-iface)
         (else u32vector-sequence-iface)
         )))

    (define (line-editor-freeze line-ed lbrk)
      ;; Freeze all characters in the line buffer into a new
      ;; `<text-line-type>` object that contains the exact right size
      ;; to hold all of the characters.
      (gap-buffer-update-min-max! line-ed-iface line-ed)
      (let*((weight   (gap-buffer-weight  line-ed))
            (cursor   (gap-buffer-cursor  line-ed))
            (lo       (gap-buffer-minimum line-ed))
            (hi       (gap-buffer-maximum line-ed))
            (iface    (%line-editor-pre-freeze lo hi))
            (vec      ((iface-make-sequence iface) weight))
            (seq-set! (iface-sequence-set! iface))
            )
        (gap-buffer-for-each/index
         (lambda (i n)
           (seq-set! vec i (- n lo))
           (set! i (+ 1 i))
           )
         line-ed-iface line-ed
         )
        (make<text-line> vec #f #f lo hi lbrk iface)
        ))

    (define (line-editor-freeze-line-after line-ed lbrk)
      ;; Freeze only the characters after the cursor.
      (cond
       ((gap-buffer-end-of-line? line-ed)
        (make<text-line> #f #f #f #f #f lbrk #f)
        )
       (else
        (let*((lo (gap-buffer-ref-after line-ed-iface line-ed))
              (hi lo)
              )
          (gap-buffer-for-each-after
           (lambda (n)
             (cond
              ((< n lo) (set! lo n))
              ((< hi n) (set! hi n))
              (else (values))
              ))
           line-ed-iface line-ed
           )
          (let*((iface    (%line-editor-pre-freeze hi lo))
                (weight   (gap-buffer-weight line-ed))
                (vec      ((iface-make-sequence iface) weight))
                (seq-set! ((iface-sequence-set! iface)))
                )
            (gap-buffer-for-each-after/index
             line-ed-iface line-ed
             (lambda (i n) (seq-set! vec i n))
             )
            (make<text-line> vec #f #f lo hi lbrk iface)
            )))))

    ;;----------------------------------------------------------------


    ;;----------------------------------------------------------------

    (define (run-editor-engine proc . args)
      (parameterize
          ((impl/new-buffer*       new-text-editor)
           (impl/buffer-type?*     text-editor-type?)
           )
        (apply proc args)
        ))

    ))

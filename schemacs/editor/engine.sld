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
    (only (scheme write) display write) ;;DEBUG
    (scheme case-lambda)
    (only (schemacs vector)
          u32vector?  make-u32vector  u32vector-ref  u32vector-set!
          u64vector?  make-u64vector
          )
    (only (schemacs vbal)
          vbal-type?  vbal->alist  alist->vbal
          )
    (only (schemacs lexer) make<source-file-location>)
    (prefix (schemacs ui text-buffer-impl) impl/)
    (only (schemacs ui text-buffer-impl)
          make<text-location>
          text-location-line
          text-location-column
          )
    (only (schemacs sequence)
          sequence-grow
          vector-sequence-iface
          u16vector-sequence-iface
          u32vector-sequence-iface
          u64vector-sequence-iface
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
          gap-buffer-for-each-before
          gap-buffer-for-each-before/index
          gap-buffer-map/index!  gap-buffer-map!
          gap-buffer-map/index   gap-buffer-map
          gap-buffer-update-min-max!
          gap-buffer-insert-min-max!
          gap-buffer-move-cursor
          gap-buffer-set-cursor
          gap-buffer-cursor-to-start!
          gap-buffer-cursor-to-end!
          gap-buffer-insert-before
          gap-buffer-insert-after
          gap-buffer-minimum     set!gap-buffer-minimum
          gap-buffer-maximum     set!gap-buffer-maximum
          gap-buffer-ref-before  gap-buffer-ref-after
          gap-buffer-cursor      gap-buffer-weight
          )
    )
  (export
   ;; Text lines, these are contain individual lines of text possibly
   ;; terminated with some line breaking character sequence.
   text-line-type?  new-text-line  text-line-size
   write-text-line  text-line-for-each

   ;; The text editor data type
   new-text-editor  text-editor-insert
   text-editor-force-line-break
   text-editor-cursor-line
   text-editor-cursor-column
   text-editor-cursor-location

   ;; Changing the line-break protocol for the editor
   text-editor-set-line-break!
   line-break-newline  line-break-return
   line-break-null  line-break-crlf  line-break-lfcr

   *init-text-editor-line-count*

   run-editor-engine
   ;; ^ This procedure is called in the same way as the Scheme
   ;; `apply` procedure, except that it parameterizes all of the
   ;; relevant parameter variables in the
   ;; `(schemacs ui text-buffer-impl)` library.
   )

  (begin

    ;;----------------------------------------------------------------
    ;; Cumulative Distribution Function (CDF): since most text editors
    ;; require moving the cursor to a precise index, as though the
    ;; text buffer were an array of characters, and since this editor
    ;; engine buffers variable-length lines and not characters, we
    ;; provide a way to efficiently map arbitrary character indicies
    ;; to line indicies. This is accomplished with a CDF which
    ;; precisely describes how characters are distributed throughout
    ;; the text buffer, that way character lookup by index can be
    ;; performed with a simple binary search in O(log n) time. The CDF
    ;; implementation defined here provides APIs for changing the
    ;; characters and lazily re-computing the distribution whenever
    ;; characters are inserted or removed from somewhere in the middle
    ;; of the text buffer.

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

    (define cdf-vector
      ;; Construct a cumulative distribution function (CDF) of type
      ;; `<cdf-vector-type>` of a given `SIZE` and (optionally) using
      ;; a given sequence interface `IFACE`. If `IFACE` is not provided
      ;; then the `u64vector-sequence-iface` is selected by default.
      ((size) (cdf-vector size u64vector-sequence-iface))
      ((size iface)
       (make<cdf-vector> iface ((make-vector iface) size) 0 0)
       ))

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
                  ((vec len)
                   (let ((new-vec (sequence-grow iface vec 1)))
                     (if new-vec
                         (values new-vec ((iface-sequence-length iface) new-vec))
                         (values vec len)
                         ))
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
             (car elems)
             ))))))

    (define (cdf-pop cdf n)
      (cdf-invalidate! cdf (max 0 (- (cdf-cursor cdf) n)))
      )

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
      (case-lambda
        ((cdf n) (cdf-find cdf #f n))
        ((cdf init n)
         (let*((iface (cdf-sequence-iface cdf))
               (ref (iface-sequence-ref iface))
               (vec (cdf-vector cdf))
               (len ((iface-sequence-length iface) vec))
               (init
                (or (and init (max 0 (min init (- len 1))))
                    (floor-quotient len 2)
                    )))
           (let loop ((interval 1) (i0 init))
             (let*((i0 (min i0 (- len 1)))
                   (i1 (+ 1 i0))
                   (lo (ref vec i0))
                   (hi (if (>= i1 len) #f (ref vec i1)))
                   )
               (cond
                ((and (<= lo n) (or (not hi) (< n hi)))
                 (values i0 lo)
                 )
                ((< n lo)
                 (let ((interval (floor-quotient interval 2)))
                   (loop interval (- i0 interval))
                   ))
                (else
                 (let ((interval (floor-quotient (- len i0) 2)))
                   (loop interval (+ i0 interval))
                   )))))))))

    ;;----------------------------------------------------------------
    ;; Line breaking state machine (not to be confused with a
    ;; break-dancing robot).

    (define-record-type <line-break-type>
      ;; This record type keeps a set of procedures and data related
      ;; to how certain line-break protocols are used. If the line
      ;; break scheme requires two characters, such as
      ;; line-feed->carriage-return (LFCR) or
      ;; carriage-return->line-feed (CRLF), The
      ;; `LINE-BREAK-SETUP-EDITOR!` acts as a state transition procedure
      ;; which waits for a second character to be inserted and then
      ;; decides what to do after. To use the procedure stored to
      ;; `LINE-BREAK-SETUP-EDITOR!` apply a text editor to it, the text
      ;; editor will be updated to have it's
      ;; `text-editor-insert-char` procedure set to the initial
      ;; state of the state machine.
      (make<line-break> str to-port ins-char)
      line-break-type?
      (str       line-break-as-string)
      (to-port   line-break-write-to-port)
      (ins-char  line-break-setup-editor!)
      )

    (define (line-break-2-state break-ch0 break-ch1)
      ;; To understand this procedure, consider the case where we are
      ;; constructing a state machine to handle CR-LF line breaks.
      (define (make-state-machine ed)
        (define (state-1 input-ch)
          ;; This is the procedure for the state that the text editor is
          ;; in ordinarily, having not yet received a CR character since
          ;; the last line break event.
          (cond
           ((char=? input-ch break-ch0)
            ;; On receiving the CR character we set the text editor to
            ;; the next state in the state machine, which awaits an LF
            ;; character.
            (set!text-editor-insert-char ed state-2)
            )
           (else
            ;; If we do not receive the CR character, trigger the
            ;; `ON-NON-BREAK` character event, which would insert the
            ;; character into the buffer as usual.
            (text-editor-force-insert-char ed input-ch)
            )))
        (define (state-2 input-ch)
          ;; This is the procedure for the state the the text editor is
          ;; in after having received the CR character. In this second
          ;; state, regardless of what character we receive, the text
          ;; editor always returns back to being in the first state.
          (set!text-editor-insert-char ed state-1)
          (cond
           ((char=? input-ch break-ch1)
            ;; If we receive the LF character, trigger the `ON-BREAK`
            ;; event, which will freeze the line editor and push the
            ;; line into the buffer, then reset the line editor.
            (text-editor-force-line-break ed)
            )
           (else
            ;; If we are in the state of having received a CR but then
            ;; we do not receive an LF, insert the CR as any ordinary
            ;; character.
            (text-editor-force-insert-char ed break-ch0)
            (text-editor-force-insert-char ed input-ch)
            )))
        state-1
        )
      (make<line-break>
       (let ((str (make-string 2)))
         (string-set! str 0 break-ch0)
         (string-set! str 1 break-ch1)
         str
         )
       (lambda (port)
         (write-char break-ch0 port)
         (write-char break-ch1 port)
         )
       (lambda (ed)
         (set!text-editor-insert-char ed (make-state-machine ed))
         )))

    (define (line-break-1-state break-ch)
      (make<line-break>
       (make-string 1 break-ch)
       (lambda (port) (write-char break-ch port))
       (lambda (ed)
         (set!text-editor-insert-char
          ed (lambda (input-ch)
               (cond
                ((char=? input-ch break-ch)
                 (text-editor-force-line-break ed)
                 )
                (else
                 (text-editor-force-insert-char ed input-ch)
                 )))))))

    (define line-break-crlf    (line-break-2-state #\return #\newline))
    (define line-break-lfcr    (line-break-2-state #\newline #\return))
    (define line-break-newline (line-break-1-state #\newline))
    (define line-break-return  (line-break-1-state #\return))
    (define line-break-null    (line-break-1-state #\null))

    (define (text-editor-set-line-break! ed lbrk)
      ;; Change the line breaking character used by the editor. By
      ;; default it is set to `line-break-newline`, which is the
      ;; `#\newline` character.
      ;;--------------------------------------------------------------
      (cond
       ((line-break-type? lbrk) ((line-break-setup-editor! lbrk) ed))
       (else (error "not a line-breaker configuration" lbrk))
       ))

    ;;----------------------------------------------------------------
    ;; "immutable" text lines

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

    (define (new-text-line string)
      (make<text-line> string #f #f #f #f #f (get-sequence-iface string))
      )

    (define (text-line-size line)
      ((iface-sequence-length (text-line-sequence-iface line))
       (text-line-string line)
       ))

    (define (text-line-ref line i)
      (integer->char
       (+ (text-line-char-offset line)
          ((iface-sequence-ref (text-line-sequence-iface line))
           (text-line-string line) i
           ))))

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
         (let ((lbrk (text-line-break line)))
           (text-line-for-each (lambda (i) (write-char (integer->char i))) line)
           (when lbrk ((line-break-write-to-port lbrk) port))
           ))))

    ;;----------------------------------------------------------------

    (define-record-type <text-editor-type>
      (make<text-editor>
       lines  count  line-ed  line-ch  moved
       cdf  ins-char  lbrk  textprops
       )
      text-editor-type?
      (lines      text-editor-lines         set!text-editor-lines)
      ;; ^ A <gap-buffer-type> which buffers <text-line-type> values.
      (count      text-editor-char-count    set!text-editor-char-count)
      ;; ^ Counting the number of characters.
      (line-ed    text-editor-line-editor   set!text-editor-line-editor)
      ;; ^ A <gap-buffer-type> which buffers characters, edits the
      ;; current line under the cursor.
      (line-ch    text-editor-line-changed  set!text-editor-line-changed)
      ;; ^ A boolean value indicating that the current line being edited
      ;; by the `text-editor-line-editor` has actually changed. This
      ;; allows the editor to decide whether the current line editor
      ;; needs to be frozen and written-back to the line buffer. If
      ;; there have been no edits when the cursor is moved, the freeze
      ;; and write-back step can be skipped.
      (moved      text-editor-line-moved    set!text-editor-line-moved)
      ;; ^ A boolean value indicating that the cursor of the
      ;; `text-editor-lines` gap buffer has moved and the line editor
      ;; need to be reset with the content of the current line.
      (cdf        text-editor-cdf           set!text-editor-cdf)
      ;; ^ The "Cumulative Distribution Function" is a gap buffer that
      ;; keeps a running total number of characters for each line in
      ;; the `text-editor-lines` gap buffer. Any change to the
      ;; `text-editor-lines` buffer erases everything after the cursor
      ;; in the CDF so that they can be re-computed.
      (ins-char   %text-editor-insert-char   set!text-editor-insert-char)
      ;; ^ A function which inserts characters into the editor.
      (lbrk       text-editor-line-break     set!text-editor-line-break)
      ;; ^ The current line-breaking protocol.
      (textprops  text-editor-text-props     set!text-editor-text-props)
      ;; A VBAL that contains text properties for ranges of text that
      ;; span multiple <text-line-type> values. This is useful for
      ;; syntax coloring as you can declare all characters between any
      ;; two (line,colunm) coordinates to have a particular tag.
      )

    (define line-ed-iface u32vector-sequence-iface)

    (define *init-text-editor-line-count* (make-parameter 4096))

    (define new-text-editor
      (case-lambda
        (() (new-text-editor line-break-newline))
        ((lbrk)
         (cond
          ((line-break-type? lbrk) (new-text-editor lbrk #f))
          ((or (vbal-type? lbrk) (list? lbrk)) (new-text-editor #f lbrk))
          (else (error "expecting properties list, or `line-break-type?`" lbrk))
          ))
        ((lbrk props)
         (let*((size (*init-text-editor-line-count*))
               (line (new-gap-buffer make-u32vector size))
               (ed (let ()
                     (set!gap-buffer-minimum line #xFFFFFFFF)
                     (set!gap-buffer-maximum line 0)
                     (make<text-editor>
                      (new-gap-buffer make-vector size)
                      0 line #f #f
                      (cdf-vector size sequence-u64vector-iface)
                      #f lbrk props
                      ))))
           ((line-break-setup-editor! lbrk) ed)
           ed
           ))))

    ;;----------------------------------------------------------------
    ;; Line editor procedures

    (define (line-editor-cursor-to-end! line-ed)
      (gap-buffer-cursor-to-end! line-ed-iface line-ed)
      )

    (define (line-editor-cursor-to-start! line-ed)
      (gap-buffer-cursor-to-start! line-ed-iface line-ed)
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
      ;;--------------------------------------------------------------
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

    (define (text-editor-line-editor-unfreeze ed)
      (when (text-editor-line-moved ed)
        (let*((line-ed  (text-editor-line-editor ed))
              (col-num  (gap-buffer-cursor line-ed))
              (lines    (text-editor-lines ed))
              (line-num (gap-buffer-cursor lines))
              (line
               (if (<= 0 line-num) #f (gap-buffer-ref lines (- line-num 1)))
               )
              (size (text-line-size line))
              )
          ;; Clear the current line editor, and size it to fit the new line.
          (gap-buffer-clear line-ed)
          (gap-buffer-allocate line-ed size)
          ;; First loop, fill the line editor from the start of the line.
          (let loop ((i 0))
            (cond
             ((< i col-num)
              (gap-buffer-insert-before line-ed (text-line-ref line i))
              (loop (+ 1 i))
              )
             (else (values))
             ))
          ;; Second loop, fill the line editor from the end of the line.
          (let loop ((i size))
            (cond
             ((<= col-num i)
              (let ((i (- i 1)))
                (gap-buffer-insert-after line-ed (text-line-ref line i))
                (loop i)
                ))
             (else (values))
             )))))

    (define (line-editor-char-range ref foreach line-ed)
      (let*((lo (ref line-ed #f))
            (hi lo)
            )
        (foreach
         (lambda (n)
           (cond
            ((< n lo) (set! lo n))
            ((< hi n) (set! hi n))
            (else (values))
            ))
         line-ed-iface line-ed
         )
        (values lo hi)
        ))

    (define (line-editor-freeze-part ref foreach foreach/index)
      (lambda (line-ed lbrk)
        (cond
         ((gap-buffer-end-of-line? line-ed)
          (make<text-line> #f #f #f #f #f lbrk #f)
          )
         (else
          (let-values (((lo hi) (line-editor-char-range ref foreach line-ed)))
            (let*((iface    (%line-editor-pre-freeze hi lo))
                  (weight   (gap-buffer-weight line-ed))
                  (vec      ((iface-make-sequence iface) weight))
                  (seq-set! (iface-sequence-set! iface))
                  )
              (foreach/index line-ed-iface line-ed (lambda (i n) (seq-set! vec i n)))
              (make<text-line> vec #f #f lo hi lbrk iface)
              ))))))

    (define line-editor-freeze-line-before
      (line-editor-freeze-part
       gap-buffer-ref-before
       gap-buffer-for-each-before
       gap-buffer-for-each-before/index
       ))

    (define line-editor-freeze-line-after
      (line-editor-freeze-part
       gap-buffer-ref-after
       gap-buffer-for-each-after
       gap-buffer-for-each-after/index
       ))

    ;;----------------------------------------------------------------
    ;; Inserting text

    (define (text-editor-force-line-break ed)
      (let*((line-ed (text-editor-line-editor ed))
            (line
             (line-editor-freeze-line-before
              line-ed (text-editor-line-break ed)
              ))
            (lines (text-editor-lines ed))
            (cur (gap-buffer-cursor lines))
            (cdf (text-editor-cdf ed))
            (sum (cdf-invalidate! cdf cur))
            )
        ;; insert the line into the line buffer
        (gap-buffer-insert-before (text-editor-lines ed) line)
        ;; insert the next running total into the CDF buffer, if the
        ;; CDF is up-to-date. If not, do nothing, the CDF buffer will
        ;; have to be brought up-to-date later.
        (when sum (cdf-push cdf (+ (text-line-size line) sum)))
        (gap-buffer-clear line-ed)
        line
        ))

    (define (text-editor-insert ed thing)
      (cond
       ((char? thing)
        ((%text-editor-insert-char ed) thing)
        )
       ((string? thing)
        (string-for-each (%text-editor-insert-char ed) thing)
        )
       ((and (input-port? thing) (input-port-open? thing))
        (text-editor-insert-line-from-port ed thing)
        )
       (else (error "editor cannot insert text from" thing))
       ))

    (define (text-editor-insert-char ed ch)
      (cond
       ((char? ch) ((%text-editor-insert-char ed) ch))
       (else (error "not a character" ch))
       ))

    (define (text-editor-force-insert-char ed ch)
      (text-editor-line-editor-unfreeze ed)
      (let ((line-ed (text-editor-line-editor ed))
            (chi (char->integer ch))
            )
        (gap-buffer-insert-before line-ed-iface line-ed chi)
        (gap-buffer-insert-min-max! line-ed chi)
        (set!text-editor-char-count ed (+ 1 (text-editor-char-count ed)))
        (set!text-editor-line-changed ed #t)
        ch
        ))

    (define (text-editor-insert-from-port-until until ed port)
      (let loop ((next (read-char port)))
        (cond
         ((eof-object? next) next)
         (else
          (let ((result ((%text-editor-insert-char ed) next)))
            (if (until result) result (loop (read-char port)))
            )))))

    (define (text-editor-insert-from-port ed port)
      (text-editor-insert-from-port-until (lambda _ #f) ed port)
      )

    (define (text-editor-insert-line-from-port ed port)
      (text-editor-insert-from-port-until text-line-type? ed port)
      )

    (define (text-editor-dump-before ed port)
      (let*((line-gb (text-editor-lines ed))
            (line (gap-buffer-cursor line-gb))
            (changed (text-editor-line-changed ed))
            )
        (cond
         (changed
          (let ((end (max 0 (- line 1))))
            (gap-buffer-for-each-before/index
             (lambda (i text-line)
               (when (< i end) (write-text-line text-line port))
               )
             line-gb
             )
            (gap-buffer-for-each-before
             (lambda (ch) (write-char (integer->char ch) port))
             (text-editor-line-editor ed)
             )))
         (else
          (gap-buffer-for-each-before
           (lambda (text-line)
             (write-text-line text-line port)
             )
           line-gb
           )))))

    (define (text-editor-dump-after ed port)
      (let*((line-gb (text-editor-lines ed))
            (line    (gap-buffer-cursor line-gb))
            (changed (text-editor-line-changed ed))
            )
        (when changed
          (gap-buffer-for-each-after
           (lambda (ch) (write-char (integer->char ch) port))
           (text-editor-line-editor ed)
           ))
        (gap-buffer-for-each-after
         (lambda (text-line) (write-text-line text-line port))
         line-gb
         )))

    (define (text-editor-dump ed port)
      (text-editor-dump-before ed port)
      (text-editor-dump-after ed port)
      )

    (define (text-load-port ed port _flags)
      (text-editor-insert-from-port ed port)
      )

    (define (text-dump-port ed port _flags)
      (text-editor-dump ed port)
      )

    (define (text-editor-cursor-line ed)
      (let ((lines (text-editor-lines ed)))
        (or (and lines (gap-buffer-cursor lines)) 0)
        ))

    (define (text-editor-cursor-column ed)
      (let ((line-ed (text-editor-line-editor ed)))
        (or (and line-ed (gap-buffer-cursor line-ed)) 0)
        ))

    (define (text-editor-cursor-location ed)
      (make<source-file-location>
       #f
       (text-editor-cursor-line-number ed)
       (text-editor-cursor-column-number ed)
       ))

    (define (text-editor-make-cdf-fill-range lines to)
      ;; Creates a closure that acts as a generator of lines in the
      ;; `LINES` gap buffer between the indices of the current cursor
      ;; position of the CDF up until the index given by the argument
      ;; `TO` (not including `TO`) using `gap-buffer-ref`, and
      ;; `text-line-size` to produce the output values.
      ;;--------------------------------------------------------------
      (lambda (cursor accum)
        (cond
         ((< cursor to)
          (text-line-size (gap-buffer-ref lines cursor))
          )
         (else #f)
         )))

    (define (text-editor-make-cdf-fill-until lines accum-max-value)
      ;; Creates a closure that acts as a generator of lines in the
      ;; `LINES` gap buffer and continues until the end of the buffer
      ;; is reached or until the accumulator matches or exceeds that
      ;; of `ACCUM-MAX-VALUE`.
      ;;--------------------------------------------------------------
      (let ((weight (gap-buffer-weight lines)))
        (lambda (cursor accum)
          (cond
           ((and (< accum accum-max-value) (< from weight))
            (text-line-size (gap-buffer-ref lines cursor))
            )
           (else #f)
           ))))

    (define (text-editor-text-line-ref ed offset)
      ;; Used internally to get the character on the current
      ;; line. Checks if the line has been editted first, then decides
      ;; whether to lookup the character from the line buffer or from
      ;; the text line under the cursor.
      (let*((lines (text-editor-lines ed))
            (line-cur (gap-buffer-cursor lines))
            )
        (cond
         ;; Under two conditions do we read from the line editor: (1)
         ;; if the cursor is at zero, which means the line editor is
         ;; editing a line at the beginning of the buffer, or (2) if
         ;; the line editor contains changes from the text-line in the
         ;; line buffer.
         ((or (= 0 line-cur) (text-editor-line-changed ed))
          (gap-buffer-ref (text-editor-line-editor ed) offset)
          )
         ;; Otherwise we read from the text line in the line buffer
         (else
          (text-line-ref (gap-buffer-ref lines (- line-cur 1)) offset)
          ))))

    (define (text-editor-get-cursor ed)
      ;; Check if the CDF needs updating, and if so, recompute all
      ;; elements up to the current cursor position. Returns the
      ;; character position of the text editor's cursor when complete.
      ;;--------------------------------------------------------------
      (let*((lines    (text-editor-lines ed))
            (line-num (gap-buffer-cursor lines))
            (line-ed  (text-editor-line-editor ed))
            (vec      (gap-buffer-vector lines))
            (cdf      (text-editor-cdf ed))
            (cdf-cur  (cdf-cursor cdf))
            (offset
             (cond
              ((< cdf-cur line-num)
               (cdf-fill
                cdf (text-editor-make-cdf-fill-range lines line-num)
                ))
              (else
               (cdf-ref cdf line-num)
               ))))
        (+ offset (or (and line-ed (gap-buffer-cursor line-ed)) 0))
        ))

    (define (text-editor-move-cursor ed move-by)
      (let*-values
          (((lines) (text-editor-lines ed))
           ((line-num-before) (gap-buffer-cursor lines))
           ((cdf-cur offset)
            (text-editor-index-line-offset ed (+ ch-index move-by))
            )
           ;; First set the cursor position according to the value
           ;; computed from the CDF.
           (() (gap-buffer-set-cursor (text-editor-lines ed) cdf-cur))
           ((line-num-after)  (gap-buffer-cursor lines))
           )
        ;; Then make a note that the text editor line changed and
        ;; needs to be reset.
        (set!text-editor-line-changed ed #t)
        (unless (= line-num-before line-num-after)
          (set!text-editor-line-moved ed #t)
          )))

    (define (text-editor-set-cursor ed index)
      (cond
       ((text-location-type? index)
        ;;TODO
        )
       ((integer? index)
        (let ((cursor (text-editor-get-cursor ed)))
          (text-editor-move-cursor ed (- index cursor))
          ))
       (else
        (error
         "text editor index must be set with integer or text-location-type"
         index
         ))))

    (define (text-editor-index-line-offset ed ch-index)
      ;; This function is used to update the CDF and to return the
      ;; line number, and character index offset of that line, for the
      ;; character index `CH-INDEX` relative to the start of the text
      ;; buffer. Returns two values: (1) the line index to which the
      ;; `CH-INDEX` is pointing, and (2) the character offset of that
      ;; line.
      ;;--------------------------------------------------------------
      (let*((lines (text-editor-lines ed))
            (cdf (text-editor-cdf ed))
            (cdf-max (cdf-maximum cdf))
            )
        (cond
         ((< ch-index cdf-max)
          (let*((cdf-cur (cdf-find cdf ch-index))
                (offset (cdf-ref cdf cdf-cur))
                )
            (values cdf-cur offset)
            ))
         (else
          (cdf-fill cdf (text-editor-make-cdf-fill-until lines ch-index))
          (let((cdf-cur (cdf-cursor cdf)))
            (cond
             ((<= cdf-cur 0) #f)
             (else
              (let*((cdf-cur (- cdf-cur 1))
                    (offset (cdf-ref cdf-cur))
                    )
                (values cdf-cur offset)
                ))))))))

    (define (text-buffer-get-char-index ed ch-index)
      ;; Get the character at the given index `CH-INDEX`. This
      ;; recomputes part of the CDF for the editor buffer.
      ;;--------------------------------------------------------------
      (let*-values
          (((cdf-cur offset) (text-editor-index-line-offset ed ch-index))
           ((lines) (text-editor-lines ed))
           )
        (cond
         ((= cdf-cur (text-editor-line-index ed))
          (text-editor-line-editor-ref ed offset)
          )
         (else
          (let ((line (gap-buffer-ref lines cdf-cur)))
            (text-line-ref line (- ch-index offset))
            )))))

    (define (text-buffer-get-line-column ed ch-index)
      (cond
       ;; If `index` is not `#f` compute the line and column number of
       ;; that character index.
       (ch-index
        (let*-values
            (((line-index offset)
              (text-editor-index-line-offset ed ch-index)
              ))
          (make<text-location> (+ 1 line-index) (+ 1 offset))
          ))
       ;; Otherwise get the current cursor position.
       (else
        (make<text-location>
         (+ 1 (text-edtior-cursor-line ed))
         (+ 1 (text-editor-cursor-column ed))
         ))))

    (define (text-editor-get-end-of-line ed)
      (text-editor-get-cursor ed)
      (let*((lines (text-editor-lines ed))
            (line-num (gap-buffer-cursor lines))
            )
        (cond
         ((= 0 line-num)
          (cond
           ((text-editor-line-moved ed) #f)
           (else (- (gap-buffer-weight (text-editor-line-editor ed)) 1))
           ))
         (else
          (- (cdf-ref (text-editor-cdf ed) (- line-num 1)) 1)
          ))))

    (define (text-editor-get-start-of-line ed)
      (text-editor-get-cursor ed)
      (let*((lines (text-editor-lines ed))
            (line-num (gap-buffer-cursor lines))
            )
        (cond
         ((< line-num 2)
          (cond
           ((text-editor-line-moved ed) #f)
           (else 0)
           ))
         (else
          (- (cdf-ref (text-editor-cdf ed) (- line-num 2)) 1)
          ))))

    ;;----------------------------------------------------------------

    (define (run-editor-engine proc . args)
      (parameterize
          ((impl/new-buffer*           new-text-editor)
           (impl/buffer-type?*         text-editor-type?)
           (impl/buffer-length*        text-editor-char-count)
           (impl/text-load-port*       text-load-port)
           (impl/text-dump-port*       text-dump-port)
           (impl/style-type?*          vbal-type?)
           (impl/new-style*            alist->vbal)
           (impl/get-cursor-index*     text-editor-get-cursor)
           (impl/move-cursor-index*    text-editor-move-cursor)
           (impl/set-cursor-position*  text-editor-set-cursor)
           (impl/index->line-column*   text-editor-get-line-column)
           (impl/get-end-of-line*      text-editor-get-end-of-line)
           (impl/get-start-of-line*    text-editor-get-start-of-line)
           (impl/insert*               text-editor-insert)
           (impl/copy-string*          '*TODO*)
           (impl/get-char*             '*TODO*)
           (impl/delete-range*         '*TODO*)
           (impl/delete-from-cursor*   '*TODO*)
           (impl/get-default-style*    '*TODO*)
           (impl/set-default-style*    '*TODO*)
           (impl/get-text-style*       '*TODO*)
           (impl/set-text-style*       '*TODO*)
           (impl/get-selection*        '*TODO*)
           (impl/set-selection*        '*TODO*)
           (impl/scan-for-char*        '*TODO*)
           (impl/scan-for-string*      '*TODO*)
           )
        (apply proc args)
        ))

    )
  )

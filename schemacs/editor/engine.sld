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
          )
    (only (schemacs vbal)
          vbal-type?  vbal->alist  alist->vbal
          )
    (only (schemacs lexer) make<source-file-location>)
    (prefix (schemacs ui text-buffer-impl) impl/)
    (only (schemacs sequence)
          vector-sequence-iface
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
          gap-buffer-for-each-before
          gap-buffer-for-each-before/index
          gap-buffer-map/index!  gap-buffer-map!
          gap-buffer-map/index   gap-buffer-map
          gap-buffer-update-min-max!
          gap-buffer-insert-min-max!
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
   text-line-type?  new-text-line
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
      (make<text-editor> lines line-ed cdf ins-char lbrk textprops)
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
                      line (make-u32vector size)
                      #f lbrk props
                      ))))
           ((line-break-setup-editor! lbrk) ed)
           ed
           ))))

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

    (define (line-editor-char-range ref foreach line-ed)
      (let*((lo (ref line-ed-iface line-ed))
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
      (let ((line
             (line-editor-freeze-line-before
              (text-editor-line-editor ed)
              (text-editor-line-break ed)
              )))
        (gap-buffer-insert-before
         vector-sequence-iface (text-editor-lines ed) line
         )
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
      (let ((line-ed (text-editor-line-editor ed))
            (chi (char->integer ch))
            )
        (gap-buffer-insert-before line-ed-iface line-ed chi)
        (gap-buffer-insert-min-max! line-ed chi)
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

    ;;----------------------------------------------------------------

    (define (run-editor-engine proc . args)
      (parameterize
          ((impl/new-buffer*       new-text-editor)
           (impl/buffer-type?*     text-editor-type?)
           )
        (apply proc args)
        ))

    ))

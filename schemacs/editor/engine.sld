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
    (only (scheme write) display write) ;;DEBUG
    (scheme case-lambda)
    (only (schemacs vector)
          u32vector?  make-u32vector  u32vector-ref  u32vector-set!
          u64vector?  make-u64vector
          )
    (only (schemacs vbal)
          vbal-type?  vbal->alist  alist->vbal
          )
    ;;(only (schemacs lexer) make<source-file-location>)
    (only (schemacs editor cdf)
          new-cdf  cdf-cursor  cdf-maximum  cdf-ref
          cdf-fill  cdf-invalidate!  cdf-push  cdf-find
          )
    (prefix (schemacs ui text-buffer-impl) impl/)
    (only (schemacs ui text-buffer-impl)
          make<text-location>  text-location-type?
          text-location-line   text-location-column
          text-location  show-text-location
          )
    (only (schemacs sequence)
          *sequence-allocate-function*
          sequence-resize
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
          new-gap-buffer              gap-buffer-allocate
          gap-buffer-end-of-line?     gap-buffer-start-of-line?
          gap-buffer-for-each         gap-buffer-for-each/index
          gap-buffer-for-each-after   gap-buffer-for-each-after/index
          gap-buffer-for-each-before  gap-buffer-for-each-before/index
          gap-buffer-update-min-max   gap-buffer-insert-min-max
          gap-buffer-set-cursor       gap-buffer-ref
          gap-buffer-ref-before       gap-buffer-ref-after
          gap-buffer-cursor-to-start  gap-buffer-cursor-to-end
          gap-buffer-insert-before    gap-buffer-insert-after
          gap-buffer-minimum          set!gap-buffer-minimum
          gap-buffer-maximum          set!gap-buffer-maximum
          gap-buffer-cursor           gap-buffer-weight
          gap-buffer-clear-before     gap-buffer-clear
          )
    )
  (cond-expand
   ;; To define pretty-printers for Guile
   (guile-3
    (import (only (srfi srfi-9 gnu) set-record-type-printer!))
    )
   (else)
   )
  (export
   ;; Text lines, these are contain individual lines of text possibly
   ;; terminated with some line breaking character sequence.
   text-line-type?  new-text-line  text-line
   text-line-inner-size  text-line-outer-size
   write-text-line  text-line-for-each
   text-line-ref    text-line-code-ref
   text-line->string  show-text-line

   ;; The text editor data type
   new-text-editor  text-editor-type?
   *init-text-editor-line-count*
   text-editor-char-count
   text-load-port  text-dump-port  text-editor-to-string
   text-editor-insert

   ;; Changing the line-break protocol for the editor
   text-editor-set-line-break!
   line-break-newline  line-break-return
   line-break-null  line-break-crlf  line-break-lfcr
   line-break-bytevector  line-break-write-to-port
   line-break-size   *default-line-break*
   line-break  show-line-break

   ;; Getting and setting the cursor index
   text-editor-char-count
   text-editor-cursor-line
   text-editor-cursor-column
   text-editor-cursor-location
   text-editor-get-start-of-line
   text-editor-get-end-of-line
   text-editor-get-line-column
   text-editor-set-cursor
   text-editor-move-cursor
   text-editor-get-cursor
   text-editor-get-char-index
   text-editor-line-editor-ref
   text-editor-text-line-ref
   text-editor  show-text-editor

   run-editor-engine
   ;; ^ This procedure is called in the same way as the Scheme
   ;; `apply` procedure, except that it parameterizes all of the
   ;; relevant parameter variables in the
   ;; `(schemacs ui text-buffer-impl)` library.
   )

  (begin

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

    (define cdf-sequence-iface u64vector-sequence-iface)

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
      (make<line-break> bv str to-port ins-char)
      line-break-type?
      (bv        line-break-bytevector)
      (str       line-break-string)
      (to-port   line-break-write-to-port)
      (ins-char  line-break-setup-editor!)
      )

    (define (line-break-size lbrk)
      (bytevector-length (line-break-bytevector lbrk))
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
       (let ((bv (make-bytevector 2)))
         (bytevector-u8-set! bv 0 (char->integer break-ch0))
         (bytevector-u8-set! bv 1 (char->integer break-ch1))
         bv
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
       (make-bytevector 1 (char->integer break-ch))
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

    (define (line-break str)
      (cond
       ((not str) #f)
       ((line-break-type? str) str)
       ((char? str)
        (cond
         ((char=? str #\newline) line-break-newline)
         ((char=? str #\return)  line-break-return)
         ((char=? str #\null)    line-break-null)
         (else (error "invalid line break" str))
         ))
       ((string? str)
        (cond
         ((string=? str "\n")   line-break-newline)
         ((string=? str "\r")   line-break-return)
         ((string=? str "\n\r") line-break-crlf)
         ((string=? str "\r\n") line-break-lfcr)
         ((string=? str "\0")   line-break-null)
         (else (error "invalid line break" str))
         ))
       ((symbol? str)
        (cond
         ((eq? str 'crlf)    line-break-crlf)
         ((eq? str 'lfcr)    line-break-lfcr)
         ((eq? str 'newline) line-break-newline)
         ((eq? str 'return)  line-break-return)
         ((eq? str 'null)    line-break-null)
         (else (error "unknown line break type" str))
         ))
       (else (error "not a string or char" str))
       ))

    (define (line-break->string lbrk)
      (cond
       ((not lbrk) #f)
       ((line-break-type? lbrk) (line-break-string lbrk))
       ((string? lbrk) lbrk)
       ((char? lbrk) lbrk)
       (else (error "not a line break type" lbrk))
       ))

    (define show-line-break
      (case-lambda
       ((lbrk) (show-line-break lbrk (current-output-port)))
       ((lbrk port)
        (display "(line-break " port)
        (write (line-break->string lbrk) port)
        (display ")" port)
        )))

    (cond-expand
     (guile
      (set-record-type-printer! <line-break-type> show-line-break)
      )
     (else)
     )

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

    (define new-text-line
      (case-lambda
       ((string) (new-text-line (get-sequence-iface string) string))
       ((iface string)
        (make<text-line> string #f #f #f #f #f iface)
        )))

    (define (text-line-inner-size line)
      ;; Return the number of characters in the text line
      ;; *NOT_INCLUDING* the line break.
      ;;--------------------------------------------------------------
      (let ((iface (text-line-sequence-iface line)))
        ;; If `iface` is `#f` this is an indication that the line is empty
        (cond
         (iface ((iface-sequence-length iface) (text-line-string line)))
         (else 0)
         )))

    (define (text-line-outer-size line)
      ;; Return the number of characters in the text line including
      ;; the line break.
      ;;--------------------------------------------------------------
      (let ((lbrk (text-line-break line)))
        (+ (text-line-inner-size line)
           (if lbrk (line-break-size lbrk) 0)
           )))

    (define (text-line-ref line i)
      ;; Lookup a character in the `LINE` at the given index `I`.
      ;;--------------------------------------------------------------
      (integer->char (text-line-code-ref line i))
      )

    (define (text-line-code-ref line i)
      ;; Like `text-line-ref` but returns the UTF code point, rather
      ;; than a `char?` value.
      ;;--------------------------------------------------------------
      (let*((iface (text-line-sequence-iface line))
            (str (text-line-string line))
            (len (if iface ((iface-sequence-length iface) str) 0))
            )
        (cond
         ((< i 0) #f)
         ((< i len)
          (+ (text-line-char-offset line)
             ((iface-sequence-ref (text-line-sequence-iface line))
              (text-line-string line) i
              )))
         (else
          (let*((lbrk (text-line-break line))
                (lbrk-str (and lbrk (line-break-bytevector lbrk)))
                (lbrk-len (and lbrk-str (bytevector-length lbrk-str)))
                (i (and lbrk-len (- i len)))
                )
            (cond
             ((and i (< i lbrk-len)) (bytevector-u8-ref lbrk-str i))
             (else #f)
             ))))))

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
      ;; Write the content of a `text-line-type?` to a port. If the
      ;; text line applied is the only argument, and no port is
      ;; applied as an argument, then the port returned by
      ;; `current-output-port` is used.
      ;;--------------------------------------------------------------
      (case-lambda
        ((line) (write-text-line line (current-output-port)))
        ((line port)
         (let ((lbrk (text-line-break line)))
           (text-line-for-each (lambda (c) (write-char c port)) line)
           (when lbrk ((line-break-write-to-port lbrk) port))
           ))))

    (define (text-line->string line)
      (call-with-port (open-output-string)
        (lambda (port)
          (write-text-line line port)
          (get-output-string port)
          )))

    (define (text-line str)
      ;; Construct a text line from a string `STR`. The given `STR` is
      ;; copied into a new character vector up to but not including
      ;; any line breaking character (if any). All characters after a
      ;; line breaking character are ignored. Line breaking characters
      ;; include `#\newline`, `#\return`, and `#\null`.
      ;;--------------------------------------------------------------
      (cond
       ((string? str)
        (let ((len (string-length str)))
          (let loop ((lo #x10FFFF) (hi 0) (count 0))
            (let ((ch (and (< count len) (string-ref str count))))
              (cond
               ((or (not ch) 
                    (char=? ch #\newline)
                    (char=? ch #\return)
                    (char=? ch #\null)
                    )
                (let*((iface (%line-editor-pre-freeze lo hi))
                      (set-char! (iface-sequence-set! iface))
                      (vec ((iface-make-sequence iface) count))
                      )
                  (let loop ((i 0))
                    (cond
                     ((< i count)
                      (set-char! vec (- (char->integer (string-ref str i)) lo))
                      (loop (+ 1 i))
                      )
                     (else (make<text-line> vec #f #f lo hi #f iface))
                     ))))
               (else
                (let ((pt (char->integer ch)))
                  (loop (min lo pt) (max hi pt) (+ 1 count))
                  )))))))
       (else (error "not a string" str))
       ))

    (define show-text-line
      (case-lambda
       ((line) (show-text-line line (current-output-port)))
       ((line port)
        (display "(text-line " port)
        (write (text-line->string line) port)
          ;; ^ TODO: this need to output characters WITHOUT allocating
          ;; a string copy of the line first
        (display ")" port)
        )))

    (cond-expand
     (guile
      (set-record-type-printer! <text-line-type> show-text-line)
      )
     (else)
     )

    ;;----------------------------------------------------------------

    (define-record-type <text-editor-type>
      (make<text-editor>
       lines  count  line-ed  line-ch  moved  column
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
      (column     text-editor-column        set!text-editor-column)
      ;; ^ When the selected line changes, the column number of the cursor
      ;; may be lost. This field keeps a record of the column number.
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

    (define *init-text-editor-line-count* (make-parameter 4096))

    (define *default-line-break* (make-parameter line-break-newline))

    (define new-text-editor
      (case-lambda
        (() (new-text-editor #f #f))
        ((lbrk)
         (cond
          ((line-break-type? lbrk) (new-text-editor lbrk #f))
          ((or (vbal-type? lbrk) (list? lbrk)) (new-text-editor #f lbrk))
          (else (error "expecting properties list, or `line-break-type?`" lbrk))
          ))
        ((lbrk props)
         (cond
          ((and lbrk (not (line-break-type? lbrk)))
           (error "first argument not a line-break-type" lbrk)
           )
          ((and props (not (or (vbal-type? props) (list? props))))
           (error "second argument not a properties list" props)
           )
          (else
           (let*((size (*init-text-editor-line-count*))
                 (line (new-gap-buffer u32vector-sequence-iface size))
                 (lbrk (or lbrk (*default-line-break*)))
                 (ed (let ()
                       (set!gap-buffer-minimum line #xFFFFFFFF)
                       (set!gap-buffer-maximum line 0)
                       (make<text-editor>
                        (new-gap-buffer vector-sequence-iface size)
                        0 line #f #f 0
                        (new-cdf u64vector-sequence-iface size)
                        #f lbrk props
                        ))))
             ((line-break-setup-editor! lbrk) ed)
             ed
             ))))))

    (define (text-editor location lbrk . lines)
      ;; Construct a text editor from a list of text lines.
      ;;--------------------------------------------------------------
      (let ((ed (new-text-editor lbrk)))
        (let loop ((lines lines))
          (cond
           ((null? lines)
            (when location (text-editor-set-cursor ed location))
            ed)
           (else
            (text-editor-insert ed (car lines))
            (loop (cdr lines))
            )))))

    (define (show-text-editor-single-line port)
      (lambda (line)
        (display "  " port)
        (write (text-line->string line) port)
        (newline port)
        ))

    (define show-text-editor
      ;; Write the whole content of a text editor to a port.
      ;;--------------------------------------------------------------
      (case-lambda
       ((ed) (show-text-editor ed (current-output-port)))
       ((ed port)
        (let*((lines (text-editor-lines ed))
              (weight (gap-buffer-weight lines))
              (location (text-editor-cursor-location ed))
              )
          (display "(text-editor " port)
          (show-text-location location port)
          (cond
           ((= weight 0) (display ")" port))
           (else
            (newline port)
            (gap-buffer-for-each-before (show-text-editor-single-line port) lines)
            ;; TODO: output current gap buffer, if necessary.
            (gap-buffer-for-each-after (show-text-editor-single-line port) lines)
            (display "  )\n" port)
            ))))))

    (cond-expand
     (guile
      (set-record-type-printer! <text-editor-type> show-text-editor)
      )
     (else)
     )

    ;;----------------------------------------------------------------
    ;; Line editor procedures

    (define (line-editor-cursor-to-end! line-ed)
      (gap-buffer-cursor-to-end line-ed)
      )

    (define (line-editor-cursor-to-start! line-ed)
      (gap-buffer-cursor-to-start line-ed)
      )

    (define (%line-editor-pre-freeze lo hi)
      (or
       (and lo hi
        (let*((range (abs (- hi lo))))
          (cond
           ((<= range #xFF) bytevector-sequence-iface)
           ((<= range #xFFFF) u16vector-sequence-iface)
           (else #f)
           )))
       u32vector-sequence-iface
       ))

    (define (line-editor-freeze line-ed lbrk)
      ;; Freeze all characters in the line buffer into a new
      ;; `<text-line-type>` object that contains the exact right size
      ;; to hold all of the characters.
      ;;--------------------------------------------------------------
      (gap-buffer-update-min-max line-ed)
      (let*((weight   (gap-buffer-weight  line-ed))
            (cursor   (gap-buffer-cursor  line-ed))
            (lo       (gap-buffer-minimum line-ed))
            (hi       (gap-buffer-maximum line-ed))
            (iface    (%line-editor-pre-freeze lo hi))
            (vec      ((iface-make-sequence iface) weight))
            (seq-set! (iface-sequence-set! iface))
            )
        (gap-buffer-for-each/index
         (lambda (i n) (seq-set! vec i (- n lo)))
         line-ed
         )
        (make<text-line> vec #f #f lo hi lbrk iface)
        ))

    (define (text-editor-line-editor-unfreeze ed)
      (when (text-editor-line-moved ed)
        (let*((line-ed  (text-editor-line-editor ed))
              (col-num  (text-editor-column ed))
              (lines    (text-editor-lines ed))
              (line-num (gap-buffer-cursor lines))
              (line
               (if (<= line-num 0) #f (gap-buffer-ref lines (- line-num 1)))
               )
              (size (or (and line (text-line-inner-size line)) 0))
              )
          ;; Clear the current line editor, and size it to fit the new line.
          (gap-buffer-clear line-ed)
          (gap-buffer-allocate line-ed size)
          ;; First loop, fill the line editor from the start of the line.
          (let loop ((i 0))
            (cond
             ((< i col-num)
              (gap-buffer-insert-before
               line-ed (text-line-code-ref line i)
               )
              (loop (+ 1 i))
              )
             (else (values))
             ))
          ;; Second loop, fill the line editor from the end of the line.
          (let loop ((i size))
            (cond
             ((<= col-num i)
              (let ((i (- i 1)))
                (gap-buffer-insert-after
                 line-ed (text-line-code-ref line i)
                 )
                (loop i)
                ))
             (else (values))
             ))
          ;; Be sure to reset the `text-editor-line-changed`
          (set!text-editor-line-changed ed #f)
          (set!text-editor-line-moved ed #f)
          )))

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
         line-ed
         )
        (values lo hi)
        ))

    (define (line-editor-freeze-part calc-frozen-size ref foreach foreach/index)
      (lambda (line-ed lbrk)
        (let*((weight (gap-buffer-weight line-ed))
              (cursor (gap-buffer-cursor line-ed))
              (frozen-size (calc-frozen-size weight cursor))
              )
          (cond
           ((< 0 weight)
            (let-values (((lo hi) (line-editor-char-range ref foreach line-ed)))
              (let*((iface    (%line-editor-pre-freeze lo hi))
                    (vec      ((iface-make-sequence iface) frozen-size))
                    (seq-set! (iface-sequence-set! iface))
                    )
                (foreach/index (lambda (i n) (seq-set! vec i (- n lo))) line-ed)
                (make<text-line> vec #f #f lo hi lbrk iface)
                )))
           (else (make<text-line> #f #f #f #f #f lbrk #f))
           ))))

    (define line-editor-freeze-line-before
      (line-editor-freeze-part
       (lambda (_weight cursor) cursor)
       gap-buffer-ref-before
       gap-buffer-for-each-before
       gap-buffer-for-each-before/index
       ))

    (define (line-editor-freeze-line-after line-ed lbrk)
      (cond
       ((gap-buffer-end-of-line? line-ed)
        (make<text-line> #f #f #f #f #f lbrk #f)
        )
      (else
       (let ((freeze
              (line-editor-freeze-part
               (lambda (weight cursor) (- weight cursor))
               gap-buffer-ref-after
               gap-buffer-for-each-after
               gap-buffer-for-each-after/index
               )))
       (freeze line-ed lbrk)
       ))))

    ;;----------------------------------------------------------------
    ;; Inserting text

    (define (text-editor-force-line-break ed)
      ;; Forces a line break regardless of whether an actual line
      ;; breaking character has been inserted. This will create a new
      ;; <text-line-type> object by freezing all characters in the
      ;; line edtior before the cursor and pushing the
      ;; <text-line-type> object to the buffer before the line
      ;; cursor. The characters after the line editor cursor remain
      ;; buffered in the line editor, this way, if a line break occurs
      ;; while the line editor cursor is in the middle of a line, only
      ;; the characters before the cursor are frozen and buffered, the
      ;; line editor characters after the cursor remain on the same
      ;; line as the cursor.
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
        (when sum (cdf-push cdf (text-line-outer-size line)))
        (gap-buffer-clear-before line-ed)
        (set!text-editor-line-changed
         ed (< 0 (gap-buffer-weight line-ed))
         )
        line
        ))

    (define (text-editor-insert ed thing)
      (cond
       ((text-line-type? thing)
        (text-line-for-each
         (lambda (c) ((%text-editor-insert-char ed) c))
         thing
         ))
       ((string? thing)
        (string-for-each (%text-editor-insert-char ed) thing)
        )
       ((char? thing)
        ((%text-editor-insert-char ed) thing)
        )
       ((and (input-port? thing) (input-port-open? thing))
        (text-editor-insert-line-from-port ed thing)
        )
       (else (error "editor cannot insert text from" thing))
       ))

    (define (text-editor-force-insert-char ed ch)
      (text-editor-line-editor-unfreeze ed)
      (let ((line-ed (text-editor-line-editor ed))
            (chi (char->integer ch))
            )
        (gap-buffer-insert-before line-ed chi)
        (gap-buffer-insert-min-max line-ed chi)
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

    (define text-load-port
      (case-lambda
       ((ed port) (text-load-port ed port #f))
       ((ed port _flags) (text-editor-insert-from-port ed port))
       ))

    (define text-dump-port
      (case-lambda
       ((ed port) (text-dump-port ed port #f))
       ((ed port _flags) (text-editor-dump ed port))
       ))

    (define (text-editor-to-string ed)
      ;; Dump the text editor buffer into a string.
      (call-with-port (open-output-string)
        (lambda (port)
          (text-dump-port ed port)
          (get-output-string port)
          )))

    (define (text-editor-cursor-line ed)
      (let ((lines (text-editor-lines ed)))
        (or (and lines (gap-buffer-cursor lines)) 0)
        ))

    (define (text-editor-cursor-column ed)
      (let ((line-ed (text-editor-line-editor ed)))
        (or (and line-ed (gap-buffer-cursor line-ed)) 0)
        ))

    (define (text-editor-cursor-line-number ed)
      (gap-buffer-cursor (text-editor-lines ed))
      )

    (define (text-editor-cursor-column-number ed)
      (gap-buffer-cursor (text-editor-line-editor ed))
      )

    (define (text-editor-cursor-location ed)
      (make<text-location>
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
          (text-line-outer-size (gap-buffer-ref lines cursor))
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
           ((and (< accum accum-max-value) (< cursor weight))
            (text-line-outer-size (gap-buffer-ref lines cursor))
            )
           (else #f)
           ))))

    (define (text-editor-text-line-ref ed offset)
      ;; Get a whole line of text given the line number.
      ;;--------------------------------------------------------------
      (gap-buffer-ref (text-editor-lines ed) offset)
      )

    (define (text-editor-line-editor-ref ed offset)
      ;; Used to get the character on the current line. Checks if the
      ;; line has been edited first, then decides whether to lookup
      ;; the character from the line buffer or from the text line
      ;; under the cursor.
      ;;--------------------------------------------------------------
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
          (integer->char (gap-buffer-ref (text-editor-line-editor ed) offset))
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
            (cdf      (text-editor-cdf ed))
            (cdf-cur  (cdf-cursor cdf))
            (offset
             (cond
              ((< cdf-cur line-num)
               (cdf-fill
                cdf (text-editor-make-cdf-fill-range lines line-num)
                ))
              ((< 0 line-num)
               (cdf-ref cdf (- line-num 1))
               )
              (else 0)
              )))
        (+ offset (or (and line-ed (gap-buffer-cursor line-ed)) 0))
        ))

    (define (text-editor-move-cursor ed move-by)
      (let*-values
          (((lines) (text-editor-lines ed))
           ((ch-index) (text-editor-get-cursor ed))
           ((line-num-before _old-offset)
            (text-editor-index-line-offset ed ch-index)
            )
           ((cdf-cur offset)
            (text-editor-index-line-offset ed (+ ch-index move-by))
            )
           ;; First set the cursor position according to the value
           ;; computed from the CDF.
           (() (gap-buffer-set-cursor lines cdf-cur))
           ((line-num-after)  (gap-buffer-cursor lines))
           )
        ;; Then make a note that the text editor line changed and
        ;; needs to be reset.
        (unless (= line-num-before line-num-after)
          (set!text-editor-line-moved ed #t)
          (set!text-editor-column ed offset)
          (gap-buffer-clear (text-editor-line-editor ed))
          )))

    (define text-editor-set-cursor
      (case-lambda
       ((ed index)
        (cond
         ((text-location-type? index)
          (text-editor-set-cursor
           ed (text-location-line index)
                 (text-location-column index)
              ))
         ((integer? index)
          (let ((cursor (text-editor-get-cursor ed)))
            (text-editor-move-cursor ed (- index cursor))
            ))
         (else
          (error
           "text editor index must be set with integer or text-location-type"
           index
           ))))
       ((ed line-num column-num)
        (let ((lines (text-editor-lines ed)))
          (gap-buffer-set-cursor lines line-num)
          (set!text-editor-line-moved ed #t)
          (gap-buffer-clear (text-editor-line-editor ed))
          (set!text-editor-column ed column-num)
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
         ((< ch-index cdf-max) (cdf-find cdf ch-index))
         (else
          (cdf-fill cdf (text-editor-make-cdf-fill-until lines ch-index))
          (let((cdf-cur (cdf-cursor cdf)))
            (cond
             ((<= cdf-cur 0) (values 0 0))
             (else
              (let*((cdf-cur (- cdf-cur 1))
                    (offset (cdf-ref cdf cdf-cur))
                    )
                (values cdf-cur (+ 1 (- ch-index offset)))
                ))))))))

    (define (text-editor-get-char-index ed ch-index)
      ;; Get the character at the given index `CH-INDEX`. This
      ;; recomputes part of the CDF for the editor buffer.
      ;;--------------------------------------------------------------
      (let*-values
          (((cdf-cur offset) (text-editor-index-line-offset ed ch-index))
           ((line-offset) (- ch-index offset))
           ((lines) (text-editor-lines ed))
           )
        (cond
         ((= cdf-cur (gap-buffer-cursor lines))
          (integer->char
           (gap-buffer-ref (text-editor-line-editor ed) line-offset)
           ))
         (else
          (let ((line (gap-buffer-ref lines cdf-cur)))
            (text-line-ref line line-offset)
            )))))

    (define (%text-editor-get-line-column ed ch-index)
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
         (+ 1 (text-editor-cursor-line ed))
         (+ 1 (text-editor-cursor-column ed))
         ))))

    (define text-editor-get-line-column
      (case-lambda
       ((ed) (%text-editor-get-line-column ed #f))
       ((ed ch-index) (%text-editor-get-line-column ed ch-index))
       ))

    (define (text-editor-get-end-of-line ed)
      (text-editor-get-cursor ed)
      (let*((lines (text-editor-lines ed))
            (line-num (gap-buffer-cursor lines))
            (cdf (text-editor-cdf ed))
            )
        (cond
         ;; First check if the current line editor contains recently
         ;; added characters. If not, we need the CDF for the current
         ;; line cursor.
         ((text-editor-line-moved ed)
          (cond
           ((< 0 line-num) (cdf-ref cdf (- line-num 1)))
           (else 0)
           ))
         ;; Otherwise we need the CDF for the previous line, and then
         ;; add the number of characters in the current line editor.
         (else
          (+ (cond
              ((< 1 line-num) (cdf-ref cdf (- line-num 2)))
              (else 0)
              )
             (gap-buffer-weight (text-editor-line-editor ed))
             )))))

    (define (text-editor-get-start-of-line ed)
      (text-editor-get-cursor ed)
      (let*((lines (text-editor-lines ed))
            (line-num (gap-buffer-cursor lines))
            (cdf (text-editor-cdf ed))
            )
        (cond
         ;; The start of line is always the value of the CDF index of
         ;; the line before the line cursor.
         ((< 1 line-num) (cdf-ref cdf (- line-num 2)))
         ;; If the cursor is at the beginning of the buffer, the
         ;; start-of-line is always zero.
         (else 0)
         )))

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

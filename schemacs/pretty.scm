;; A pretty printer

(define-record-type <pp-line-type>
  ;; A line of text that is stored into a buffer filled by a pretty
  ;; printer.
  ;;------------------------------------------------------------------
  (make<pp-line-type> indent char str)
  pp-line-type?
  (indent pp-line-indent)
  ;; The indentation depth.
  ;;------------------------------------------------------------------
  (char   pp-line-indent-char)
  ;; The character to use for indentation.
  ;;------------------------------------------------------------------
  (str    pp-line-string)
  ;; The string for a single line of text in a <PP-LINE-BUFFER-TYPE>.
  ;;------------------------------------------------------------------
  )

(define *empty-line* (make<pp-line-type> 0 #f "\n"))

(define (empty-line? line)
  (or (eq? line *empty-line*)
      (and
       (= 0 (pp-line-indent line))
       (equal? "\n" (pp-line-string line)))))

(define pp-line
  ;; Construct a <PP-LINE-TYPE> data structure. If one argument is
  ;; given, it must be a string containing the text.
  ;;
  ;; If two arguments are given, they must be: 1. the indentation
  ;; level, and 2. the string of text.
  ;;
  ;; If 3 arguments are given, they must be: 1. indentation level,
  ;; 2. indentation fill character, 3. the string of text.
  (case-lambda
    ((txt) (make<pp-line-type> 0 #\space txt))
    ((i txt) (make<pp-line-type> i #\space txt))
    ((i c txt) (make<pp-line-type> i c txt))))


(define pp-display-line
  ;; Write a <PP-LINE-TYPE> data structure to a given port, or if no
  ;; port is given to the port returned by the Scheme
  ;; `CURRENT-OUTPUT-PORT` procedure.
  ;;------------------------------------------------------------------
  (case-lambda
    ((line) (pp-display-line line (current-output-port)))
    ((line port)
     (let ((indent (pp-line-indent line))
           (char (pp-line-indent-char line))
           )
       (let loop ((i 0))
         (cond
          ((>= i indent) (values))
          (else
           (write-char char port)
           (loop (+ 1 i)))))
       (display (pp-line-string line) port)))))


;; -------------------------------------------------------------------------------------------------

(define-record-type <pp-line-buffer-stats-type>
  (make<pp-line-buffer-stats-type> maxlen minlen numzeros charcount minindent maxindent)
  pp-line-buffer-stats-type?
  (maxlen    pp-line-buffer-max-len     set!pp-line-buffer-max-len)
  ;; The maximum length of an unbroken line of text in this buffer.
  ;;------------------------------------------------------------------
  (minlen    pp-line-buffer-min-len     set!pp-line-buffer-min-len)
  ;; The minimum non-zero length of a line of text in this buffer.
  ;;------------------------------------------------------------------
  (numzeros  pp-line-buffer-zero-count  set!pp-line-buffer-zero-count)
  ;; The number of zero-length lines of text in this buffer.
  ;;------------------------------------------------------------------
  (charcount pp-line-buffer-char-count  set!pp-line-buffer-char-count)
  ;; The number of characters written into this buffer.
  ;;------------------------------------------------------------------
  (minindent pp-line-buffer-min-indent  set!pp-line-buffer-min-indent)
  ;; The minimum indentation level.
  ;;------------------------------------------------------------------
  (maxindent pp-line-buffer-max-indent  set!pp-line-buffer-max-indent)
  ;; The maximum indentation level.
  ;;------------------------------------------------------------------
  )

(define new-line-buffer-stats
  ;; This constructor can take 0, 1, or 6 arguments:
  ;;
  ;;  0. means construct a new statistics counter with all values set
  ;;     to zero
  ;;
  ;;  1. pass another <pp-line-buffer-stats> to create a copy of it
  ;;
  ;;  6. fabricate your own statistics, pass all 6 values yourself to
  ;;     set the fields: max-len, min-len, zero-count, char-count,
  ;;     min-indent, max-indent.
  (case-lambda
    (() (make<pp-line-buffer-stats-type> 0 0 0 0 #f #f))
    ((maxlen minlen numzeros charcount minindent maxindent)
     (make<pp-line-buffer-stats-type>
      maxlen minlen numzeros charcount minindent maxindent))
    ((stats)
      (make<pp-line-buffer-stats-type>
       (pp-line-buffer-max-len stats)
       (pp-line-buffer-min-len stats)
       (pp-line-buffer-zero-count stats)
       (pp-line-buffer-char-count stats)
       (pp-line-buffer-min-indent stats)
       (pp-line-buffer-max-indent stats)))
    ))

(define =>pp-line-buffer-max-len
  (record-unit-lens
   pp-line-buffer-max-len
   set!pp-line-buffer-max-len
   '=>pp-line-buffer-max-len))

(define =>pp-line-buffer-min-len
  (record-unit-lens
   pp-line-buffer-min-len
   set!pp-line-buffer-min-len
   '=>pp-line-buffer-min-len))

(define =>pp-line-buffer-zero-count
  (record-unit-lens
   pp-line-buffer-zero-count
   set!pp-line-buffer-zero-count
   '=>pp-line-buffer-zero-count))

(define =>pp-line-buffer-char-count
  (record-unit-lens
   pp-line-buffer-char-count
   set!pp-line-buffer-char-count
   '=>pp-line-buffer-char-count))

(define =>pp-line-buffer-min-indent
  (record-unit-lens
   pp-line-buffer-min-indent
   set!pp-line-buffer-min-indent
   '=>pp-line-buffer-min-indent))

(define =>pp-line-buffer-max-indent
  (record-unit-lens
   pp-line-buffer-max-indent
   set!pp-line-buffer-max-indent
   '=>pp-line-buffer-max-indent))

;; -------------------------------------------------------------------------------------------------

(define-record-type <pp-line-buffer-type>
  ;; A type of procedure that takes the result of a pretty printer
  ;; procedure and consumes it by iterating over the lines of text
  ;; produced.
  (make<pp-line-buffer-type> buffer stats commit-indent commit-char)
  pp-line-buffer-type?
  (buffer  pp-line-buffer)
  ;; The buffer itself, a `<MUTABLE-VECTOR-TYPE>`.
  ;;------------------------------------------------------------------
  (stats   pp-line-buffer-stats  set!pp-line-buffer-stats)
  ;; Statistics about the line buffer
  ;;------------------------------------------------------------------
  (commit-indent  pp-line-buffer-indent  set!pp-line-buffer-indent)
  ;; When text is first committed to a line, it must freeze the
  ;; indentation in place. Indentation can be changed after, but not
  ;; once text has been written to a line.
  ;;------------------------------------------------------------------
  (commit-char   pp-line-buffer-indent-char set!pp-line-buffer-indent-char)
  ;; Like `PP-LINE-BUFFER-INDENT` but for the indentation character.
  ;;------------------------------------------------------------------
  )

(define *default-line-buffer-init-size* (make-parameter 128))

(define new-line-buffer
  ;; Construct a new line buffer. Optionally pass an initial number of
  ;; lines to allocate for the buffer, or pass an already allocated
  ;; `<MUTABLE-VECTOR-TYPE>`. By default the initial line buffer size is
  ;; taken from the parameter object `*DEFAULT-LINE-BUFFER-INIT-SIZE*`.
  (case-lambda
    (() (new-line-buffer #f (*default-line-buffer-init-size*)))
    ((init)
     (cond
      ((integer? init) (new-line-buffer #f init))
      ((pp-line-buffer-type? init) (new-line-buffer #f init))
      ((boolean? init)
       (new-line-buffer init (*default-line-buffer-init-size*)))
      (else
       (error "argument must be either a boolean, an integer buffer size, or a line buffer"
        init))))
    ((init sink)
     (let ((stats
            (cond
             ((boolean? init) (if init (new-line-buffer-stats) #f))
             ((pp-line-buffer-stats-type? init) init)
             (else
              (error "first argument must be a boolean or a buffer stat record type"))
             ))
           (buffer
            (cond
             ((integer? sink) (new-mutable-vector sink))
             ((pp-line-buffer-type? sink) sink)
             (else (error "second argument must be an integer buffer size, or a line buffer" sink))))
           )
       (make<pp-line-buffer-type> buffer stats 0 #\space))))
  )


(define (pp-line-buffer-append! buffer line)
  ;; Append a line of text of type `<PP-LINE-TYPE>` (which may be
  ;; constructed by the `PP-LINE` procedure) to the given `BUFFER`
  ;; which must be of `<PP-LINE-BUFFER-TYPE>`. If the types do not
  ;; match an error condition is raised.
  ;;
  ;; This procedure should normally not be used, the pretty printer
  ;; API is designed to efficiently construct and fill line buffers
  ;; with text in a convenient, structured way. Using this procedure
  ;; is much less structured and less convenient.
  (mutable-vector-append! (pp-line-buffer buffer) line))


(define (pp-fold-lines fold init buffer)
  ;; Constructs a `<PP-CONSUMER-TYPE>` that can be used as the first
  ;; argument to `PRETTY-PRINT`. This consumer iterates a the
  ;; given procedure `FOLD` over every line produced by the pretty
  ;; printer. The `FOLD` procedure takes 3 arguments:
  ;;
  ;;  1. the current line number (0-based,
  ;;     so the first line is zero, not one)
  ;;
  ;;  2. the <PP-LINE-TYPE> object
  ;;
  ;;  3. the result of the previous call to `FOLD`, and
  ;;     `INIT` when `FOLD` is called the first time.
  ;;
  ;; The `FOLD` procedure must return a single value, typically of the
  ;; same type of data as `INIT`.
  (let-values
      (((len accum)
        (mutable-vector-fold/index
         fold init (pp-line-buffer buffer))))
    accum))

;;-------------------------------------------------------------------------------------------------

(define-record-type <pp-state-type>
  ;; Internal state of pretty printers, not exported except as an
  ;; opaque record type, constructed with `NEW-PRETTY-PRINTER`. All
  ;; `<PP-TYPE>` procedures take a value of this type as an argument.
  ;;------------------------------------------------------------------
  (make<pp-state>
   line-break first-indent write-line finalize
   line-buf out-port indent char do-indent?)
  pp-state-type?
  (line-break    pp-state-line-break)
  ;; The line break procedure stored here must be a closure, and must
  ;; defined to take zero one arguments. When given 1 argument, it
  ;; takes this state data structure itself (the state that contains
  ;; this procedure) as an argument and performs a line break. When
  ;; given 0 arguments, it is being "finalized" and returns the
  ;; accumulation of pretty printed text (if any) stored within the
  ;; closure.
  ;;------------------------------------------------------------------
  (first-indent  pp-state-first-indent)
  ;; The first-indent procedure is called when the pretty printer is
  ;; instructed to print text immediately after a line break and
  ;; indentation has not been committed to the output text yet.
  ;;------------------------------------------------------------------
  (write-line    pp-state-write-line)
  ;; This function is used to write a <PP-LINE-TYPE> of object to the
  ;; output. When writing to buffers, the given line is merely
  ;; appended. When writing to ports, the given line is rendered to a
  ;; string.
  ;;------------------------------------------------------------------
  (finalize      pp-state-finalize)
  ;; This function is called to flush the state and deallocate any
  ;; objects within it that might be taking up a lot of memory.
  ;;------------------------------------------------------------------
  (line-buf      pp-state-line-buffer     set!pp-state-line-buffer)
  ;; When writing to a buffer, this field holds a reference to the
  ;; buffer being written. When writing to a port, this is set to #f.
  ;;------------------------------------------------------------------
  (out-port      pp-state-output-port     set!pp-state-output-port)
  ;; When writing to a port, this field holds a reference to the port
  ;; to which output is being written.
  ;;------------------------------------------------------------------
  (indent        pp-state-indent          set!pp-state-indent)
  (char          pp-state-indent-char     set!pp-state-indent-char)
  (do-indent?    pp-state-start-of-line?  set!pp-state-start-of-line)
  ;; pp-state-start-of-line? is set to #t immediately after a line
  ;; break, if any string input is written, indentation is filled
  ;; first then pp-state-start-of-line? is set to #f.
  ;;------------------------------------------------------------------
  )


(define (pp-update-stats-line! stats line)
  (cond
   ((not stats) (values))
   (else
    (let*((str    (pp-line-string line))
          (strlen (string-length str))
          (indent (pp-line-indent line))
          (update-indent
           (lambda (compare)
             (lambda (count)
               (if (not count) indent (compare indent count)))))
          )
      (update (lambda (len) (max len (string-length str)))
       stats =>pp-line-buffer-max-len)
      (cond
       ((empty-line? line)
        (update (lambda (count) (+ 1 count)) stats =>pp-line-buffer-zero-count))
       (else
        (update (lambda (len) (if (> len 0) (min len strlen) strlen))
         stats =>pp-line-buffer-min-len)))
      (update (lambda (count) (+ count strlen)) stats =>pp-line-buffer-char-count)
      (update (update-indent min) stats =>pp-line-buffer-min-indent)
      (update (update-indent max) stats =>pp-line-buffer-max-indent)
      ))))


(define (pp-line-buffer-recompute-stats! line-buffer)
  ;; Run statistics calculations on all lines in the given
  ;; `LINE-BUFFER`, update the `LINE-BUFFER` in-place, replacing the
  ;; previous statistics. The `<PP-LINE-BUFFER-STATS-TYPE>` is returned
  (let ((stats (new-line-buffer-stats)))
    (pp-fold-lines
     (lambda (_ line stats) (pp-update-stats-line! stats line))
     stats line-buffer)
    (set!pp-line-buffer-stats line-buffer stats)
    stats))


(define (%buffered-flush pp)
  (let*((line-buffer (pp-state-line-buffer pp))
        (port (pp-state-output-port pp))
        (str (if (output-port? port) (get-output-string port) "")))
    (cond
     ((or (not port) (pp-state-start-of-line? pp)) (values))
     ((equal? "" str) (values))
     (else
      (let*((indent (pp-line-buffer-indent line-buffer))
            (char (pp-line-buffer-indent-char line-buffer))
            (line (make<pp-line-type> indent char str))
            )
        ;; Now that we have the <PP-LINE-TYPE> structure constructed,
        ;; close the line buffer and delete it. A new one will be crated
        ;; on-demand by `BUFFERED-FIRST-INDENT`.  Update statistics if
        ;; necessary
        (close-port (pp-state-output-port pp))
        (set!pp-state-output-port pp #f)
        (pp-line-buffer-append! line-buffer line)
        (pp-update-stats-line! (pp-line-buffer-stats line-buffer) line)
        (values))))))


(define buffered-line-break
  ;; Procedure used to construct the line breaking procedure for
  ;; line-buffered pretty printers. This procedure takes the content
  ;; of line buffer, appends it to the line buffer with information
  ;; about its indentation depth, and then closes the output string
  ;; port, sets the `PP-STATE-OUTPUT-PORT` in the `PP` state to #f.
  (lambda (pp)
    (let ((port (pp-state-output-port pp)))
      (cond
       ((or (pp-state-start-of-line? pp) (not port))
        (buffered-write-line pp *empty-line*))
       (else
        (newline port)
        (%buffered-flush pp))))))


(define buffered-finalize
  (lambda (pp)
    (let ((line-buffer (pp-state-line-buffer pp)))
      (%buffered-flush pp)
      line-buffer)))


(define buffered-first-indent
  ;; Line-buffered pretty printers do not print indentation, they only
  ;; record the indentation depth and indentation character of each
  ;; line. However each line does need to be buffered, and since the
  ;; `BUFFERED-LINE-BREAK` procedure closes its line buffer at the end
  ;; of each line, a new line buffer needs to be created here.
  (lambda (pp)
    (let ((line-buffer (pp-state-line-buffer pp)))
      (unless (pp-state-output-port pp)
        (set!pp-state-output-port pp (open-output-string)))
      (set!pp-line-buffer-indent line-buffer (pp-state-indent pp))
      (set!pp-line-buffer-indent-char line-buffer (pp-state-indent-char pp)))
    ))


(define buffered-write-line
  ;; This function is used to write a <PP-LINE-TYPE> of object to the
  ;; output. When writing to buffers, the given line is merely
  ;; appended.
  ;;------------------------------------------------------------------
  (lambda (pp line)
    (let*((line-buffer (pp-state-line-buffer pp))
          (port  (pp-state-output-port pp))
          (stats (pp-line-buffer-stats line-buffer))
          )
      (cond
       ((not port)
        (pp-line-buffer-append! line-buffer line)
        (pp-update-stats-line! (pp-line-buffer-stats line-buffer) line))
       (else
        (pp-display-line line port)
        (when stats
          (update
           (lambda count
             (apply + (pp-line-indent line) (string-length (pp-line-string line)) count))
           stats =>pp-line-buffer-char-count)))
       ))
    ))


(define (finalize-output-string pp)
  (let*((port (pp-state-output-port pp))
        (str (get-output-string port)))
    (close-port port)
    (set!pp-state-output-port pp #f)
    str))


(define (dont-finalize-port pp) (values))


(define (immediate-line-break pp)
  ;; Procedure used to construct the line breaking procedure for an
  ;; output port pretty printer. If the output is to a string buffer,
  ;; pass the string buffer as an argument to this function, the
  ;; content of the buffer will be retrieved and returned during finalization.
  (newline (pp-state-output-port pp)))


(define (immediate-first-indent pp)
  (let ((port   (pp-state-output-port pp))
        (indent (pp-state-indent pp))
        (char   (pp-state-indent-char pp)))
    (let loop ((i 0))
      (if (>= i indent)
          (values)
          (begin
            (write-char char port)
            (loop (+ 1 i)))))))


(define (immediate-write-line pp line)
  ;; This function is used to write a <PP-LINE-TYPE> of object to the
  ;; output. When writing to ports, the given line is rendered to a
  ;; string.
  ;;------------------------------------------------------------------
  (pp-display-line line (pp-state-output-port pp)))


(define (%line-break pp)
  ;; Run the line breaking function currently installed in the state.
  ((pp-state-line-break pp) pp)
  (set!pp-state-start-of-line pp #t))

(define (%check-indent pp)
  ;; Check if we are at the start of the line, and if so, indent.
  (cond
   ((pp-state-start-of-line? pp)
    ((pp-state-first-indent pp) pp)
    (set!pp-state-start-of-line pp #f)
    )
   (else (values))))

(define (%write-line pp line)
  ((pp-state-write-line pp) pp line))

(define (%finalize pp)
  ((pp-state-finalize pp) pp))

(define (%run-pp pp pp-proc)
  ((pretty-printer-procedure pp-proc) pp))


(define print-to-buffer
  ;; Construct a pretty printer state that captures lines of printed
  ;; output and stores them in a line buffer, then returns the buffer.
  ;; Must pass a boolean indicating whether you want the buffer to
  ;; collect statistics on characters and lines written. Optionally
  ;; pass as the second argument a buffer or a buffer size indicating
  ;; the desired initial number of lines to allocate for buffering
  ;; (which can grow if necessary).
  (case-lambda
    ((stats) (print-to-buffer stats 128))
    ((stats init)
     (let ((buffer
            (cond
             ((integer? init)
              (new-line-buffer stats init))
             ((pp-line-buffer-type? init) init)
             (else (error "takes a buffer or size as an argument, instead got" init))))
           )
       (make<pp-state>
        buffered-line-break
        buffered-first-indent
        buffered-write-line
        buffered-finalize
        buffer #f 0 #\space #t)))))


(define print-to-port
  ;; Construct a pretty printer state that prints directly to a
  ;; port. The port may also be #t or #f in which case the semantics
  ;; is the same as SRFI-48 `FORMAT` procedure, where passing #t means
  ;; to print to the port returned by the standard Scheme procedure
  ;; `CURRENT-OUTPUT-PORT`, and passing #f means to print to a string
  ;; and return the string.
  (case-lambda
    (() (print-to-port #t))
    ((sink)
     (let-values
         (((port finalize)
           (cond
            ((eq? #f sink) (values (open-output-string) finalize-output-string))
            ((eq? #t sink) (values (current-output-port) dont-finalize-port))
            ((port? sink)  (values sink dont-finalize-port))
            ((output-port? sink) (values sink dont-finalize-port))
            (else (error "takes a port (or #t or #f) as an argument, instead got" sink))))
          )
       (make<pp-state>
        immediate-line-break
        immediate-first-indent
        immediate-write-line
        finalize
        #f port 0 #\space #t)))))

;; -------------------------------------------------------------------------------------------------

(define-record-type <pp-quoted-type>
  ;; Used to wrap-up a string so that it is printed in its readable
  ;; form (using the Scheme `WRITE` procedure, rather than the
  ;; `DISPLAY` procedure). Objects printed by `PRINT` use the Scheme
  ;; `WRITE` procedure by default, literal strings are printed with
  ;; `DISPLAY`.
  (%quoted str)
  pp-quoted-type?
  (str pp-unquote))

(define (qstr arg)
  ;; Wrap an argument in a <LITERAL-TYPE>, which has slightly
  ;; different semantics when pretty printing, namely that wrapped
  ;; strings and characters are printed to the outut port using
  ;; Scheme `DISPLAY` rather than `WRITE.`
  (cond
   ((pp-quoted-type? arg) (%quoted (pp-unquote arg))) 
  (else (%quoted arg))))

;; -------------------------------------------------------------------------------------------------

(define-record-type <pp-type>
  ;; A type of pretty printing function constructed by one of the
  ;; constructors below. Run a function of this type using
  ;; `PRETTY-PRINT`.
  (make<pp> proc)
  pp-type?
  (proc pretty-printer-procedure)
  )


(define (%print pp arg)
  ;; Construct a closure that outputs the argument. Has a recursion
  ;; counter to prevent infinite recursion, pass 0 as the second
  ;; argument when calling this procedure.
  (cond
   ((string?  arg)
    (cond
     ((equal? "" arg) #f)
     (else
      (lambda ()
        (let ((strlen (string-length arg))
              (do-check #t)
              )
          (string-for-each
           (lambda (c)
             (cond
              ((char=? c #\newline)
               (%line-break pp)
               (set! do-check #t))
              (else
               (when do-check
                 (%check-indent pp)
                 (set! do-check #f))
               (write-char c (pp-state-output-port pp)))))
           arg))))))
   ((number? arg)
    (lambda ()
      (%check-indent pp)
      (display (number->string arg) (pp-state-output-port pp))))
   ((pp-quoted-type? arg)
    (lambda ()
      (%check-indent pp)
      (write (pp-unquote arg) (pp-state-output-port pp))))
   ((char?    arg)
    (lambda ()
      (%check-indent pp)
      (write-char arg (pp-state-output-port pp))
      (when (char=? arg #\newline)
        (%line-break pp))))
   ((not      arg) #f)
   ((pp-type? arg) (lambda () (%run-pp pp arg)))
   ((pp-line-type? arg) (%write-line pp arg))
   ((pp-line-buffer-type? arg)
    (pp-fold-lines
     (lambda (_i line _accum) (%write-line pp line))
     #f arg))
   (else
    (lambda ()
      (%check-indent pp)
      (write arg (pp-state-output-port pp))))))


(define (print . args)
  ;; Pretty-print each argument.
  ;;
  ;;  - Strings are printed using `DISPLAY` semantics
  ;;
  ;;  - #f values are ignored
  ;;
  ;;  - values wrapped in `QSTR` are printed with `WRITE` semantics
  ;;    (#f values are not ignored)
  ;;
  ;;  - values of type <PP-TYPE> (constructed by this function, and
  ;;    others like `REPEAT`, `INDENT-BY`, `JOIN-BY` `JOIN-LINES`,
  ;;    `BRACKETED`) are applied to the current state as though by the
  ;;    `pretty` function.
  ;;
  ;;  - All other built-in data types are printed with `WRITE` semantics.
  ;;
  ;;  - Procedures passed as arguments to `PRINT` must take zero
  ;;    arguments and must return a value of any of the above types.
  ;;
  ;;------------------------------------------------------------------
  (make<pp>
   (lambda (pp)
     (let ((old-indent (pp-state-indent pp))
           (old-char   (pp-state-indent-char pp)))
       (let loop ((args args))
         (cond
          ((null? args) (values))
          (else
           (let ((proc (%print pp (car args))))
             (when proc (proc))
             (loop (cdr args))))))))))


(define (repeat count . args)
  ;; Repeatedly print `ARG` `COUNT` number of times.
  ;;------------------------------------------------------------------
  (make<pp>
   (lambda (pp)
     (cond
      ((or (<= count 0) (null? args)) (values))
      (else
       (let*((arg (car args))
             (proc
              (cond
               ((null? (cdr args))
                (cond
                 ((string? arg) (lambda () (display arg (pp-state-output-port pp))))
                 ((char?   arg) (lambda () (write-char arg (pp-state-output-port pp))))
                 (else (%print pp arg))))
               (else (lambda () (%run-pp pp (apply print args))))))
             )
         (when proc
           (%check-indent pp)
           (let loop ((i 0))
             (cond
              ((>= i count) (values))
              (else (proc) (loop (+ 1 i))))))))))))


(define (indent-by +indent . args)
  ;; Increase the indendataion level by `N` and pretty print each
  ;; argument in `ARGS`.
  ;;------------------------------------------------------------------
  (make<pp>
   (lambda (pp)
     (cond
      ((null? args) (values))
      (else
       (let ((old-indent (pp-state-indent pp))
             (old-char   (pp-state-indent-char pp)))
         (set!pp-state-indent pp (+ old-indent +indent))
         (%run-pp pp (apply print args))
         (set!pp-state-indent pp old-indent)
         (set!pp-state-indent-char pp old-char)))))))


(define (newline-indent)
  ;; Write a newline to the buffer, then indent using the current
  ;; indentation level set by `INDENT-BY`.
  ;;------------------------------------------------------------------
  (make<pp> %line-break))


(define line-break newline-indent)


(define force-write-indent
  ;; Force write the current indentation to the current line. Takes
  ;; zero, one, or two arguments. By default, inserts spaces at the
  ;; current indentation level. You can also pass a single character
  ;; as an argument, or you can pass an alternative indentation number
  ;; (defaulting to spaced). You can also pass both a character and an
  ;; indentation level.
  ;;------------------------------------------------------------------
  (case-lambda
    (()
     (make<pp>
      (lambda (pp)
        (%run-pp pp
         (force-write-indent (pp-state-indent pp) (pp-state-indent-char pp))))))
    ((n)
     (make<pp>
      (lambda (pp)
        (cond
         ((char?    n) (force-write-indent (pp-state-indent pp) n))
         ((integer? n) (force-write-indent n (pp-state-indent-char pp)))
         (else (error "write-indent takes an integer, a char or both" n))))))
    ((indent char)
     (make<pp>
      (lambda (pp)
        (%check-indent pp)
        (let loop ((i 0))
          (cond
           ((>= i indent) (values))
           (else
            (write-char char (pp-state-output-port pp))
            (loop (+ 1 i))))))))))


(define (bracketed +indent open close . args)
  ;; Print every argument in `ARGS` in between running the `OPEN` and
  ;; `CLOSE` procedure, i.e. first `OPEN` runs, then every argument in
  ;; `ARGS` runs in order, then `CLOSE` runs.
  ;;------------------------------------------------------------------
  (make<pp>
   (lambda (pp)
     (%run-pp pp (print open))
     (%run-pp pp (indent-by +indent (apply print args) (print close))))))


(define (form +indent . args)
  ;; Calls `BRACKETED` with round brackets
  ;;------------------------------------------------------------------
  (bracketed +indent #\( #\) (apply join-by #\space args)))


(define (join-by joint . args)
  ;; Print each argument in `ARGS`, but in between each argument
  ;; evaluate the `JOINT` procedure.
  (make<pp>
   (lambda (pp)
     (cond
      ((null? args) (values))
      (else
       (%run-pp pp (print (car args)))
       (let loop ((args (cdr args)))
         (cond
          ((null? args) (values))
          (else
           (%run-pp pp (print joint (car args)))
           (loop (cdr args))))))))))


(define (join-lines . args)
  ;; Print each argument in `ARGS` delimited by a line break.
  (apply join-by (newline-indent) args))

;; -------------------------------------------------------------------------------------------------

(define pretty
  (case-lambda
    ((proc) (pretty #t proc))
    ((sink proc)
     (let ((pp
            (cond
             ((pp-state-type? sink) sink)
             ((output-port? sink) (print-to-port sink))
             ((pp-line-buffer-type? sink) (print-to-buffer #f sink))
             ((pp-line-buffer-type? sink) (print-to-buffer #f sink))
             ((integer? sink) (print-to-buffer #f sink))
             ((eq? #t sink) (print-to-port #t))
             ((eq? #f sink) (print-to-port #f))
             (else "incorrect type for first argument" sink)))
           )
       (%run-pp pp proc)
       (%finalize pp)))))

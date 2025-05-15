
(define-record-type <lexer-monad-type>
  (make<lexer-monad> lex)
  lexer-monad-type?
  (lex lexer-monad-procedure)
  )

(define (%take-char=? lexer)
  (take (lambda (c) (if (and (char? c) (char=? c lexer)) c #f))))

(define (%run-lexer st lexer)
  (cond
   ((eq? #f lexer) #f)
   ((eq? #t lexer) (%run-lexer st (any)))
   ((lexer-monad-type? lexer)
    ((lexer-monad-procedure lexer) st))
   ((char? lexer)
    (%run-lexer st (%take-char=? lexer)))
   ((procedure? lexer)
    (%run-lexer st
     (take (lambda (c) (if (char? c) (lexer c) #f)))))
   ((char-set? lexer)
    (%run-lexer st
     (take (lambda (c) (if (and (char? c) (char-set-contains? lexer c)) c #f)))))
   ((string? lexer)
    (%run-lexer st
     (apply lex (map %take-char=? (string->list lexer)))))
   ((parse-table-type? lexer)
    (%run-lexer st (lex-table lexer)))
   (else
    (error "not a lexer" lexer))
   ))

(define (run-lexer st lexer)
  ;; This is the monad evaluator, it is generally not used unless the
  ;; `LEXER` argument applied must return multiple `VALUES`. It is
  ;; usually better to evaluate lexer monads using `LEX-ALL`
  ;; instead.
  ;;
  ;; This procedure must be applied exactly 2 arguments:
  ;;
  ;;  1. a lexer state, or an argument that can be applied to
  ;;     `LEXER-STATE` to construct a lexer state
  ;;
  ;;  2. a lexer monad to be evaluated with the above statea.
  ;;------------------------------------------------------------------
  (%run-lexer (lexer-state st) lexer)
  )

;;--------------------------------------------------------------------------------------------------

(define-record-type <lexer-error-type>
  (make<lexer-error> location message irritants)
  lexer-error-type?
  (location   lexer-error-location   set!lexer-error-location)
  (message    lexer-error-message    set!lexer-error-message)
  (irritants  lexer-error-irritants  set!lexer-error-irritants)
  )

(define (new-lexer-error message . irritants)
  ;; Unlike `LEX-ERROR` which is a monad, this procedure need not be
  ;; applied to a monadic evaluator. It constructs a
  ;; `<LEXER-ERROR-TYPE>` and returns it as-is. But without access to
  ;; the monadic context, the error it constructs has not positional
  ;; information. The location information can be updated with the
  ;; `=>LEXER-ERROR-LOCATION*!` lens.
  ;;------------------------------------------------------------------
  (make<lexer-error> #f message irritants)
  )

(define =>lexer-error-location*!
  (record-unit-lens
   lexer-error-location
   set!lexer-error-location
   '=>lexer-error-location*!))

(define =>lexer-error-message*!
  (record-unit-lens
   lexer-error-message
   set!lexer-error-message
   '=>lexer-error-message*!))

(define =>lexer-error-irritants*!
  (record-unit-lens
   lexer-error-irritants
   set!lexer-error-irritants
   '=>lexer-error-message*!))

;;--------------------------------------------------------------------------------------------------

(define-record-type <lexer-state-type>
  (make<lexer-state> line column buffer ahead1 path port cont)
  lexer-state-type?
  (line    lexer-line         set!lexer-line)
  (column  lexer-column       set!lexer-column)
  (buffer  lexer-buffer       set!lexer-buffer)
  (ahead1  lexer-look-ahead   set!lexer-look-ahead)
  (path    lexer-filepath     set!lexer-filepath)
  (port    lexer-port         set!lexer-port)
  (cont    lexer-continue     set!lexer-continue)
  )

(define =>lexer-line*!
  (record-unit-lens
   lexer-line
   set!lexer-line
   '=>lexer-line*!))

(define =>lexer-column*!
  (record-unit-lens
   lexer-column
   set!lexer-column
   '=>lexer-column*!))

(define =>lexer-buffer*!
  (record-unit-lens
   lexer-buffer
   set!lexer-buffer
   '=>lexer-buffer*!))

(define =>lexer-look-ahead*!
  (record-unit-lens
   lexer-look-ahead
   set!lexer-look-ahead
   '=>lexer-look-ahead*!))

(define =>lexer-filepath*!
  (record-unit-lens
   lexer-filepath
   set!lexer-filepath
   '=>lexer-filepath*!))

(define =>lexer-port*!
  (record-unit-lens
   lexer-port
   set!lexer-port
   '=>lexer-port*!))

(define =>lexer-continuation*!
  ;; If this continuation field is set with a procedure constructed by
  ;; `CALL/CC`, the continuation procedure is applied when the current
  ;; input port `=>LEXER-PORT*!` returns an EOF object. The
  ;; continuation procedure is applied three arguments:
  ;;
  ;;  1. the current lexer state
  ;;
  ;;  2. `#F`, which is otherwise the return value of the lexer if
  ;;     the continuation is never called.
  ;;
  ;;  3. a continuation to call that will resume lexing.
  ;;
  ;; If you want to resume parsing, open a new input port and `UPDATE`
  ;; the current lexer state's `=>LEXER-PORT*!` field to contain this
  ;; input port, then apply the updated lexer state to the
  ;; continuation to resume parsing. If you do not update the lexer
  ;; port and then pass the lexer state to the resume parsing
  ;; continuation this same `=>LEXER-CONTINUATION*!` will be applied
  ;; again.
  ;;
  ;; If there is no more input, you update the lexer state
  ;; `=>LEXER-PORT*!` to `#F` and apply the updated lexer state to the
  ;; resume parsing continuation, tokenizing will continue such that
  ;; any calls to `LEX-EOF?` will return `#T`.
  (record-unit-lens
   lexer-continue
   set!lexer-continue
   '=>lexer-continuation*!))

;;--------------------------------------------------------------------------------------------------

(define-record-type <source-file-location-type>
  (make<source-file-location> path line column)
  source-file-location-type?
  (path   source-file-path)
  (line   source-file-line)
  (column source-file-column)
  )

(define (lexer-state-get-location st)
  (make<source-file-location>
   (lexer-filepath st)
   (lexer-line st)
   (lexer-column st)
   ))

(define write-lexer-location
  ;; Write to an output port (defaulting to `CURRENT-OUTPUT-PORT`) the given location.
  ;;
  ;;  - If 1 arguments applied, must be an object satisfing the
  ;;  `SOURCE-FILE-LOCATION-TYPE?` predicate.
  ;;
  ;;  - If 2 arguments applied, the first argument must be an object
  ;;    satisfying the `SOURCE-FILE-LOCATION-TYPE?` predicate, the
  ;;    second must be a port satisfying the `OUTPUT-PORT?` predicate,
  ;;    the output will be written to this port.
  ;;
  ;;  - If 3 arguments applied, the first three arguments must be the
  ;;    file path, line number, and column number, the same
  ;;    information contained within a `SOURCE-FILE-LOCATION-TYPE?`
  ;;    object.
  ;;
  ;;  - If 4 arguments applied, the first three are the file path,
  ;;    line number, column number (same as above), and the fourth
  ;;    argument must b ea port satisfying the `OUTPUT-PORT?`
  ;;    predicate, the output will be written to this port.
  ;;------------------------------------------------------------------
  (case-lambda
    ((location) (write-lexer-location location (current-output-port)))
    ((location out-port)
     (cond
      ((source-file-location-type? location)
       (write-lexer-location
        (source-file-path location)
        (source-file-line location)
        (source-file-column location)
        out-port
        ))
      ((lexer-state-type? location)
       (write-lexer-location
        (lexer-filepath location)
        (lexer-line location)
        (lexer-column location)
        out-port
        ))
      (else
       (error "not a source-file-location-type or lexer-state-type" location)
       )))
    ((path line column)
     (write-lexer-location path line column (current-output-port))
     )
    ((path line column out-port)
     (when path
       (display path out-port)
       (write-char #\: out-port)
       )
     (write line out-port)
     (write-char #\: out-port)
     (write column out-port)
     )))

(define (with-lexer-location proc)
  ;; This is a curried procedure that takes a procedure `PROC` and
  ;; returns a new procedure that takes a lexer state and applies the
  ;; filepath, current line, and current column to the `PROC` to
  ;; return a value.
  ;;------------------------------------------------------------------
  (lambda (st)
    (proc (lexer-filepath st) (lexer-line st) (lexer-column st))
    ))

;;--------------------------------------------------------------------------------------------------

(define lexer-state
  ;; Construct a lexer state from a port or string as the first
  ;; argument. If the first argument is already a lexer state value,
  ;; the rest of the non-`#F` arguments applied will update the state
  ;; metadata, all of which are optional:
  ;;
  ;;  1. a string?, input-port?, or lexer-state-type?
  ;;     (defaults to `CURRENT-INPUT-PORT`)
  ;;
  ;;  2. the current line number
  ;;
  ;;  3. the current column number
  ;;
  ;;  4. the current filepath
  ;;
  ;;  5. a continuation to be applied when end of input has been
  ;;     reached
  ;;------------------------------------------------------------------
  (case-lambda
    (() (lexer-state (current-input-port)))
    ((port)
     (cond
      ((lexer-state-type? port) port)
      (else (lexer-state port 1))
      ))
    ((port line) (lexer-state port line 1))
    ((port line column) (lexer-state port line column #f))
    ((port line column filepath) (lexer-state port line column filepath #f))
    ((port line column filepath cont)
     (cond
      ((input-port? port)
       (cond
        ((input-port-open? port)
         (let ((st (make<lexer-state> line column #f #f filepath port cont)))
           (lens-set port st =>lexer-port*!)
           st))
        (else (error "cannot initialize lexer with closed input port" port))
        ))
      ((string? port)
       (lexer-state (open-input-string port) line column filepath cont)
       )
      ((lexer-state-type? port)
       (let ((put
              (lambda (new =>lens*!)
                (update (lambda (old) (or new old)) port =>lens*!)))
             )
         (put line     =>lexer-line*!)
         (put column   =>lexer-column*!)
         (put cont     =>lexer-continuation*!)
         (put filepath =>lexer-filepath*!)
         port
         ))
      ((eq? port #t) (lexer-state (current-input-port) line column filepath cont))
      (else (error "cannot initialize lexer with input value" port))
      ))))

(define (lexer-feed-string! lexst str)
  ;; Prepare the lexer to read a string. This does not reset the
  ;; line/column location counter.
  ;;------------------------------------------------------------------
  (lens-set (open-input-string str) lexst =>lexer-port*!))


(define (lexer-feed-file! lexst filename)
  ;; Prepare the lexer to read a file. This does not reset the
  ;; line/column location counter.
  ;;------------------------------------------------------------------
  (lens-set (open-input-file filename) lexst =>lexer-port*!))


(define set-location!
  ;; Set the current location to be recorded into tokens. This is a
  ;; monad and operates on the current monad state.
  ;;------------------------------------------------------------------
  (case-lambda
    (()     (set-location!    1 1))
    ((line) (set-location! line 1))
    ((line column)
     (make<lexer-monad>
      (lambda (st)
        (lens-set line   st =>lexer-line*!)
        (lens-set column st =>lexer-column*!)
        #t)))))


(define (%look st)
  (let ((c (lexer-look-ahead st)))
    (cond
     ((not c)
      (let ((c (read-char (lexer-port st))))
        (set!lexer-look-ahead st c)
        c))
     (else c)
     )))

(define (|1+| line) (+ 1 line))

(define (%increment-line st c)
  (cond
   ((char=? #\newline c)
    (update |1+| st =>lexer-line*!)
    (lens-set 1 st =>lexer-column*!)
    )
   (else
    (update |1+| st =>lexer-column*!)
    )))

(define (%step! st)
  (let ((c (lexer-look-ahead st)))
    (%increment-line st c)
    (set!lexer-look-ahead st (read-char (lexer-port st)))
    #t))


(define any
  ;; Return the current character (consuming it) and advance the
  ;; cursor.
  ;;
  ;; Apply an single optional argument to be used as the return value
  ;; of this monad, which will be returned after the cursor is
  ;; advanced, instead of the consumed character from the
  ;; input. Applying `#F` or no arguments will cause the next
  ;; character, whatever it might be, to be returned instead. Be
  ;; careful not to return a non-character inside of a `LEX/BUFFER`,
  ;; `MANY/BUFFER`, or `MANY1/BUFFER` lexer as these lexers buffer
  ;; characters and expect lexers to return characters.
  ;;------------------------------------------------------------------
  (case-lambda
    (() (any #f))
    ((return-val)
     (make<lexer-monad>
      (lambda (st)
        (let ((get-next (lambda () (read-char (lexer-port st))))
              (return (lambda (next) (if return-val return-val next)))
              )
          (let loop ((st st) (next (%look st)))
            (cond
             ((eof-object? next)
              (let ((wait-for-more (lexer-continue st)))
                (cond
                 (wait-for-more
                  (let ((st
                         (call/cc
                          (lambda (resume-with-updated-state)
                            (wait-for-more #f st resume-with-updated-state)
                            ))))
                    (cond
                     ((lexer-state-type? st)
                      (loop st (%look st)))
                     (else (return next))
                     )))
                 (else (return next))
                 )))
             ((char? next)
              (%step! st)
              (return next)
              )
             (else
              (error "unknown value returned by read-char" next)
              )))))))))


(define (lex-const return-val)
  ;; Return any value without advancing the cursor. This is useful as
  ;; the final lexer of a sequence constructed with `LEX`, it returns
  ;; the value instead of a character or `#T` on success. If any other
  ;; lexer runs after this one, the return value given to `LEX-CONST`
  ;; is simply ignored and the return value from the next lexer is
  ;; returned instead.
  ;;------------------------------------------------------------------
  (make<lexer-monad> (lambda (st) return-val)))


(define look
  ;; Get the current character under the cursor and return it, or if
  ;; the optional argument `STEP` is applied, pass the character to
  ;; the given procedure `STEP`. In any case, this monad will not
  ;; consume the character or advance the cursor.
  ;;------------------------------------------------------------------
  (case-lambda
    (() (make<lexer-monad> (lambda (st) (%look st))))
    ((step) (make<lexer-monad> (lambda (st) (step (%look st)))))))


(define (take pred)
  ;; Get the current character under the cursor and apply it to the
  ;; given procedure `PRED`. If `PRED` returns non-`#f` then consume
  ;; the character and advance the cursor, return the result that was
  ;; returned by `PRED`.
  ;;------------------------------------------------------------------
  (make<lexer-monad>
   (lambda (st)
     (let ((result (pred (%look st))))
       (cond
        (result (%step! st) result)
        (else #f))
       ))))


(define (char pred)
  ;; This monad is similar to `TAKE` except it returns the character
  ;; that matched. This is different from `TAKE` which will simply
  ;; apply the next character to that procedure then returns the
  ;; result of the procedure, halting the lexing if the procedure
  ;; returns `#F`. This means, for example, if you pass a predicate
  ;; procedure like `CHAR-UPPER-CASE?` to `TAKE` the value `#T` is
  ;; returned, and so is not accumulated by buffered functions like
  ;; `MANY1/BUFFER`. Conversely, the `CHAR` monad will apply a
  ;; predicate `PRED` and returns the current character under the
  ;; cursor if applying this character to the predicate returns
  ;; non-`#F` It is also possible to apply a character rather than a
  ;; predicate procedure as the `PRED` argument, in which case, the
  ;; procedure:
  ;;
  ;;     `(LAMBDA (CURSOR) (CHAR=? CURSOR PRED))`
  ;;
  ;; will be used as the predicate of this monad.
  ;;------------------------------------------------------------------
  (cond
   ((char? pred) (%take-char=? pred))
   ((procedure? pred)
    (make<lexer-monad>
     (lambda (st)
       (let*((next (%look st)))
         (cond
          ((and (char? next) (pred next)) (%step! st) next)
          (else #f)
          )))))
   (else
    (error
     "must apply a character, or a character predicate procedure"
     pred)
    )))


(define skip-to-next-line
  ;; Skip quickly to the next `#\NEWLINE` character. This operation is
  ;; common when parsing files with comments, and the algorithm in
  ;; this monad combinator is more efficient than using some other
  ;; monad such as "many" to accomplish the same goal because it
  ;; avoids counting characters. This procedure returns #t when the
  ;; cursor reaches the next `#\NEWLINE`, or returns an EOF object
  ;; if the end of input is reached.
  ;;
  ;; Optionally apply an integer indicating how many newline
  ;; characters you would like to skip past.
  ;;------------------------------------------------------------------
  (case-lambda
    (() (skip-to-next-line 1))
    ((nlines)
     (make<lexer-monad>
      (lambda (st)
        (let ((port (lexer-port st)))
          (let next-line ((n 0))
            (cond
             ((= n nlines) #t)
             (else
              (let loop ()
                (let ((c (read-char port)))
                  (cond
                   ((eof-object? c) c)
                   ((char=? c #\newline)
                    (update (lambda (line) (+ 1 line)) st =>lexer-line*!)
                    (lens-set 1 st =>lexer-column*!)
                    (next-line (+ 1 n))
                    )
                   (else (loop))
                   ))))))))))))


(define (lex-brackets open close inner)
  ;; Construct a lexer that evaluates a given `OPEN`, `INNER`, and
  ;; `CLOSE` lexer, and returns the value returned by `INNER`.
  ;;------------------------------------------------------------------
  (make<lexer-monad>
   (lambda (st)
     (let ((open (%run-lexer st open)))
       (cond
        ((or (not open) (lexer-error-type? open)) open)
        (open
         (let ((result (%run-lexer st inner)))
           (cond
            ((not result) (%run-lexer st (lex-error "invalid bracketed expression")))
            ((lexer-error-type? result) result)
            (else
             (let ((close (%run-lexer st close)))
               (cond
                ((lexer-error-type? close) close)
                ((not close)
                 (%run-lexer st (lex-error "expecting closing bracket")))
                (else result)
                ))))))
        (else #f)
        )))))

(define (lex-first first second . rest)
  ;; Evaluate a lexer monad `FIRST` and keep it's result, then
  ;; evaluate all other monads after `FIRST`. If all succeed, then
  ;; return the result from the `FIRST` evaluated monad.
  ;;------------------------------------------------------------------
  (make<lexer-monad>
   (lambda (st)
     (let ((result (%run-lexer st first)))
       (cond
        ((and result (apply lex second rest)) result)
        (else #f)
        )))))


(define (lex-apply proc . lexers)
  ;; Similar to monaidc bind, evaluates the `LEXER` monad, then passes
  ;; the returned value to the `WRAPPER` procedure, the value returned
  ;; by `WRAPPER` is the value returned by this monad.
  ;;------------------------------------------------------------------
  (make<lexer-monad> (lambda (st) (proc (%run-lexer st (apply lex lexers))))))


(define eof
  ;; Returns `#T` if the cursor has gone past the end of the character
  ;; stream. Supply one optional argument to this procedure which will
  ;; be returned instead of `#T`.
  ;;------------------------------------------------------------------
  (case-lambda
    (()
     (make<lexer-monad>
      (lambda (st) (eof-object? (%look st)))
      ))
    ((return)
     (make<lexer-monad>
      (lambda (st) (and (eof-object? (%look st)) return))
      ))))


(define (lex-error message . irritants)
  ;; This monad evaluates to an error but does not raise the error, it
  ;; returns an `<LEXER-ERROR>` value.
  ;;------------------------------------------------------------------
  (make<lexer-monad>
   (lambda (st)
     (make<lexer-error> (lexer-state-get-location st) message irritants)
     )))


(define (lex-raise message . irritants)
  ;; Raises an lexer exception.
  (make<lexer-monad>
   (lambda (st)
     (raise (%run-lexer st (apply lex-error message irritants))))))


(define lex-trace
  (case-lambda
    ((monad) (lex-trace monad #f))
    ((monad port)
     (make<lexer-monad>
      (lambda (st)
        (parameterize ((current-output-port (if port port (current-output-port))))
          (write-string ";; ") (write (lexer-line st))
          (write-char #\:) (write (lexer-column st)) (write-string ": ")
          (write (%look st)) (display " -> ") (write monad) (newline)
          (%run-lexer st monad)
          )))
     )))


(define location
  ;; Return the current location in the input, number of line breaks
  ;; counted since the start of lexing, and the number of characters
  ;; counted since the most recent line break. The value returned is a
  ;; `<SOURCE-FILE-LOCATION-TYPE>`
  ;;
  ;; This monad optionally can be applied a procedure `PROC` which,
  ;; when the monad is evaluated, will apply the current filepath,
  ;; current line, and current column of the lexer to `PROC` to return
  ;; a value instead.
  ;;------------------------------------------------------------------
  (case-lambda
    (() (make<lexer-monad> (lambda (st) (lexer-state-get-location st))))
    ((proc) (make<lexer-monad> (with-lexer-location proc)))))

;;--------------------------------------------------------------------------------------------------

(define (%lex-loop/buffer lexers on-success on-fail)
  (define (buffer-push buffer result)
    (cond
     ((not     result) #f)
     ((char?   result) (write-char result buffer) #t)
     ((string? result) (write-string result buffer) #t)
     (else #t)
     ))
  (define (run-lexers st buffer lexers)
    (let loop ((lexers lexers) (return #f))
      (cond
       ((null? lexers) (on-success loop return))
       (else
        (let*((next-lex (car lexers))
              (more (cdr lexers))
              (result (%run-lexer st next-lex))
              )
          (if (buffer-push buffer result)
              (loop more #t)
              (on-fail return)
              ))))))
  (cond
   ((null? lexers) #t)
   ((and (output-port? (car lexers)) (null? (cdr lexers))) #t)
   (else
    (make<lexer-monad>
     (lambda (st)
       (let ((buffer (lexer-buffer st))
             (new-buffer (car lexers))
             )
         (cond
          ((and
            (output-port? new-buffer)
            (output-port-open? new-buffer)
            )
           (set!lexer-buffer st new-buffer)
           (let*((return (run-lexers st new-buffer (cdr lexers)))
                 )
             (set!lexer-buffer st buffer)
             return
             ))
          ((output-port? buffer) (run-lexers st buffer lexers))
          (else
           ;; Allocate a new buffer, make sure it is closed when this
           ;; procedure completes.
           (let*((buffer (open-output-string))
                 (result
                  (let ()
                    (lens-set buffer st =>lexer-buffer*!)
                    (run-lexers st buffer lexers)
                    ))
                 (return (if result (get-output-string buffer) #f))
                 )
             (close-output-port buffer)
             (lens-set #f st =>lexer-buffer*!)
             return
             )))))))))


(define (%lex-loop lexers on-success on-fail)
  (make<lexer-monad>
   (lambda (st)
     (let loop ((lexers lexers) (return #f))
       (cond
        ((null? lexers) (on-success loop return))
        (else
         (let ((result (%run-lexer st (car lexers))))
           (if result (loop (cdr lexers) result) (on-fail return))
           )))))))


(define (lex/buffer . lexers)
  ;; Like `LEX`, except that each `LEXER` monad that returns a
  ;; character or string will push those character into a buffer. The
  ;; content of the buffer is returned as a string if this monad
  ;; succeeds.
  ;;
  ;; If the first argument to this procedure is of type `OUTPUT-PORT?`
  ;; then the given output port is used as the buffer, and it is not
  ;; closed when the lexeing procedure halts. Otherwise, a new buffer
  ;; is allocated and closed when the lexing procedure halts. If this
  ;; monad is evaluated within another buffering lexer monad, a new
  ;; buffer is NOT allocated, the existing buffer is used.
  ;;------------------------------------------------------------------
  (%lex-loop/buffer lexers (lambda (loop return) return) (lambda (return) return)))


(define (lex . lexers)
  ;; Construct a lexer monad from a sequence of other lexers. Each
  ;; lexer is tried in turn until one of the lexers fail. None of the
  ;; characters are ever buffered (for buffering use `LEX/BUFFER`). At
  ;; least one of the lexers must succed in order for this monad to
  ;; succeed.
  ;;------------------------------------------------------------------
  (%lex-loop lexers (lambda (loop return) return) (lambda (return) return)))


(define (either . lexers)
  (cond
   ((null? lexers) #f)
   (else
    (make<lexer-monad>
     (lambda (st)
       (let loop ((lexers lexers))
         (cond
          ((null? lexers) #f)
          (else
           (let*((next-lex (car lexers))
                 (result (%run-lexer st next-lex))
                 )
             (if result result (loop (cdr lexers)))
             )))))))))


(define (many/buffer . lexers)
  ;; Like `MANY`, except that whenever any of the `LEXERS` return a
  ;; character or string, the characters are pushed to a buffer. The
  ;; content of the buffer is returned as a string when the lexing
  ;; stops.
  ;;
  ;; If the first argument to this procedure is of type `OUTPUT-PORT?`
  ;; then the given output port is used as the buffer, and it is not
  ;; closed when the lexeing procedure halts. Otherwise, a new buffer
  ;; is allocated and closed when the lexing procedure halts. If this
  ;; monad is evaluated within another buffering lexer monad, a new
  ;; buffer is NOT allocated, the existing buffer is used.
  ;;------------------------------------------------------------------
  (%lex-loop/buffer lexers
   (lambda (loop return) (loop lexers return))
   (lambda (return) #t)))


(define (many . lexers)
  ;; Construct a lexer monad by looping over a sequence of
  ;; lexers. Looping continues until one of the lexers fails.  This
  ;; monad always succeeds
  ;;------------------------------------------------------------------
  (%lex-loop lexers
   (lambda (loop return) (loop lexers return))
   (lambda (return) #t)))


(define (many1/buffer . lexers)
  ;; Similar to `MANY/BUFFER`, except that at least the first of the
  ;; given `LEXERS` must succeed for this monad to succeed.
  ;;
  ;; If the first argument to this procedure is of type `OUTPUT-PORT?`
  ;; then the given output port is used as the buffer, and it is not
  ;; closed when the lexeing procedure halts. Otherwise, a new buffer
  ;; is allocated and closed when the lexing procedure halts. If this
  ;; monad is evaluated within another buffering lexer monad, a new
  ;; buffer is NOT allocated, the existing buffer is used.
  ;;------------------------------------------------------------------
  (%lex-loop/buffer lexers
   (lambda (loop return) (loop lexers return))
   (lambda (return) return)))


(define (many1 . lexers)
  ;; Similar to `MANY`, except that at least the first of the given
  ;; `LEXERS` must succeed for this monad to succeed.
  ;;------------------------------------------------------------------
  (%lex-loop lexers
   (lambda (loop return) (loop lexers return))
   (lambda (return) return)))


(define (lex-fold-count init-accum fold-proc final-proc . lexers-list)
  ;; Similar to `MANY1/BUFFER`, except every time a lexer in the lexer
  ;; list is applied and produces a non-`#F` value, this value is
  ;; passed to a given `FOLD-PROC` procedure to update an accumulator
  ;; value. The `FOLD-PROC` procedure takes three arguments:
  ;;
  ;;  1. the previous value returned by `FOLD-PROC`, or `INIT-ACCUM`
  ;;     on the first evaluation.
  ;;
  ;;  2. the number of values that have been folded so far, which may
  ;;     be useful for ensuring a minimum or maximum characters are
  ;;     lexed.
  ;;
  ;;  3. the value returned by the most recently applied lexer.
  ;;
  ;; `FOLD-PROC` must return two values:
  ;;
  ;;  1. a boolean indicating whether to continue looping,
  ;;  2. the accumulator value to be used for the next iteration.
  ;;
  ;; The first lexer to return `#F` will stop looping.  Because this
  ;; is a lexer monad, is must return 1 value only, however the place
  ;; where this monad is evaluated may need to know about two values:
  ;;
  ;;  1. the accumulator value,
  ;;  2. the number of characters accumulated.
  ;;
  ;; So before this monad finishes evaluation, those two values are
  ;; both applied to the `FINAL-PROC` procedure, which can then act on
  ;; both values, and return the actual character or `#F`.
  ;;------------------------------------------------------------------
  (cond
   ((null? lexers-list)
    (lex-const (final-proc init-accum 0))
    )
   (else
    (make<lexer-monad>
     (lambda (st)
       (let loop ((accum init-accum) (count 0) (lexers lexers-list))
         (cond
          ((null? lexers) (loop accum count lexers-list))
          (else
           (let*((result (%run-lexer st (car lexers))))
             (cond
              (result
               (let-values (((continue accum) (fold-proc accum count result)))
                 (if continue
                     (loop accum (+ 1 count) (cdr lexers))
                     (final-proc accum count))))
              (else (final-proc accum count))
              ))))))))))


(define (lex-fold init-accum fold-proc . lexers-list)
  ;; Similar to `LEX-FOLD-COUNT` except the `FOLD-PROC` does not take
  ;; the character count as the second argument. So the `FOLD-PROC`
  ;; procedure must take two arguments:
  ;;
  ;;  1. the previous value returned by `FOLD-PROC`, or `INIT-ACCUM`
  ;;     on the first evaluation.
  ;;
  ;;  2. the value returned by the most recently applied lexer.
  ;;
  ;; and `FOLD-PROC` must return two values:
  ;;
  ;;  1. a boolean indicating whether to continue looping,
  ;;  2. the accumulator value to be used for the next iteration.
  ;;
  ;; The first lexer to return `#F` will stop looping and return the
  ;; accumulator value. Finally, unlike `LEX-FOLD-COUNT`, since there
  ;; is no character counter, there is no `FINAL-PROC` needed to
  ;; handle it, so whatever the final call to `FOLD-PROC` returned
  ;; becomes the return value of this monad when it completes
  ;; evaluation.
  ;;------------------------------------------------------------------
  (cond
   ((null? lexers-list) (lex-const init-accum))
   (else
    (make<lexer-monad>
     (lambda (st)
       (let loop ((accum init-accum) (lexers lexers-list))
         (cond
          ((null? lexers) (loop accum lexers-list))
          (else
           (let*((result (%run-lexer st (car lexers))))
             (cond
              (result
               (let-values (((continue accum) (fold-proc accum result)))
                 (if continue (loop accum (cdr lexers)) accum)))
              (else accum)
              ))))))))))


(define (lex-fmap proc lexer)
  ;; This is the "Functor Map" operator of the lexer monad, so unlike
  ;; `map` which operates on the contents of the list, this functor
  ;; map applies `PROC` to the value returned by the `LEXER` monad
  ;; when `LEXER` evaluates. The name is inspired by the "Functor Map"
  ;; operator from Haskell.
  ;;------------------------------------------------------------------
  (make<lexer-monad> (lambda (st) (proc (%run-lexer st lexer)))))

;;--------------------------------------------------------------------------------------------------

(define (lex-all input . lexers)
  ;; Runs a sequence of `LEXERS` on some `INPUT` and does not wait for
  ;; more input when the input is exhausted. The `INPUT` can be any
  ;; of:
  ;;
  ;;  1. a string, in which case the string is fed into the lexer.
  ;; 
  ;;  2. ai input port, where the character read from the port are fed
  ;;     into the lexer.
  ;;
  ;;  3. a `<LEXER-STATE-TYPE>` constructed with the `LEXER-STATE`
  ;;     procedure, or a state returned from a previous call to
  ;;     `LEX-ALL`.
  ;;
  ;;  4. `#T` indicates that `STANDARD-INPUT-PORT` should be used.
  ;;
  ;; The `LEX-ALL` procedure returns 2 values:
  ;;
  ;;  1. the return value of the final lexer in `LEXERS`,
  ;;
  ;;  2. the `<LEXER-STATE-TYPE>`.
  ;;------------------------------------------------------------------
  (%run-lexer (lexer-state input) (apply lex lexers))
  )


(define (lex-part input . lexers)
  ;; Similar to `LEX-ALL`, however when the end of input is reached,
  ;; the lexer pauses and returns a continuation that allows you to
  ;; feed more input into the lexer state and resume lexing. This is
  ;; useful for constructing interactive REPLs where an end user may
  ;; type the parts of a form across multiple lines.
  ;;
  ;; Three values returned by this procedure:
  ;;
  ;;  1. The return value of the final lexer in `LEXERS`.
  ;;
  ;;  2. The `<LEXER-STATE-TYPE>` value.
  ;;
  ;;  3. A continuation function or `#F`. A non-`#F` value indicates
  ;;     that all input was consumed but the lexer did not run to
  ;;     completion.
  ;;------------------------------------------------------------------
  (call/cc
   (lambda (continue-after-eof)
     (let ((st (lexer-state input)))
       (lens-set continue-after-eof st =>lexer-continuation*!)
       (let ((result (%run-lexer st (apply lex lexers))))
         (values result st #f)
         )))))


(define (lex-resume input lexer-state continue)
  ;; If you evaluate `LEX-PART on some input` and the third value
  ;; returned is not `#F` but a `PROCEDURE?` type, this means
  ;; `LEX-PART` consumed all input but did not complete lexing. To
  ;; feed more input into the lexer and resume lexing, apply the
  ;; following arguments to this procedure:
  ;;
  ;;  1. As the `INPUT` argument apply a string or an input port. If
  ;;     you pass `#F` as the input, this indicates that the true EOF
  ;;     has been reached, and the tokenizer will react accordingly --
  ;;     calls to the `EOF` monad will return `#T`.
  ;;
  ;;  2. As the `LEXER-STATE` argument apply second value returned by
  ;;     `LEX-PART`, a value of type `<LEXER-STATE-TYPE>`.
  ;;
  ;;  3. As the `CONTINUE` argument apply the third value returned by
  ;;     `LEX-PART`, a continuation procedure that resumes lexing where
  ;;     it had stopped at the end of the previous part of the input.
  ;;
  ;; This procedure returns three values following the exact same
  ;; protocol as the `LEX-PART` procedure.
  ;;------------------------------------------------------------------
  (cond
   ((string? input)
    (lens-set (open-input-string input) lexer-state =>lexer-port*!)
    (continue lexer-state)
    )
   ((input-port? input)
    (cond
     ((input-port-open? input)
      (lens-set input lexer-state =>lexer-port*!)
      (continue lexer-state)
      )
     (else (values #f lexer-state continue))
     ))
   (else
    (error "cannot resume lexer with input value" input)
    )))


(define scan-for-string
  ;; This monad implements a Grep-like algorithm, but for fixed
  ;; strings rather than regular expressions. It efficiently scans
  ;; forward through the stream of characters from the lexer state
  ;; searching for a given string, updating the line and column counts
  ;; appropriately. You can limit the number of lines by applying a
  ;; positive integer as the first argument and the search string as
  ;; the second argument.
  ;; ------------------------------------------------------------------
  (case-lambda
    ((findstr) (scan-for-string #f findstr))
    ((line-limit findstr)
     (let ((full-len (string-length findstr)))
       (define (inc-line c line)
         (if (char=? c #\newline) (+ 1 line) line)
         )
       (define (inc-column c column)
         (if (char=? c #\newline) 1 (+ 1 column))
         )
       (define (inc-offset offset)
         (remainder (+ 1 offset) full-len)
         )
       (define (preload-str st)
         (let ((port (lexer-port st))
               (buffer (make-string full-len)))
           (let loop ((c (read-char port)) (i 0))
             (cond
              ((eof-object? c) #f)
              (else
               (cond
                ((char=? c #\newline)
                 (update (lambda (line) (+ 1 line)) st =>lexer-line*!)
                 (lens-set 1 st =>lexer-column*!)
                 )
                (else (update (lambda (column) (+ 1 column)) st =>lexer-column*!))
                )
               (string-set! buffer i c)
               (let ((i (+ 1 i)))
                 (cond
                  ((>= i full-len) buffer)
                  (else (loop (read-char port) i))
                  )))))))
       (define (no-match? offset buffer)
         ;; return #f if offset buffer string matches findstr,
         ;; otherwise return the number of characters to be skipped,
         ;; always a minimum of 1.
         (let loop ((x 0) (y offset))
           (cond
            ((< x full-len)
             (cond
              ((char=? (string-ref findstr x) (string-ref buffer y))
               (loop (+ 1 x) (inc-offset y))
               )
              (else (+ 1 x))
              ))
            (else #f)
            )))
       (define (skip-chars port buffer offset line column n)
         (let loop
             ((n n)
              (c (read-char port))
              (offset offset)
              (line line)
              (column column))
           (cond
            ((eof-object? c)
             (values c offset line column))
            (else
             (string-set! buffer offset c)
             (let*((n      (- n 1))
                   (line   (inc-line c line))
                   (column (inc-column c column))
                   (offset (inc-offset offset))
                   )
               (cond
                ((<= n 0)
                 (values c offset line column)
                 )
                (else (loop n (read-char port) offset line column))
                ))))))
       (define (scan st buffer buf-offset line column)
         (cond
          ((and line-limit (>= line line-limit)) #f)
          (else
           (let ((skip-count (no-match? buf-offset buffer)))
             (cond
              ((not skip-count) ;; success
               (update (lambda (old-linenum) (+ line old-linenum)) st =>lexer-line*!)
               (lens-set column st =>lexer-column*!)
               (set!lexer-look-ahead st #f)
               #t)
              (else
               (let-values
                   (((c buf-offset line column)
                     (skip-chars
                      (lexer-port st)
                      buffer buf-offset line column skip-count
                      )))
                 (cond
                  ((eof-object? c) #f)
                  (else
                   (scan st buffer buf-offset line column)
                   )))))))))
       (cond
        ((= 0 full-len) (lex-const #t))
        (else
         (make<lexer-monad>
          (lambda (st)
            (let ((buffer (preload-str st)))
              (cond
               (buffer (scan st buffer 0 0 (lexer-column st)))
               (else #f)
               ))))))))))

;;--------------------------------------------------------------------------------------------------

(define-record-type <parse-table-type>
  (make<parse-table> min-index default on-eof vector)
  parse-table-type?
  (min-index   parse-table-min-index)
  (default     parse-table-default)
  (on-eof      parse-table-on-eof)
  (vector      parse-table-vector)
  )

(define *unicode-max-code-point* (integer->char #x10FFFF))
(define *unicode-min-code-point* #\null)

(define (ensure-int c)
  (cond
   ((char? c) (char->integer c))
   ((integer? c) c)
   (else (error "index must be character or integer" c))
   ))

(define parse-table
  ;; Construct a new lexer table with indicies between and including
  ;; `MIN-INDEX` and `MAX-INDEX`. An optional 3rd argument
  ;; `DEFAULT-ACTION` sets the default tokenizer of this table, which
  ;; is evaluated when a character from the input is out-of-bounds
  ;; from this table. If `DEFAULT-ACTION` is given, an optional 4th
  ;; argument `ON-EOF` can be given as well, which is the action taken
  ;; if the input comes to an end while this table is being used to
  ;; tokenize the input.
  ;;------------------------------------------------------------------
  (case-lambda
    ((min-index max-index) (parse-table min-index max-index #f))
    ((min-index max-index default-action)
     (parse-table min-index max-index default-action #f)
     )
    ((min-index max-index default-action on-eof)
     (let ((min-index (ensure-int min-index))
           (max-index (ensure-int max-index))
           )
       (let ((min-index (min min-index max-index))
             (max-index (max min-index max-index))
             )
         (make<parse-table>
          min-index default-action on-eof
          (make-vector (+ 1 (- max-index min-index)) default-action)
          ))))))


(define (parse-table-index-bounds table)
  ;; Get the lower and upper bounds of indicies that can be looked-up
  ;; in this table. Returns two values, the smallest index and the
  ;; largest index (inclusive) that can be referenced by
  ;; `PARSE-TABLE-REF` without returning the default action.
  ;;------------------------------------------------------------------
  (let*((lo (parse-table-min-index table))
        (hi (+ lo (vector-length (parse-table-vector table)) -1))
        )
    (values lo hi)
    ))


(define (parse-table-alist-index-bounds alist)
  ;; Takes an association list of indicies to tokenizing monads (or
  ;; values that can evaluate as tokenizing monads), and computes the
  ;; minimum and maximum index of all `CAR` cells of the given
  ;; association list. This is used to allocation space for new parse
  ;; tables by the `ALIST->PARSE-TABLE` procedure. This procedure
  ;; returns two values: the minimum and maximum index.
  ;;------------------------------------------------------------------
  (let loop
      ((alist alist)
       (lo (char->integer *unicode-max-code-point*))
       (hi (char->integer *unicode-min-code-point*))
       )
    (cond
     ((pair? alist)
      (let*((assoc (car alist))
            (more  (cdr alist))
            (next
             (case-lambda
               ((a)   (loop more (min lo a)   (max hi a)))
               ((a b) (loop more (min lo a b) (max hi a b)))
               ))
            (loop-parse-table
             (lambda (table)
               (let-values
                   (((table-lo table-hi)
                     (parse-table-index-bounds table)))
                 (loop more (min lo table-lo) (max hi table-hi))
                 )))
            )
        (cond
         ((pair? assoc)
          (let ((index  (car assoc))
                (action (cdr assoc))
                )
            (cond
             ((pair? index)
              (next (ensure-int (car index))
                    (ensure-int (cdr index)))
              )
             ((char? index) (next (char->integer index)))
             ((integer? index) (next index))
             ((string? index)
              (string-for-each
               (lambda (c)
                 (set! lo (min lo (char->integer c)))
                 (set! hi (max hi (char->integer c)))
                 )
               index)
              (loop more lo hi)
              )
             ((parse-table-type? index) (loop-parse-table index))
             (else (error "not a character or character range" index))
             )))
         ((parse-table-type? assoc) (loop-parse-table assoc))
         (else (error "list contains non-pair element" assoc))
         )))
     (else
      (cond
       ((> lo hi) (values #f #f))
       (else (values lo hi))
       )))))


(define alist->parse-table
  ;; Construct a new lexer table from an association list that
  ;; associatiates characters to tokenizer procedures. Each
  ;; association pair contains some form of character index in the
  ;; `CAR` cell of the pair.
  ;;
  ;;   - `CHAR?` values are converted to offset integer indices.
  ;;
  ;;   - `INTEGER?` values are converted offset indices.
  ;;
  ;;   - `STRING?` assign the associated tokenizer to all of the
  ;;     characters in the string in the same way that passing a
  ;;     single `CHAR?` would do.
  ;;
  ;;   - `PAIR?` matching (FROM . TO) copies the associated tokenizer
  ;;     to all of the indicies between and including the indicies
  ;;     `FROM` and `TO`. These `FROM` and `TO` indicies may be
  ;;     characters or integers.
  ;;
  ;;   - `PARSE-TABLE-TYPE?` will use the content of another lexer
  ;;     table, overwriting the content of the new table with the
  ;;     content from the other table. Values in the associative list
  ;;     need not be a pair, but if the `PARSE-TABLE-TYPE?` is the
  ;;     `CAR` element of a pair, the `CDR` is used as a function to
  ;;     combine, rather than overwrite the values of the two tables.
  ;;
  ;; This constructor automatically checks the upper and lower bounds
  ;; of all `CAR` cells in the given `ALIST` and calls `PARSE-TABLE`
  ;; with the correct bounds.
  ;;
  ;; If 2 arguments are given, (1) `DEFAULT-ACTION` and (2) `ALIST`,
  ;; the `DEFAULT-ACTION` is applied to the `PARSE-TABLE` constructor.
  ;;
  ;; If 3 arguments are given, (1) `DEFAULT-ACTION`, (2) `ON-EOF`, and
  ;; (3) the `ALIST`, the `DEFAULT-ACTION` and `ON-EOF` procedures are
  ;; both applied to the `PARSE-TABLE` constructor.
  ;;------------------------------------------------------------------
  (case-lambda
    ((alist) (alist->parse-table #f alist))
    ((default-action alist)
     (alist->parse-table default-action #f alist)
     )
    ((default-action on-eof alist)
     (let*-values
         (((lo hi) (parse-table-alist-index-bounds alist))
          ((table) (parse-table lo hi default-action on-eof))
          )
       (set!alist->parse-table table alist)
       table
       ))))


(define (%parse-table-ref table i)
  (let-values
      (((i)     (ensure-int i))
       ((lo hi) (parse-table-index-bounds table))
       )
    (cond
     ((<= lo i hi)
      (vector-ref
       (parse-table-vector table)
       (- (ensure-int i) (parse-table-min-index table))
       ))
     (else (parse-table-default table))
     )))


(define (parse-table-ref table i)
  ;; Lookup an element in the parse table `TABLE` at the integer index `I`.
  (cond
   ((eof-object? i) (parse-table-on-eof table))
   ((pair? i)
    (let ((lo (ensure-int (car i)))
          (hi (ensure-int (cdr i))))
      (let ((lo (min lo hi))
            (hi (max lo hi)))
        (define (iter i)
          (if (> i hi) '() (cons i (iter (+ 1 i)))))
        (map (lambda (i) (%parse-table-ref table i)) (iter lo))
        )))
   (else
    (%parse-table-ref table (ensure-int i))
    )))


(define set!parse-table
  ;; Update a parse-table in place given an index and a tokenizer
  ;; procedure. All indices must be in-bounds, the table is not
  ;; resized to accomodate indices out of range.
  ;;
  ;; This procedure Takes 3 or 4 arguments:
  ;;
  ;;  1. `TABLE` is the table to update
  ;;
  ;;  2. `I` is the index in the `TABLE` to update
  ;;
  ;;  3. `TOKENIZER` is the procedure or monad to associate with the
  ;;      table index.
  ;;
  ;; In place of the second argument `I` you can pass 2 arguments
  ;; `FROM` and `TO`, both indicies, and all indicies between and
  ;; including those two indicies will be set to `TOKENIZER`.
  ;;------------------------------------------------------------------
  (case-lambda
    ((table i tokenizer)
     (vector-set!
      (parse-table-vector table)
      (- (ensure-int i) (parse-table-min-index table))
      tokenizer
      )
     table
     )
    ((table from to tokenizer)
     (let ((from (ensure-int from))
           (to   (ensure-int to))
           (i0   (parse-table-min-index table))
           )
       (let ((from (- (min from to) i0))
             (to   (- (max from to) i0))
             )
         (let loop ((i from))
           (cond
            ((<= i to)
             (vector-set! (parse-table-vector table) i tokenizer)
             (loop (+ 1 i))
             )
            (else table)
            )))))))


(define =>parse-table-index
  (record-unit-lens
   parse-table-ref
   set!parse-table
   '=>parse-table-index
   ))


(define (parse-table-for-each proc table)
  ;; Iterate over each of the cells in the vector of the lexer table,
  ;; evaluate the given procedure `PROC` with the offset index and the
  ;; value in the cell.
  ;;------------------------------------------------------------------
  (let*((i0 (parse-table-min-index table))
        (vec (parse-table-vector table))
        (len (vector-length vec))
        )
    (let loop ((i 0))
      (cond
       ((< i len)
        (proc (+ i i0) (vector-ref vec i))
        (loop (+ 1 i))
        )
       (else (values))
       ))))


(define (set!alist->parse-table table alist)
  ;; Update a lexer table `TABLE` with a new set of
  ;; character-to-action associations given by the `ALIST` association
  ;; list argument.
  ;;------------------------------------------------------------------
  (let loop ((alist alist))
    (cond
     ((null? alist) (values))
     (else
      (let ((assoc (car alist)))
        (cond
         ((pair? assoc)
          (let*((index  (car assoc))
                (action (cdr assoc))
                (alist  (cdr alist))
                )
            (cond
             ((pair? index)
              (set!parse-table table (car index) (cdr index) action)
              )
             ((or (char? index) (integer? index))
              (set!parse-table table index action)
              )
             ((string? index)
              (string-for-each
               (lambda (c) (set!parse-table table (char->integer c) action))
               index
               ))
             ((and (parse-table-type? index) (procedure? action))
              (parse-table-for-each
               (lambda (i tokenizer)
                 (set!parse-table table i
                  (action (%parse-table-ref table i) tokenizer)
                  ))
               index
               ))
             (else (error "not a valid table index type" index))
             )
            (loop alist)
            ))
         ((parse-table-type? assoc)
          (parse-table-for-each
           (lambda (i tokenizer)
             (set!parse-table table i tokenizer))
           assoc
           )
          (loop (cdr alist)))
         (else (error "list contains non-pair element" assoc))
         ))))))


(define lex-table
  ;; This procedure constructs a lexer monad that uses the given lexer
  ;; table as the set of rules to select the next characters in the
  ;; input. The lexer table argument applied to `LEX-TABLE` must be
  ;; constructed with the `PARSE-TABLE` procedure. The table should
  ;; contain lexer monadic procedures that can be evaluated by
  ;; `LEX-ALL`.
  ;;
  ;; It is also possible for the table to contain other types of monad
  ;; besides lexer monads, in which case you must apply two arguments:
  ;;
  ;;  1.  the monad runner, which otherwise defaults to `LEX-ALL` if
  ;;      only the `PARSE-TABLE` is applied,
  ;;
  ;;  2. the `PARSE-TABLE`
  ;;
  ;; If two arguments are applied to `LEX-TABLE`, the first must be a
  ;; monad runner such as `LEX-ALL` but suitable to whataver monadic
  ;; procedures are stored in the lexer table, the second must be the
  ;; parse table. The default monad runner is `LEX-ALL`, which takes
  ;; the lexer state and lexer monad as arguments. If you supply a
  ;; custom monad runner (which must also take a lexer state and monad
  ;; as arguments), you may evaluate tables containing different types
  ;; of monadic procedures.
  ;;
  ;; If three arguments are applied to `LEX-TABLE`:
  ;;
  ;;  1. is an arbitrary state value passed to the monad runner
  ;;  2. is the monad runner which takes three arguments:
  ;;     A. the lexer state
  ;;     B. the arbitrary state value, item (1) above
  ;;     C. the monad to evaluate
  ;;  3. is the parse table
  ;;------------------------------------------------------------------
  (case-lambda
    ((table)
     (lex-table %run-lexer table)
     )
    ((parser-state run-monad table)
     (lex-table (lambda (st monad) (run-monad st parser-state monad)) table)
     )
    ((run-monad table)
     (make<lexer-monad>
      (lambda (st)
        (let ((c (%look st)))
          (cond
           ((eof-object? c)
            (let ((on-eof (parse-table-on-eof table)))
              (cond
               ((procedure? on-eof) (on-eof c))
               (else #f)
               )))
           (else
            (let*((i (char->integer c))
                  (i0 (parse-table-min-index table))
                  (vec (parse-table-vector table))
                  (top (+ i0 (vector-length vec)))
                  (default (parse-table-default table))
                  (run-default
                   (if default
                       (lambda () (%run-lexer st default))
                       (lambda () #f)
                       ))
                  )
              (cond
               ((< i  i0) (run-default))
               ((< i top) (%run-lexer st (vector-ref vec (- i i0))))
               (else (run-default))
               ))))
          ))))))

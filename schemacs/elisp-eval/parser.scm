
(define-record-type <elisp-tokenizer-monad-type>
  ;; Every monadic procedure takes a single argument, the parser state
  ;; (which contains the tokenizer state), and returns 3 values:
  ;;
  ;;  1. the token ID
  ;;  2. the tokenized string (which may be #f)
  ;;  3. the next parser in the state to be used
  ;;------------------------------------------------------------------
  (make<elisp-tokenizer-monad> proc)
  elisp-tokenizer-monad?
  (proc  elisp-tokenizer-procedure)
  )

(define-record-type <elisp-tokenizer-error-type>
  (make<elisp-tokenizer-error> message irritants)
  elisp-tokenizer-error-type?
  (message   elisp-tokenizer-error-message)
  (irritants elisp-tokenizer-error-irritants)
  )

(define (elisp-tokenizer-error message . irritants)
  (make<elisp-tokenizer-error> message irritants))

(define (%run-elisp-tokenizer st proc)
  ((elisp-tokenizer-procedure proc) st))

(define (run-elisp-tokenizer st)
  ;; Lexes the next Elisp token from the given state which is some
  ;; kind of character input stream port.
  (%run-elisp-tokenizer (lexer-state st) elisp-tokenizer))


(define token-error         0)
(define token-eof           1)
(define token-space         2)
(define token-open-bracket  3)
(define token-close-bracket 4)
(define token-symbol        5)
(define token-dot           6)
(define token-int           7)
(define token-float         8)
(define token-string        9)
(define token-char         10)
(define token-quote        11)
(define token-backquote    12)
(define token-unquote      13)
(define token-splice       14)
(define token-hashcode     15)


(define token-type-names
  ;; not exported, used only for debugging, should be eliminated as dead code.
  (vector
   "error" "EOF" "space" "open-bracket" "close-bracket" "symbol" "dot" "int"
   "float" "string" "char" "quote" "backquote" "unquote" "splice" "hashcode"))

(define (token-type-name token-type)
  ; dead code, only used for debugging
  (if (< token-type (vector-length token-type-names))
      (vector-ref token-type-names token-type)
      "<token-type-error>"
      ))

(define (whitespace? c)
  (or (char<=? c #\space)
      (char=? c #\delete)
      ))

(define skip-space-chars
  (many1 whitespace?))

(define elisp-special-char "#;()[]\"'`\\")

(define symbol-char-table
  ;; This table also consumes dots and strings of numerical digits as
  ;; these characters can be used as parts of symbols as
  ;; well. Backslash symbols are special symbol characters and are
  ;; handled separately.
  (alist->parse-table  #t
   `(((#\null . #\space) . #f)
      ;; ^ the first 33 ASCII chars (control characters) are considered spaces, not symbol chars
     (#\delete . #f)
      ;; ^ the delete character (0x7F) is a control character and also not a symbol char
     (,elisp-special-char . #f)
      ;; ^ special non-symbol non-space characters are not symbol chars
     )))

(define (%buffer-if buffer char-pred?)
  (lambda (c)
    (cond
     ((char-pred? c) (write-char c buffer) c)
     (else #f)
     )))

(define (%accum-dec-int buffer st)
  (lambda (accum count c)
    (cond
     ((char-numeric? c)
      (write-char c buffer)
      (values #t (+ (* 10 accum) (digit-value c)))
      )
     (else (values #f accum))
     )))

(define (%tokenize-decimal-digits buffer st)
  (run-lexer st
   (lex-fold-count 0
    (%accum-dec-int buffer st)
    (lambda (accum count)
      (if (> count 0)
          (values accum count)
          (values #f 0)
          ))
    (char char-numeric?)
    )))

(define (negative i) (- i))
(define (positive i) i)

(define (%lex-sign buffer)
  ;; Note that this must run after we are already certain that the
  ;; next character in the stream is a decimal digit. So it is safe to
  ;; use an element in a lexer table, or after a call to `LOOK`.
  (either
   (take
    (lambda (c)
      (cond
       ((char=? c #\+)
        (write-char #\+ buffer)
        positive
        )
       ((char=? c #\-)
        (write-char #\- buffer)
        negative
        )
       (else
        #f)
       )))
   (lex-const positive)))

(define (%tokenize-sign buffer st)
  (let ((result (run-lexer st (%lex-sign buffer))))
    (if result result positive)
    ))

(define (%tokenize-signed-int buffer st)
  (let*-values
      (((sign) (%tokenize-sign buffer st))
       ((digits-value sig-figs) (%tokenize-decimal-digits buffer st))
        )
    (if digits-value (sign digits-value) #f)
    ))

(define (%tokenize-fraction buffer st)
  (let*((delim
         (run-lexer st
          (%buffer-if buffer (lambda (c) (char=? c #\.))))
         ))
    (if (not delim) (values #f 0)
        (run-lexer st
         (lex-fold-count 0
          (%accum-dec-int buffer st)
          (lambda (accum count)
            (cond
             ((> count 0)
              (let ((e (expt 10 count))) (values (/ accum e) count)))
             (else (values #f 0))
             ))
          (char char-numeric?)
          )))))

(define (%tokenize-exponent buffer st)
  (let*((delim
         (run-lexer st
          (%buffer-if buffer
           (lambda (c) (or (char=? c #\e) (char=? c #\E))))))
        (int (if (not delim) #f (%tokenize-signed-int buffer st)))
        )
    (if (not delim) 0 (if int int #f))
    ))

(define (%return-symbol buffer)
  (let ((return (get-output-string buffer)))
    (close-port buffer)
    (values token-symbol (string->symbol return))
    ))

(define (%return-float buffer sign coef frac e sig-figs)
  (close-port buffer)
  (values
   token-float
   (sign (* (+ coef frac) (expt 10 e)))
   ))


(define tokenize-number
  ;; This procedure can tokenize a signed or unsigned number depending
  ;; on the calling context. A lexer table, for example, can
  ;; explicitly pass `POSITIVE` or `NEGATIVE` when on a `#\+` or `#\-`
  ;; character.
  (make<elisp-tokenizer-monad>
   (lambda (st)
     ;; When GNU Emacs tokenizes numerical literals, if there are any
     ;; characters that do not follow the numerical lexer rules, even
     ;; if these characters are immediately after a fully well-formed
     ;; numerical literal, the GNU Emacs lexer will treat the
     ;; characters as a symbol, not a numerical literal. To replicate
     ;; this behavior, we build the numerical literal as it is being
     ;; lexically analyzed, but we also buffer every character that
     ;; comprises the numerical literal. If any lexical errors occur,
     ;; a symbol constructed from the buffered characters, and the
     ;; symbol string is returned rather than a numerical literal.
     (let*((buffer    (open-output-string))
           (sign-char (lexer-look-ahead st))
           (sign      (%tokenize-sign buffer st))
           )
       (define (return-int sign coef e)
         (close-port buffer)
         (values token-int (sign (* coef (expt 10 e))))
         )
       (cond
        ((run-lexer st skip-space-chars)
         (values token-symbol (string->symbol (make-string 1 sign-char)))
         )
        (else
         (let*-values
             (((coefficient cn) (%tokenize-decimal-digits buffer st))
              ((fraction    fn) (%tokenize-fraction buffer st))
              ((sig-figs)       (+ cn fn))
              ((exponent)       (%tokenize-exponent buffer st))
              ((symbol)         (run-lexer st (lex/buffer buffer lex-symbol)))
              )
           (cond
            (coefficient
             (cond
              (symbol (%return-symbol buffer))
              (fraction
               (%return-float buffer sign coefficient fraction exponent sig-figs))
              (else (return-int sign coefficient exponent))
              ))
            (else ;; we could have a float-literal starting with a decimal point
             (cond
              (symbol (%return-symbol buffer))
              (fraction (%return-float buffer sign 0 fraction exponent sig-figs))
              (coefficient (return-int sign coefficient exponent))
              (sign (%return-symbol buffer))
              (else (values #f #f))
              ))))))))))


(define escape-string-table
  (alist->parse-table (any)
   `((#\\  . ,(any #\\ ))
     (#\a  . ,(any #\alarm))
     (#\b  . ,(any #\backspace))
     (#\d  . ,(any #\delete))
     (#\e  . ,(any #\escape))
     (#\f  . ,(any #\x0C))
     (#\n  . ,(any #\newline))
     (#\r  . ,(any #\return))
     (#\s  . ,(any #\space))
     (#\t  . ,(any #\tab))
     (#\v  . ,(any #\x0B))
     )))


(define tokenize-string
  (make<elisp-tokenizer-monad>
   (lambda (st)
     (let ((open (run-lexer st #\")))
       (cond
        (open
         (let*((result
                (run-lexer st
                 (many1/buffer
                  (many/buffer
                   (char (lambda (c) (and (not (char=? c #\\ )) (not (char=? c #\"))))))
                  (many1/buffer
                   (take (lambda (c) (and (char? c) (char=? c #\\ )))) escape-string-table)
                  )))
               (closed
                (run-lexer st
                 (take (lambda (c) (and (char? c) (char=? c #\" ))))
                 )))
           (cond
            ((not closed)
             (values token-error
              (elisp-tokenizer-error
               "string literal runs past end of input"
               result
               )))
            (else (values token-string result))
            )))
        (else (values #f #f))
        )))))


(define lex-partial-symbol (many1/buffer (lex-table symbol-char-table)))


(define escape-symbols (many1/buffer (lex #\\ (lex/buffer (any)))))


(define lex-symbol
  (many1/buffer (either escape-symbols lex-partial-symbol))
  )

(define tokenize-symbol
  (make<elisp-tokenizer-monad>
   (lambda (st)
     (let ((result (run-lexer st lex-symbol)))
       (if result
           (values token-symbol (string->symbol result))
           (values #f #f)
           )))))

(define *unicode-max-code-point-integer*
  (char->integer *unicode-max-code-point*))

(define (assert-int-shift-add shift-add)
  ;; This is an accumulating lexer used by `LEX-FOLD`.
  (lambda (accum c)
    (let ((accum (shift-add accum c)))
      (cond
       ((> accum *unicode-max-code-point-integer*)
        (values #f (new-lexer-error "character literal value too large"))
        )
       (else (values #t accum))
       ))))


(define (char-int-literal in-bound? shift-add)
  (lex-fold 0 (assert-int-shift-add shift-add) (lex in-bound?)))

(define (ndigit-char-int-literal exact-count on-err in-bound? shift-add)
  (lex-fold-count 0
   (lambda (accum count c)
     ((assert-int-shift-add shift-add) accum c))
   (lambda (accum count)
     (if (= count exact-count) (integer->char accum) (new-lexer-error on-err)))
   in-bound?
   ))

(define (basechar top base)
  (char-int-literal
   (lambda (c)
     (and (char>=? c #\0) (char<=? c top) (digit-value c)))
   (lambda (accum c) (+ (* accum base) c))
   ))

(define binchar (basechar #\1 2))
(define octchar (basechar #\7 8))

(define (upperhex-int c)
  (+ 10 (- (char->integer c) (char->integer #\A))))

(define (lowerhex-int c)
  (+ 10 (- (char->integer c) (char->integer #\a))))

(define (hexdigit-value c)
  (or (and (char>=? c #\0) (char<=? c #\9) (digit-value  c))
      (and (char>=? c #\A) (char<=? c #\F) (upperhex-int c))
      (and (char>=? c #\a) (char<=? c #\f) (lowerhex-int c))
      ))

(define (hexencode-fold accum c) (+ (* accum 16) c))

(define hexchar
  (char-int-literal hexdigit-value hexencode-fold))

(define (unichar exact-count on-err)
  (lex (any)
   (ndigit-char-int-literal exact-count on-err hexdigit-value hexencode-fold)))

(define named-char-table
  (alist->parse-table
   '(((#\! . #\~) . #t)
     (#\{         . #f)
     (#\}         . #f)
     )))

(define hexdecode-string
  (case-lambda
    ((str) (hexdecode-string 0 str))
    ((i str)
     (let ((len (string-length str)))
       (let loop ((i i) (accum 0))
         (cond
          ((< i len)
           (let ((c (hexdigit-value (string-ref str i))))
             (cond
              (c (loop (+ 1 i) (hexencode-fold accum c)))
              (else
               (new-lexer-error "invalid hexadecimal unicode character literal")
               ))))
          (else (integer->char accum))
          ))))))

(define *unicode-lookup-by-name*
  (make-parameter
   (lambda (name)
     (cond
      ((equal? name "GREEK SMALL LETTER LAMBDA") #\x3BB)
      (else #f)
      ))))

(define named-char
  (lex #\N
   (lex-brackets
    #\{ #\}
    (lex-apply
     (lambda (result)
       (cond
        ((string? result)
         (cond
          ((< (string-length result) 2)
           (new-lexer-error
            "unknown named character literal"
            (list result)
            ))
          ((and (char=? #\U (string-ref result 0))
                (char=? #\+ (string-ref result 1)))
           (hexdecode-string 2 result)
           )
          (else
           (let ((c ((*unicode-lookup-by-name*) result)))
             (if c c
                 (new-lexer-error
                  "unknown unicode symbol name"
                  (list result))
                 )))))
        ((lexer-error-type? result) result)
        (else (new-lexer-error "invalid named character literal" result))
        ))
     (many/buffer
      (many1/buffer (lex-table named-char-table))
      ;; ^ gather non-spaces
      (lex skip-space-chars (lex-const #\space))
      ;; ^ skip many spaces, return single space
      )))))

(define (ascii-ctrl-char c)
  (or (and
       (char>=? c #\@)
       (char<=? c #\_)
       (integer->char (- (char->integer c) (char->integer #\@)))
       )
      (and
       (char>=? c #\a)
       (char<=? c #\z)
       (integer->char
        (- (char->integer c) (- (char->integer #\a) 1))
        ))
      (and (char=? c #\?) #\delete)
      (+ #x4000000 (char->integer c))
      ))

(define no-eof-in-char-literal
  (eof (new-lexer-error "invalid character literal")))

(define ascii-carrat-control-char
  (lex #\^ (either  no-eof-in-char-literal  ascii-ctrl-char)))

(define ascii-c-dash-control-char
  (lex #\C
   (either  no-eof-in-char-literal
    (lex #\-
     (either  no-eof-in-char-literal  ascii-ctrl-char)
     ))))

(define tokenize-escape-char-table
  (alist->parse-table (any)
   `(,escape-string-table
     (#\x . ,hexchar)
     (#\X . ,hexchar)
     (#\u . ,(unichar 4 "expecting exactly 4 hexdigits after \"?\\u\" prefix"))
     (#\U . ,(unichar 8 "expecting exactly 8 hexdigits after \"?\\U\" prefix"))
     (#\N . ,named-char)
     (#\^ . ,ascii-carrat-control-char)
     (#\C . ,ascii-c-dash-control-char)
     )))


(define (%tokenize-escape-char st)
  (run-lexer st (lex-table tokenize-escape-char-table)))


(define tokenize-char
  (make<elisp-tokenizer-monad>
   (lambda (st)
     (run-lexer st #\? ) ;; skip the question mark
     (let ((c (run-lexer st (any))))
       (cond
        ((eof-object? c) (values token-symbol '?))
        ((char=? c #\\)
         (let*((charval (%tokenize-escape-char st)))
           (cond
            (charval (values token-char charval))
            ((eof-object? charval)
             (values (new-lexer-error "end of file during parsing"))
             )
            ((lexer-error-type? charval) (values charval #f))
            (else (error "unexpected condition during lexing" charval))
            )))
        ((char? c) (values token-char c))
        (else (error "unexpected condition during lexing" c))
        )))))


(define tokenize-dot
  ;; Tokenize a symbol, number, or the infix `CONS` syntax.
  (make<elisp-tokenizer-monad>
   (lambda (st)
     (cond
      ((run-lexer st #\. )
       (cond
        ((run-lexer st skip-space-chars) (values token-dot #t))
        (else
         (let*-values
             (((buffer)            (open-output-string))
              ((fraction sig-figs) (%tokenize-decimal-digits buffer st))
              ((exponent)          (%tokenize-exponent buffer st))
              ((symbol)            (run-lexer st lex-symbol))
              )
           (cond
            (symbol (%return-symbol buffer))
            (fraction
             (%return-float
              buffer positive 0
              (/ fraction (expt 10 sig-figs))
              exponent sig-figs
              ))
            (else (values token-dot #t))
            )))))
      (else (values #f #f))
      ))))


(define (tokenize-quote qtype)
  ;; WARNING: it is assumed this tokenizer is called from a table and
  ;; that the character under the cursor has already matched a quote
  ;; or backquote. The lexer will not check whether the character
  ;; matches again, it will simply succeed and return the `QTYPE`.
  (make<elisp-tokenizer-monad>
   (lambda (st)
     (let ((result (run-lexer st (take (lambda _ #t)))))
       (if result
           (values qtype #t)
           (values #f #f)
           )))))


(define tokenize-spaces
  (make<elisp-tokenizer-monad>
   (lambda (st)
     (let ((result
            (run-lexer st
             (many1
              (take
               (lambda (c)
                 (and (char? c)
                      (or (char=? c #\delete)
                          (char<=? c #\space))))))))
           )
       (cond
        (result (%run-elisp-tokenizer st tokenize-comment))
        (else (values #f #f))
        )))))


(define tokenize-comment
  (make<elisp-tokenizer-monad>
   (lambda (st)
     (let loop ((result (run-lexer st (char #\;))))
       (cond
        (result
         (run-lexer st (many (take (lambda (c) (not (char=? c #\newline))))))
         (%run-elisp-tokenizer st tokenize-spaces)
         (values token-space #t)
         )
        (else (values token-space #t))
        )))))


(define tokenize-space-table
  (alist->parse-table
   `(((#\null . #\space) . ,tokenize-spaces) ;; space is ignored, control characters are spaces
     (#\;                . ,tokenize-comment)
     (#\delete           . ,tokenize-spaces) ;; delete is also a control character and considered space
     )))


(define tokenize-special
  (make<elisp-tokenizer-monad>
   (lambda (st)
     (run-lexer st (any))
     (values token-hashcode #t)
     )))


(define tokenize-eof
  (make<elisp-tokenizer-monad>
   (lambda (st) (values token-eof (run-lexer st (look))))
   ))


(define (tokenize-bracket sym)
  (make<elisp-tokenizer-monad>
   (lambda (st)
     (values sym (run-lexer st (any)))
     )))


(define tokenize-unquote
  (make<elisp-tokenizer-monad>
   (lambda (st)
     (run-lexer st #\, ) ;; skip leading comma
     (let ((token-type
            (run-lexer st
             (either
              (take (lambda (c) (and (char=? c #\@) token-splice)))
              (lex-const token-unquote)
              )))
           )
       (values token-type #t)
       ))))


(define elisp-tokenizer-table
  (alist->parse-table
   tokenize-symbol
   tokenize-eof
   `(,tokenize-space-table 
     ((#\!    . #\~ )    . ,tokenize-symbol)
     (#\.                . ,tokenize-dot)
     (#\?                . ,tokenize-char)
     ((#\0    . #\9 )    . ,tokenize-number)
     (#\+                . ,tokenize-number)
     (#\-                . ,tokenize-number)
     (#\"                . ,tokenize-string)
     (#\'                . ,(tokenize-quote token-quote))
     (#\`                . ,(tokenize-quote token-backquote))
     (#\,                . ,tokenize-unquote)
     (#\;                . ,tokenize-comment)
     (#\#                . ,tokenize-special)
     (#\(                . ,(tokenize-bracket token-open-bracket))
     (#\[                . ,(tokenize-bracket token-open-bracket))
     (#\{                . ,(tokenize-bracket token-open-bracket))
     (#\}                . ,(tokenize-bracket token-close-bracket))
     (#\]                . ,(tokenize-bracket token-close-bracket))
     (#\)                . ,(tokenize-bracket token-close-bracket))
     )))


(define elisp-tokenizer
  (make<elisp-tokenizer-monad>
   (lambda (st)
     (let ((step
            (run-lexer st
             (look (lambda (c) (parse-table-ref elisp-tokenizer-table c)))))
           )
       (%run-elisp-tokenizer st step)
       ))))

;;--------------------------------------------------------------------------------------------------

;; Quoting Scheme data for use in the elisp interpreter.

(define-record-type <elisp-quote-scheme-type>
  ;; This type is for wraps a Scheme value to be used directly in the
  ;; Elisp interpreter. This helps to prevents things like strings,
  ;; vectors, and list constants from being evaluated as lists in the
  ;; Elisp environment. Being wrapped in this data type indicates to
  ;; the Elisp interpreter/compiler that the value should be used
  ;; as-is. It also is produced by the Elisp parser in the AST as the
  ;; `(QUOTE ...)` syntax and the quasiquote syntax.
  ;;------------------------------------------------------------------
  (elisp-quote-scheme  scheme-value  is-backquote)
  elisp-quote-scheme-type?
  (scheme-value  elisp-unquote-scheme)
  (is-backquote  elisp-backquoted-form?)
  )

(define elisp-quote-scheme-equal?
  (case-lambda
    ((a b)
     ((elisp-quote-scheme-equal? equal?) a b)
     )
    ((equal?)
     (lambda (a b)
       (and
        (elisp-quote-scheme-type? a)
        (elisp-quote-scheme-type? b)
        (eq? (elisp-backquoted-form? a)
             (elisp-backquoted-form? b)
             )
        (equal?
         (elisp-unquote-scheme a)
         (elisp-unquote-scheme b)
         ))))))

;;--------------------------------------------------------------------

(define-record-type <elisp-unquoted-form-type>
  ;; This is the counterpart to the `ELISP-QUOTE-SCHEME-TYPE?`
  ;; structure, it wraps a value that should be interpreted by the
  ;; Elisp interpreter as an exception to the rule that forms within
  ;; the `ELISP-QUOTE-SCHEME-TYPE?` should not be interpreted. It is
  ;; also produced by the Elisp parser in the AST as the unquote form
  ;; syntax.
  ;;------------------------------------------------------------------
  (elisp-unquoted-form form splice?)
  elisp-unquoted-form-type?
  (form     elisp-unquoted-get-form)
  (splice?  elisp-spliced-form?)
  )

(define elisp-unquoted-form-equal?
  (case-lambda
    ((a b)
     ((elisp-unquoted-form-equal? equal?) a b)
     )
    ((equal?)
     (lambda (a b)
       (and
        (elisp-unquoted-form-type? a)
        (elisp-unquoted-form-type? b)
        (eq? (elisp-spliced-form? a)
             (elisp-spliced-form? b)
             )
        (equal?
         (elisp-unquoted-get-form a)
         (elisp-unquoted-get-form b)
         ))))))

;;--------------------------------------------------------------------------------------------------

(define-record-type <elisp-form-type>
  (make<elisp-form> tokens dotted locations start-loc end-loc)
  elisp-form-type?
  (tokens     elisp-form-tokens       set!elisp-form-tokens)
  (dotted     elisp-form-dot-element  set!elisp-form-dot-element)
  (locations  elisp-form-locations    set!elisp-form-locations)
  (start-loc  elisp-form-start-loc    set!elisp-form-start-loc)
  (end-loc    elisp-form-end-loc      set!elisp-form-end-loc)
  )

(define (elisp-form-length a)
  (+ (vector-length (elisp-form-tokens a))
     (if (elisp-form-dot-element a) 1 0)
     ))


(define (quote-syntax? elem)
  (and (pair? elem)
       (pair? (cdr elem))
       (null? (cddr elem))
       elem
       ))


(define list->elisp-form
  (case-lambda
    ((elems) (list->elisp-form 32 elems))
    ((prealloc elems)
     (cond
      ((pair? elems)
       (let ((next (car elems)))
         (define (pack)
           (let ((buffer (new-mutable-vector prealloc)))
             (let loop ((elems elems))
               (cond
                ((pair? elems)
                 (let ((next (car elems)))
                   (mutable-vector-append!
                    buffer
                    (cond
                     ((pair? next)
                      (list->elisp-form prealloc next)
                      )
                     (else next)
                     ))
                   (loop (cdr elems))
                   ))
                ((null? elems)
                 (make<elisp-form>
                  (mutable-vector->vector buffer)
                  #f #f #f #f
                  ))
                (else
                 (make<elisp-form>
                  (mutable-vector->vector buffer)
                  elems #f #f #f
                  ))))))
         (cond
          ((symbol? next)
           (let*((mktail
                  (lambda ()
                    (cond
                     ((null? (cddr elems))
                      (list->elisp-form (cadr elems))
                      )
                     (else
                      (error "invalid quote syntax" next (cdr elems))
                      )))))
             (case next
               ((quote) (elisp-quote-scheme (mktail) #f))
               ((quasiquote) (elisp-quote-scheme (mktail) #t))
               ((unquote) (elisp-unquoted-form (mktail) #f))
               ((unquote-splicing) (elisp-unquoted-form (mktail) #t))
               (else (pack))
               )))
          (else (pack))
          )))
      (else elems)
      ))))


(define elisp-form->list
  ;; In order to preserve the file locations of each form for as long
  ;; as possible, this procedure will by default only convert the
  ;; top-most form to a list, the elements of the form will remain as
  ;; forms. If you want a recursive conversion, pass `#T` as the first
  ;; argument to this procedure.
  ;;------------------------------------------------------------------
  (case-lambda
    ((form) (elisp-form->list #f form))
    ((recursive form)
     (cond
      ((elisp-form-type? form)
       (let*((vec (elisp-form-tokens form))
             (len (vector-length vec))
             (final (elisp-form-dot-element form))
             )
         (let loop ((i 0))
           (cond
            ((< i len)
             (cons
              (let ((elem (vector-ref vec i)))
                (if recursive (elisp-form->list recursive elem) elem)
                )
              (loop (+ 1 i))
              ))
            (final final)
            (else '())
            ))))
      ((elisp-quote-scheme-type? form)
       (elisp-quote-scheme
        (elisp-form->list recursive (elisp-unquote-scheme form))
        (elisp-backquoted-form? form)
        ))
      ((elisp-function-ref-type? form)
       (make<elisp-function-ref>
        (elisp-function-ref-loc form)
        (elisp-form->list recursive (elisp-function-get-ref form))
        ))
      (else form)
      ))))


(define (elisp-form-equal? a b)
  ;; Compare two forms by content, ignoring location.
  (or
   (and (not a) (not b))
   (elisp-function-ref-equal? a b)
   (and (symbol? a) (symbol? b) (eq? a b))
   ((elisp-quote-scheme-equal? elisp-form-equal?) a b)
   ((elisp-unquoted-form-equal? elisp-form-equal?) a b)
   (and
    (elisp-form-type? a)
    (elisp-form-type? b)
    (= (elisp-form-length a)
       (elisp-form-length b)
       )
    (equal?
     (elisp-form-dot-element a)
     (elisp-form-dot-element b)
     )
    (let*((a (elisp-form-tokens a))
          (b (elisp-form-tokens b))
          (len (vector-length a))
          )
      (or
       (and (not a) (not b))
       (and
        a b ;; both are defined
        (let loop ((i 0))
          (cond
           ((= i len) #t)
           (else
            (let ((elem-a (vector-ref a i))
                  (elem-b (vector-ref b i))
                  )
              (cond
               ((or
                 (elisp-form-equal? elem-a elem-b)
                 (equal? elem-a elem-b)
                 )
                (loop (+ 1 i))
                )
               (else #f)
               ))))))
       )))))

;;--------------------------------------------------------------------------------------------------

(define-record-type <elisp-function-ref>
  (make<elisp-function-ref> loc ref)
  elisp-function-ref-type?
  (loc elisp-function-ref-loc)
  (ref elisp-function-get-ref) ;; a form or symbol
  )

(define (elisp-function-ref-equal? a b)
  (and
   (elisp-function-ref-type? a)
   (elisp-function-ref-type? b)
   (elisp-form-equal?
    (elisp-function-get-ref a)
    (elisp-function-get-ref b)
    )))

;;--------------------------------------------------------------------------------------------------

(define-record-type <elisp-parse-state-type>
  (make<elisp-parse-state>
   tokenizer-state
   token-buffer
   location-buffer
   match-bracket
   depth-counter
   backrefs
   buffer
   )
  elisp-parse-state-type?
  (tokenizer-state  elisp-parse-tokenizer-state)
  (token-buffer     elisp-parse-token-buffer     set!elisp-parse-token-buffer)
  (location-buffer  elisp-parse-location-buffer  set!elisp-parse-location-buffer)
  (match-bracket    elisp-parse-close-bracket    set!elisp-parse-close-bracket)
  (depth-counter    elisp-parse-depth-counter    set!elisp-parse-depth-counter)
  (backrefs         elisp-parse-backref-dict     set!elisp-parse-backref-dict)
  (buffer           elisp-parse-stack            set!elisp-parse-stack)
  )

(define-record-type <elisp-parse-stack-frame-type>
  (make<elisp-parse-stack-frame> close forms locations start-loc)
  elisp-parse-stack-frame-type?
  (close      elisp-parse-stack-frame-close)
  (forms      elisp-parse-stack-frame-forms)
  (locations  elisp-parse-stack-frame-locations)
  (start-loc  elisp-parse-stack-frame-start-loc)
  )

(define-record-type <elisp-parse-monad-type>
  (make<elisp-parse-monad> proc)
  elisp-parse-monad-type?
  (proc  elisp-parse-monad-procedure)
  )


(define-record-type <elisp-parse-error-type>
  (make<elisp-parse-error> message location irritants)
  elisp-parse-error-type?
  (message    elisp-parse-error-message)
  (location   elisp-parse-error-location)
  (irritants  elisp-parse-error-irritants)
  )


(define (elisp-parse-error message location . irritants)
  (make<elisp-parse-error> message location irritants)
  )


(define parse-state
  (case-lambda
    ((port line)
     (parse-state (lexer-state port line))
     )
    ((port line column)
     (parse-state (lexer-state port line column))
     )
    ((port line column filepath)
     (parse-state (lexer-state port line column filepath))
     )
    ((port line column filepath cont)
     (parse-state (lexer-state port line column filepath cont))
     )
    ((init)
     (cond
      ((elisp-parse-state-type? init) init)
      ((or (input-port? init) (string? init))
       (parse-state (lexer-state init)))
      ((lexer-state-type? init)
       (make<elisp-parse-state> init #f #f #f 0 #f '()))
      (else (error "value cannot be use to initialize parser state" init))
      ))))


(define (%run-parse-monad st loc token monad)
  ((elisp-parse-monad-procedure monad) st loc token))


(define (run-elisp-parse-monad init-state token . monads)
  (let ((st (parse-state init-state)))
    (let loop ((monads monads) (result #f))
      (cond
       ((null? monads) result)
       (else
        (let ((next (car monads)))
          (cond
           ((elisp-parse-monad-type? next)
            (loop
             (cdr monads)
             (%run-parse-monad st
              (parser-state-get-location st)
              token next
              )))
           (else
            (error "not a parse monad" next)
            ))))))))


(define =>elisp-parse-token-buffer*!
  (record-unit-lens
   elisp-parse-token-buffer
   set!elisp-parse-token-buffer
   '=>elisp-parse-token-buffer*!))

(define =>elisp-parse-location-buffer*!
  (record-unit-lens
   elisp-parse-location-buffer
   set!elisp-parse-location-buffer
   '=>elisp-parse-location-buffer*!))

(define =>elisp-parse-close-bracket*!
  (record-unit-lens
   elisp-parse-close-bracket
   set!elisp-parse-close-bracket
   '=>elisp-parse-close-bracket*!))

(define =>elisp-parse-depth-counter*!
  (record-unit-lens
   elisp-parse-depth-counter
   set!elisp-parse-depth-counter
   '=>elisp-parse-depth-counter*!))

(define =>elisp-parse-stack*!
  (record-unit-lens
   elisp-parse-stack
   set!elisp-parse-stack
   '=>elisp-parse-stack*!))

(define =>elisp-parse-backref-dict*!
  (record-unit-lens
   elisp-parse-backref-dict
   set!elisp-parse-backref-dict
   '=>elisp-parse-backref-dict*!))

(define (elisp-parse-stack-frame st)
  (make<elisp-parse-stack-frame>
   (elisp-parse-close-bracket st)
   (elisp-parse-token-buffer st)
   (elisp-parse-location-buffer st)
   (parser-state-get-location st)
   ))

(define =>parse-state-filepath*!
  (lens
   (=>view-only-lens
    elisp-parse-tokenizer-state
    '=>parse-state-filepath*!
    )
   =>lexer-filepath*!
   ))


(define elisp-form
  (case-lambda
    ((st) (elisp-form st #f))
    ((st dot-value)
     (let ((frame (car (elisp-parse-stack st))))
       (make<elisp-form>
        (mutable-vector->vector (elisp-parse-token-buffer st))
        dot-value
        (mutable-vector->vector (elisp-parse-location-buffer st))
        (elisp-parse-stack-frame-start-loc frame)
        (parser-state-get-location st)
        )))))


(define (opposing-bracket open)
  (case open
    (( #\( ) #\) )
    (( #\[ ) #\] )
    (( #\{ ) #\} )
    (( #f )  #f  )
    (else (error "unknown bracket token" open))
    ))


(define (%push-token-buffer! st open-bracket)
  (update
   (lambda (stack) (cons (elisp-parse-stack-frame st) stack))
   st =>elisp-parse-stack*!
   )
  (update (lambda (depth) (+ 1 depth)) st =>elisp-parse-depth-counter*!)
  (set!elisp-parse-token-buffer    st (new-mutable-vector 32))
  (set!elisp-parse-location-buffer st (new-mutable-vector 64))
  (set!elisp-parse-close-bracket   st (opposing-bracket open-bracket))
  #t
  )

(define (%pop-token-buffer! st close-bracket dot-value)
  (let ((stack (elisp-parse-stack st)))
    (cond
     ((null? stack)
      (elisp-parse-error
       "unbalanced expression, too many closing brackets"
       (parser-state-get-location st)
       ))
     (else
      (let*((form  (elisp-form st dot-value))
            (frame (car stack))
            (expected-close-bracket (elisp-parse-close-bracket st))
            )
        (cond
         ((or (and (not close-bracket) (not expected-close-bracket))
              (char=? close-bracket expected-close-bracket))
          (set!elisp-parse-token-buffer    st (elisp-parse-stack-frame-forms     frame))
          (set!elisp-parse-location-buffer st (elisp-parse-stack-frame-locations frame))
          (set!elisp-parse-close-bracket   st (elisp-parse-stack-frame-close     frame))
          (set!elisp-parse-stack           st (cdr stack))
          (update (lambda (a) (- a 1)) st =>elisp-parse-depth-counter*!)
          form
          )
         (else
          (elisp-parse-error
           "mismatched closing bracket"
           (parser-state-get-location st)
           close-bracket 'expecting expected-close-bracket
           ))))))))

;;----------------------------------------------------------

(define (parser-state-get-location st)
  (lexer-state-get-location (elisp-parse-tokenizer-state st))
  )

(define (%get-location st)
  ;; Used internally by `WRITE-PARSER-LOCATION`, skips the
  ;; construction of a `SOURCE-FILE-LOCATION-TYPE?` and returns
  ;; whatever type contains the information that is useful to
  ;; reporting.
  (cond
   ((source-file-location-type? st) st)
   ((lexer-state-type? st) st)
   ((elisp-parse-state-type? st) (elisp-parse-tokenizer-state st))
   ((elisp-form-type? st) (elisp-form-start-loc st))
   (else
    (error "not a structure that contains a parser location" st)
    )))

(define (get-location st)
  ;; Get the parser/lexer location information from the applied
  ;; argument object. This could be a `SOURCE-FILE-LOCATION-TYPE?`, a
  ;; `LEXER-STATE-TYPE?`, a `ELISP-PARSE-STATE-TYPE?`, or an
  ;; `ELISP-FORM-TYPE?`.
  ;;------------------------------------------------------------------
  (cond
   ((source-file-location-type? st) st)
   ((lexer-state-type? st) (lexer-state-get-location st))
   ((elisp-parse-state-type? st) (parser-state-get-location st))
   ((elisp-form-type? st) (elisp-form-start-loc st))
   (else
    (error "not a structure that contains a parser location" st)
    )))

(define write-parser-location
  ;; Apply any value for which `GET-LOCATION` returns a source file
  ;; location and write this information to `CURRENT-OUTPUT-PORT`.
  ;; Can take 1, 2, or 3 arguments:
  ;;
  ;;  - if 1 arguments, apply it to `GET-LOCATION` and apply the
  ;;    result of that to `WRITE-LEXER-LOCATION`.
  ;;
  ;;  - if 2 arguments, first should be the file path, second should
  ;;    be a value applied to `GET-LOCATION`, apply both the filepath
  ;;    and location to the `WRITE-LEXER-LOCATION` procedure.
  ;;------------------------------------------------------------------
  (case-lambda
    ((location) (write-parser-location location (current-output-port)))
    ((location port)
     (write-lexer-location (%get-location location) port))))

;;--------------------------------------------------------------------------------------------------

(define parser-tokenizer-error
  (make<elisp-parse-monad>
   (lambda (st tokenizer-error)
     (make<elisp-parse-error>
      "tokenizer error"
      (parser-state-get-location st)
      (list tokenizer-error)
      ))))


(define parse-space
  (make<elisp-parse-monad> (lambda (st loc token) #t)))


(define (%elisp-append-token! st loc token)
  (let ((tok-buf (elisp-parse-token-buffer st))
        (loc-buf (elisp-parse-location-buffer st))
        )    
    (mutable-vector-append! tok-buf token)
    (mutable-vector-append! loc-buf (source-file-line loc))
    (mutable-vector-append! loc-buf (source-file-column loc))
    ))


(define parse-literal
  (make<elisp-parse-monad>
   (lambda (st start-loc token) token)))


(define (%unmatched-close st start-loc token)
  (elisp-parse-error
   "unmatched close bracket"
   (parser-state-get-location st)
   token 'expecting (elisp-parse-close-bracket st)
   ))


(define (%unbalanced-eof st start-loc token)
  (elisp-parse-error
   "end of file reached with unbalanced parentheses"
   (parser-state-get-location st)
   'depth (elisp-parse-depth-counter st)
   ))


(define (%unbalanced-bracket-error st loc)
  (elisp-parse-error
   "unbalanced expression, unexpected close bracket"
   loc 'depth (elisp-parse-depth-counter st)
   ))


(define unbalanced-bracket-error
  (make<elisp-parse-monad>
   (lambda (st loc _) (%unbalanced-bracket-error st loc))))


(define unbalanced-eof (make<elisp-parse-monad> %unbalanced-eof))


(define dot-syntax-error
  (make<elisp-parse-monad>
   (lambda (st loc token)
     (elisp-parse-error "invalid read syntax, dot not part of any form" loc)
     )))


(define parse-close-dot
  (make<elisp-parse-monad>
   (lambda (st start-loc _)
     (let*-values
         (((dot-value) (%elisp-read-loop st)) ;; get next form after the dot
          ((token-type token) (%parse-next-token st)) ;; get closing bracket
          )
       (cond
        ((= token-type token-close-bracket)
         (%pop-token-buffer! st token dot-value)
         )
        (else
         (elisp-parse-error
          "expecting close bracket"
          (parser-state-get-location st)
          )))))))


(define parse-end-of-file
  (make<elisp-parse-monad>
   (lambda (st start-loc eof) eof)))


(define parse-close-bracket
  (make<elisp-parse-monad>
   (lambda (st start-loc close)
     (cond
      ((char=? close (elisp-parse-close-bracket st))
       (let ((result (%pop-token-buffer! st close #f)))
         result
         ))
      (else (%unbalanced-eof st start-loc close))
      ))))


(define (%elisp-parse-many st table)
  (let loop ()
    (let*-values
        (((elem-loc) (parser-state-get-location st))
         ((token-type token) (%parse-next-token st))
         )
      (cond
       ((or (not token-type) (elisp-tokenizer-error-type? token))
        (elisp-parse-error "tokenizer error" elem-loc token)
        )
       (else
        (let*((parser (parse-table-ref table token-type))
              (result
               (let ()
                 (%run-parse-monad st
                  (parser-state-get-location st)
                  token parser
                  )))
              )
          (cond
           ((eq? #t result) (loop))
           ((elisp-parse-error-type? result) result)
           ((= token-eof token-type) result)
           ((= token-dot token-type) result)
           ((= token-close-bracket token-type) result)
           (else
            (%elisp-append-token! st elem-loc result)
            (loop)
            ))))))))


(define parse-open-bracket
  (make<elisp-parse-monad>
   (lambda (st start-loc open)
     (%push-token-buffer! st open)
     (%elisp-parse-many st elisp-form-parse-table)
     )))


(define elisp-syntax-error
  (make<elisp-parse-monad>
   (lambda (st loc _)
     (elisp-parse-error "invalid read syntax" loc)
     )))


(define (parser-lex-integer-base lexer)
  (make<elisp-parse-monad>
   (lambda (st loc _)
     (let*((tokst (elisp-parse-tokenizer-state st))
           (int   (run-lexer tokst (lex (any) lexer)))
           (after (run-lexer tokst (lex-table symbol-char-table)))
           )
       (cond
        ((not after) int)
        (else
         (elisp-parse-error
          "invalid read syntax, integer literal"
          (lexer-state-get-location tokst)
          after
          )))))))


(define parse-hashed-quote
  (make<elisp-parse-monad>
   (lambda (st loc token)
     (let*((tokst (elisp-parse-tokenizer-state st))
           (_     (run-lexer tokst (any))) ;; skip the #\' char
           (_spc  (run-lexer tokst skip-space-chars)) ;; skip spaces
           (loc   (lexer-state-get-location tokst)) ;; save location
           (datum (%elisp-read-loop st)) ;; parse any one form
           )
       (cond
        ((or (symbol? datum) (elisp-form-type? datum))
         (make<elisp-function-ref> loc datum)
         )
        (else (elisp-quote-scheme datum #f))
        )))))


(define parse-self-referential-form
  (make<elisp-parse-monad>
   (lambda (st loc token)
     (let*((tokst (elisp-parse-tokenizer-state st))
           (loc   (lexer-state-get-location tokst))
           (datum (%elisp-read-loop st)) ;; read a form that should be an integer
           )
       (cond
        ((integer? datum)
         (let ((op (run-lexer tokst
                    (char (lambda (c) (or (char=? c #\#) (char=? c #\=))))
                    ))
               )
           (cond
            ((not op)
             (elisp-parse-error
              "invalid read syntax, expecting a form"
              (lexer-state-get-location tokst)
              datum
              ))
            ((char=? op #\#)
             (parse-state-lookup-backref st loc datum)
             )
            ((char=? op #\=)
             (let*((pre-form
                    (parse-state-define-backref
                     st datum (make<elisp-form> #f #f #f #f #f))
                    )
                   (form (%elisp-read-loop st))
                   )
               (cond
                ((elisp-form-type? form)
                 (set!elisp-form-tokens      pre-form (elisp-form-tokens      form))
                 (set!elisp-form-dot-element pre-form (elisp-form-dot-element form))
                 (set!elisp-form-locations   pre-form (elisp-form-locations   form))
                 (set!elisp-form-start-loc   pre-form (elisp-form-start-loc   form))
                 (set!elisp-form-end-loc     pre-form (elisp-form-end-loc     form))
                 pre-form
                 )
                (else (parse-state-define-backref st datum form))
                )))
            (else
             (elisp-parse-error
              "invalid read syntax, expecting a form"
              (lexer-state-get-location tokst)
              )))))
        (else
         (elisp-parse-error
          "invalid read syntax, not an integer back-reference"
          loc datum
          )))))))


(define parse-null-string-symbol
  (make<elisp-parse-monad>
   (lambda (st loc token)
     ;; There is no need for a space after this, any two hash
     ;; characters ## regardless of spacing is considered to be a
     ;; reference to the null string symbol.
     (run-lexer (elisp-parse-tokenizer-state st) (any)) ;; skip the #\# char
     (string->symbol "") ;; empty symbol
     )))


(define hashcode-parse-table
  ;; Characters that have special meaning when immediately after a
  ;; hash character.
  (alist->parse-table
   elisp-syntax-error ;; default
   `(((#\0 . #\9) . ,parse-self-referential-form)
     (#\B         . ,(parser-lex-integer-base binchar))
     (#\b         . ,(parser-lex-integer-base binchar))
     (#\O         . ,(parser-lex-integer-base octchar))
     (#\o         . ,(parser-lex-integer-base octchar))
     (#\x         . ,(parser-lex-integer-base hexchar))
     (#\X         . ,(parser-lex-integer-base hexchar))
     (#\'         . ,parse-hashed-quote)
     (#\#         . ,parse-null-string-symbol)
     )))


(define parse-hashcode
  (make<elisp-parse-monad>
   (lambda (st start-loc token)
     (let ((tokst (elisp-parse-tokenizer-state st)))
       (let*((next-char (run-lexer tokst (look)))
             (next-parser (parse-table-ref hashcode-parse-table next-char))
             )
         (run-elisp-parse-monad st token next-parser)
         )))))


(define (parse-quoted-form constructor op)
  (make<elisp-parse-monad>
   (lambda (st start-loc token)
     (let ((datum (%elisp-read-loop st)))
       (cond
        ((not datum)
         (elisp-parse-error
          "invalid read syntax, expecting"
          start-loc datum
          ))
        (else
         (constructor datum op)
         ))))))


(define (elisp-parse-table on-close on-dot on-eof)
  ;; Construct a parse table with unique behavior specified for the
  ;; close bracket, dot, and eof tokens.
  (alist->parse-table
   parse-literal  on-eof
   `((,token-error         . ,parser-tokenizer-error)
     (,token-eof           . ,on-eof)
     (,token-space         . ,parse-space)
     (,token-open-bracket  . ,parse-open-bracket)
     (,token-close-bracket . ,on-close)
     (,token-dot           . ,on-dot)
     (,token-hashcode      . ,parse-hashcode)
     (,token-quote         . ,(parse-quoted-form elisp-quote-scheme #f))
     (,token-backquote     . ,(parse-quoted-form elisp-quote-scheme #t))
     (,token-unquote       . ,(parse-quoted-form elisp-unquoted-form #f))
     (,token-splice        . ,(parse-quoted-form elisp-unquoted-form #t))
     )))


(define elisp-form-parse-table
  (elisp-parse-table
   parse-close-bracket
   parse-close-dot
   unbalanced-eof
   ))


(define elisp-top-level-parse-table
  (elisp-parse-table
   unbalanced-bracket-error
   dot-syntax-error
   parse-end-of-file
   ))

;;--------------------------------------------------------------------------------------------------

(define (parse-state-define-backref st int-id value)
  (let ((table (elisp-parse-backref-dict st)))
    (cond
     (table (hash-table-set! table int-id value))
     (else
      (let ((table (make-hash-table = default-hash)))
        (set!elisp-parse-backref-dict st table)
        (hash-table-set! table int-id value)
        value
        )))))

(define (parse-state-lookup-backref st loc int-id)
  (let ((found
         (hash-table-ref/default
          (elisp-parse-backref-dict st)
          int-id #f
          )))
    (cond
     ((not found)
      (elisp-parse-error
       "read error, back-reference not found"
       loc int-id
       ))
     (else found)
     )))

;;--------------------------------------------------------------------------------------------------

(define dialect-symbol-table
  (alist->parse-table
   `(((#\0 . #\9) . #t)
     ((#\A . #\Z) . #t)
     ((#\a . #\z) . #t)
     (#\_         . #t)
     (#\+         . #t)
     (#\-         . #t)
     (#\*         . #t)
     (#\/         . #t)
     (#\^         . #t)
     (#\=         . #t)
     (#\$         . #t)
     (#\%         . #t)
     (#\@         . #t)
     (#\!         . #t)
     (#\?         . #t)
     (#\.         . #t)
     )))

(define (select-elisp-dialect! st)
  ;; Assuming the given `<PARSE-STATE-TYPE>` argument applied to this
  ;; procedure is at the very start of a file, GNU Emacs Lisp has a
  ;; rule that the Emacs Lisp dialect can be declared in the first
  ;; comment line of a file using the magic comment syntax for file
  ;; local variables. There is only one dialect option: whether or not
  ;; to use lexical binding. From GNU Emacs version 30 an later issues
  ;; a warning if there is not a magic comment indicating the lexical
  ;; binding mode.
  ;;
  ;; This monad will return `#F` if no magic comment is found. If a
  ;; magic comment is found declaring the `lexical-binding` mode, this
  ;; monad returns one of two symbols:
  ;;
  ;;  - `lexical-binding` when `lexical-binding:t` is declared
  ;;  - `dynamic-binding` when `lexical-binding:nil` is delcared
  ;;
  ;; NOTE that this procedure will affect the cursor position in the
  ;; file.
  ;;------------------------------------------------------------------
  (let*((tokst (elisp-parse-tokenizer-state st))
        (_spc (run-lexer tokst (many whitespace?)))
        (comment?
         (run-lexer
          tokst
          (look (lambda (c) (and (char? c) (char=? c #\;))))))
        )
    (cond
     (comment?
      (let*((delim (run-lexer tokst (scan-for-string 1 "-*-")))
            (sym
             (if delim
                 (run-lexer tokst
                  (lex (many whitespace?)
                       (many1/buffer dialect-symbol-table)))
                 #f))
            (return (lambda (val) (run-lexer tokst (skip-to-next-line)) val))
            )
        (cond
         ((and (string? sym) (string=? sym "lexical-binding"))
          (let*-values
              (((sep) (run-lexer tokst
                       (lex (many whitespace?)
                            (lex-first #\: (many whitespace?)))))
               ((token-type token)
                (if sep
                    (%parse-next-token st)
                    (values #f #f)))
               ((delim)
                (if token
                    (run-lexer tokst
                     (lex-first
                      (scan-for-string 1 "-*-")
                      (skip-to-next-line 1)))
                    #f))
               )
            (cond
             ((not delim) (return #f))
             (else
              (cond
               ((symbol? token)
                (cond
                 ((eq? token 'nil)
                  (return 'dynamic-binding)
                  )
                 (else (return 'lexical-binding))
                 ))
               ((and (integer? token) (= sym 0))
                (return 'dynamic-binding)
                )
               (else (return 'lexical-binding))
               )))))
         (else (return #f))
         )))
     (else #f))
    ))

;;--------------------------------------------------------------------------------------------------

(define (%parse-next-token st)
  (%run-elisp-tokenizer (elisp-parse-tokenizer-state st) elisp-tokenizer)
  )


(define (%elisp-read reset-backrefs table)
  (lambda (st)
    (let*-values
        (((st) (parse-state st))
         ((token-type token) (%parse-next-token st))
         )
      (cond
       (token-type
        (let*((next-parser
               (parse-table-ref table token-type)
               )
              (result
               (%run-parse-monad st
                (parser-state-get-location st)
                token next-parser
                ))
              )
          (set!elisp-parse-backref-dict st #f)
          result
          ))
       (else
        (raise
         (elisp-parse-error
          "invalid read syntax"
          (parser-state-get-location st)
          )))))))


(define %elisp-read-loop
  (%elisp-read #f elisp-form-parse-table)
  )


(define (elisp-read st)
  ;; Read a single form from the given parser state.
  ;;------------------------------------------------------------------
  ((%elisp-read #t elisp-top-level-parse-table) (parse-state st))
  )


(define (elisp-read-all st)
  ;; Read all forms from the given parser state into a single vector
  ;; object.
  ;;------------------------------------------------------------------
  (let ((st (parse-state st)))
    (%push-token-buffer! st #f)
    (%elisp-parse-many st elisp-top-level-parse-table)
    (%pop-token-buffer! st #f #f)
    ))


(define (elisp-read-file-for-each filepath proc)
  ;; Open a file path for reading, begin parsing forms from the file
  ;; using `ELISP-READ`, apply three arguments to `PROC` the current
  ;; filepath, the location, and each parsed form, for every form
  ;; parsed from the file.
  ;;------------------------------------------------------------------
  (call-with-port (open-input-file filepath)
    (lambda (port)
      (let ((st (parse-state port)))
        (lens-set filepath st =>parse-state-filepath*!)
        (let loop ()
          (let*((loc (parser-state-get-location st))
                (form (elisp-read st))
                )
            (cond
             ((eof-object? form) #t)
             ((eq? form #t) (loop))
             ((eq? form #f) #f)
             (else
              (proc filepath loc form)
              (loop)
              ))))))))


(define write-location-form-newline
  ;; Used to display the location, form and a newline, useful for
  ;; writing form elements of stack traces.
  ;;------------------------------------------------------------------
  (case-lambda
    ((form)
     (write-parser-location form)
     (display ": ")
     (write-elisp-form form)
     (newline)
     )
    ((loc form)
     (write-parser-location loc)
     (display ": ")
     (write-elisp-form form)
     (newline)
     )
    ((filename loc form)
     (write-string filename)
     (write-char #\:)
     (write-lexer-location loc)
     (write-string ": ")
     (write-elisp-form form)
     (newline)
     )))


(define write-elisp-form
  ;; Similar to `WRITE-STRING` or `WRITE-CHAR` semantics, takes arguments:
  ;;
  ;; - one required argument the form to write, and
  ;; - one optional output port.
  ;;
  ;; The form can really be any part of the Emacs Lisp AST, so this
  ;; incldues symbols, quoted forms, unquoted forms, and function
  ;; references.
  ;;------------------------------------------------------------------
  (case-lambda
    ((form) (write-elisp-form form (current-output-port)))
    ((form port)
     (define (display-elem elem)
       (cond
        ((symbol? elem) (write elem port))
        ((elisp-form-type? elem) (write-elisp-form elem port))
        ((elisp-quote-scheme-type? elem)
         (if (elisp-backquoted-form? elem)
             (write-char #\` port)
             (write-char #\' port)
             )
         (display-elem (elisp-unquote-scheme elem))
         )
        ((elisp-unquoted-form-type? elem)
         (if elisp-spliced-form?
             (display ",@" port)
             (write-char #\, port)
             )
         (write-elisp-form (elisp-unquoted-get-form elem) port)
         )
        ((elisp-function-ref-type? elem)
         (display "#'" port)
         (display-elem (elisp-function-get-ref elem))
         )
        (else (write elem port))
        )
       )
     (cond
      ((elisp-form-type? form)
       (write-char #\( port)
       (let*((vec (elisp-form-tokens form))
             (len (vector-length vec))
             )
         (let loop ((i 0))
           (cond
            ((< i len)
             (when (> i 0) (write-char #\space port))
             (display-elem (vector-ref vec i))
             (loop (+ 1 i))
             )
            (else #t)
            )))
       (when (elisp-form-dot-element form)
         (display " . " port)
         (display-elem (elisp-form-dot-element form))
         )
       (write-char #\) port)
       )
      (else
       (display-elem form)
       )))))

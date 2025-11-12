(define-library (schemacs data-encoding)
  ;; R7RS-compliant implementation of various RFC 4648 data encodings
  ;; for numbers and bytevectors. Not that this is *encoding only*,
  ;; not decoding. This library can provide some of the functionality
  ;; necessary to implement SRFI-207 "String-notated bytevectors", but
  ;; this library does not implement SRFI-207 itself.
  ;;------------------------------------------------------------------
  (import
    (scheme base)
    (scheme case-lambda)
    (scheme write);;DEBUG
    )
  (export
   encode-number
   ;; Write a number to an output port using `*number-alphabet*`. The
   ;; arguments to this procedure are matched with `case-lambda`
   ;; against the following patterns:
   ;;
   ;;  - `(n)`
   ;;  - `(n  out-port)`
   ;;  - `(n  radix)`
   ;;  - `(n  radix       out-port)`
   ;;  - `(n  group-size  group-delim)`
   ;;  - `(n  radix       group-size  group-delim)`
   ;;  - `(n  radix       group-size  group-delim  out-port)`
   ;;
   ;; What each of the arguments mean:
   ;;
   ;;  - `n` :: is the number to be encoded, must be an integer
   ;;
   ;;  - `radix` :: is the base number to encode, defaulting to 10 if
   ;;    not applied as an argument.  This argument must be larger
   ;;    than 1 and must be smaller than the value returned by
   ;;    `(bytevector-length (*number-alphabet*))`.
   ;;
   ;;  - `group-size` and `group-delim` :: applying a digit grouping
   ;;    size and grouping delimiter (both must be given together, if
   ;;    at all) will write the grouping delimiter every `group-size`
   ;;    number of digits written. This is used (for example) to write
   ;;    commas every three digits to make larger numbers easier to
   ;;    read: `123456789` can be written as `123,456,789` if you pass
   ;;    `3` as `group-size` and `#\,` as group-delim. The group
   ;;    delimiter may be a character or string.
   ;;
   ;;  - `out-port` :: must be an output port to which characters are
   ;;    written. If none is given a new string output port is created
   ;;    with `open-output-string` and it's contents is returned on
   ;;    completion.
   ;;-----------------------------------------------------------------

   encode-data
   ;; Encode a bytevector from a bytevector input port to a bytevector
   ;; output port using `*data-alphabet*` for base-32, or else use
   ;; `*base64-alphabet*`
   ;;-----------------------------------------------------------------

   *number-alphabet*
   ;; ^ a parameter to select the alphabet for encoding numbers,
   ;; defaults to `RFC4648-extended-hexadecimal-lowercase`.
   ;;----------------------------------------------------------------

   *data-alphabet*
   ;; ^ a parameter to select the alphabet for encoding data in
   ;; base-32. Defaults to `RFC4648-base32-lowercase`.
   ;;-----------------------------------------------------------------

   *base64-alphabet*
   ;; ^ a parameter to select the alphabet for encoding Base-64 data,
   ;; defaults to `RFC6468-base64`.
   ;;-----------------------------------------------------------------

   *base64-pad-char*
   ;; ^ a parameter to select the padding character to use at the end
   ;; of Base-64 encoded data strings where the number of encoded bits
   ;; is not an even multiple of 6.
   ;;-----------------------------------------------------------------

   ;; Alphabets compliant with RFC 4648
   ;; =================================
   RFC4648-extended-hexadecimal-lowercase
   RFC4648-extended-hexadecimal-uppercase
   RFC4648-base32-lowercase
   RFC4648-base32-uppercase
   RFC4648-filename-safe-base64
   RFC4648-base64
   )

  (begin

    (define RFC4648-extended-hexadecimal-lowercase
      (string->utf8 "0123456789abcdefghijklmnopqrstuv"))

    (define RFC4648-extended-hexadecimal-uppercase
      (string->utf8 "0123456789ABCDEFGHIJKLMNOPQRSTUV"))

    (define RFC4648-base32-lowercase
      (string->utf8 "abcdefghijklmnopqrstuvwxyz234567"))

    (define RFC4648-base32-uppercase
      (string->utf8 "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"))

    (define RFC4648-filename-safe-base64
      (string->utf8
       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"))

    (define RFC4648-base64
      (string->utf8
       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))

    (define *number-alphabet*
      (make-parameter RFC4648-extended-hexadecimal-lowercase))

    (define *data-alphabet*
      (make-parameter RFC4648-base32-lowercase))

    (define *base64-pad-char*
      (make-parameter #\=))

    (define *base64-alphabet*
      (make-parameter RFC4648-base64))

    ;;--------------------------------------------------------------------------------------------------

    (define (encode-data . args)
      (error "TODO: implement 'encode-data")
      )

    ;;--------------------------------------------------------------------------------------------------

    (define (make-number-encoder write-digit)
      ;; Returns a function that takes a number and a radix value, an
      ;; optional grouping size (typically 3) and a grouping character
      ;; (typically a comma `#\,`), and an optional output port.
      ;;
      ;;   - `write-digit` is a procedure which must take (1) a digit and
      ;;     (2) an output port. If `make-port` is `open-output-string`
      ;;     the integer should be converted to a `CHAR` using
      ;;     `integer->char`.  Otherwise, it can write the integer
      ;;     directly to the bytevector.
      ;;
      ;; Returns a curried procedure, first procedure takes 2 arguments:
      ;;
      ;;  - `group-size` is the number of digits written before a
      ;;    group-delimiting character is written.
      ;;
      ;;  - `group-delim` is a procedure that takes a port and writes a
      ;;    group delimiter to that port.
      ;;
      ;; That procedure returns a procedure that takes 3 arguments:
      ;;
      ;;  - `n` is the number to encode
      ;;  - `radix` is the radix of the encoding (defaults to base-10).
      ;;  - `out-port` is the output port to which to write.
      ;;------------------------------------------------------------------
      (let*((alloc 8)
            (alloc-1 (- alloc 1))
            )
        (lambda (group-size write-group-delim)
          (let*((grp (and group-size (- group-size 1)))
                (write-group-delim
                 (if (and group-size write-group-delim)
                     (lambda (count port)
                       (when (and
                              (> count 0)
                              (= (floor-remainder count group-size) 0))
                         (write-group-delim port)
                         ))
                     (lambda (count port) (values))
                     ))
                (flush-buf
                 (lambda (count out-port buf i)
                   (let loop ((i i) (count count))
                     (cond
                      ((< i alloc) ;; assume `buf` is always of size `alloc`
                       (write-digit (bytevector-u8-ref buf i) out-port)
                       (write-group-delim count out-port)
                       (loop (+ 1 i) (- count 1))
                       )
                      (else count)
                      )))))
            (lambda (n radix out-port)
              (let*((alphabet (*number-alphabet*)))
                (let loop ((count 0) (n n) (buf (make-bytevector alloc #x00)))
                  (let fillbuf ((count count) (n n) (i alloc-1))
                    (let ((r (floor-remainder n radix))
                          (n (floor-quotient  n radix))
                          )
                      (bytevector-u8-set! buf i (bytevector-u8-ref alphabet r))
                      (cond
                       ((= 0 n) (flush-buf count out-port buf i))
                       ((= i 0)
                        (loop (+ 1 count) n (make-bytevector alloc #x00))
                        (flush-buf count out-port buf 0)
                        )
                       (else
                        (fillbuf (+ 1 count) n (- i 1))
                        )))))))))))


    (define (write-digit-char d port) (write-char (integer->char d) port))


    (define (%encode-number n radix out-port)
      (let ((max-radix (bytevector-length (*number-alphabet*))))
        (cond
         ((not (integer? radix)) (error "radix is not an integer" radix))
         ((> radix max-radix)
          (error
           "radix too large for *number-alphabet* paramter"
           radix 'max max-radix
           ))
         ((< radix 2)
          (error "radix must have a minimum value of 2" radix)
          )
         (else
          (((make-number-encoder write-digit-char) #f #f) n radix out-port)
          ))))


    (define (%encode-number-grouped n radix group-size group-delim out-port)
      (cond
       ((not
         (and group-size group-delim
              (integer? group-size)
              (> group-size 0)
              (or (char? group-delim)
                  (and (string? group-delim)
                       (not (string=? "" group-delim))))))
        (%encode-number n radix out-port)
        )
       ((not (or (char? group-delim) (string? group-delim)))
        (error
         "grouping delimiter neither a char nor a string"
         group-delim
         ))
       ((not (and (integer? group-size) (> group-size 0)))
        (error
         "grouping size must be a positive integer"
         group-size
         ))
       (else
        (((make-number-encoder write-digit-char)
          group-size
          (if (char? group-delim)
              (lambda (port) (write-char   group-delim port))
              (lambda (port) (write-string group-delim port)))
          )
         n radix out-port
         ))))


    (define (with-output-string run)
      (let ((out-port (open-output-string)))
        (call-with-port out-port
          (lambda (port) (run port) (get-output-string out-port))
          )))


    (define (%encode-base10 n)
      (with-output-string (lambda (port) (%encode-number n 10 port)))
      )

    (define (%encode-base n radix)
      (with-output-string (lambda (port) (%encode-number n radix port)))
      )

    (define (%encode-base10-grouped n group-size group-delim)
      (with-output-string
       (lambda (out-port)
         (%encode-number-grouped n 10 group-size group-delim out-port)
         )))

    (define (%encode-base-grouped n radix group-size group-delim)
      (with-output-string
       (lambda (port) (%encode-number-grouped n radix group-size group-delim port))
       ))


    (define encode-number
      (case-lambda
        ((n) (%encode-base10 n))
        ((n radix/out-port)
         (cond
          ((output-port? radix/out-port) (%encode-number n 10 radix/out-port))
          (else (%encode-base n radix/out-port))
          ))
        ((n radix/group-size out-port/group-delim)
         (cond
          ((output-port? out-port/group-delim)
           (%encode-number n radix/group-size out-port/group-delim)
           )
          (else
           (%encode-base10-grouped n radix/group-size out-port/group-delim)
           )))
        ((n radix group-size group-delim)
         (%encode-base-grouped n radix group-size group-delim)
         )
        ((n radix group-size group-delim out-port)
         (%encode-number-grouped n radix group-size group-delim out-port)
         )))

    ;;----------------------------------------------------------------
    ))

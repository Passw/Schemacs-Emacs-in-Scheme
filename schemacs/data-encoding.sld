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

  (include "data-encoding.scm")
  )

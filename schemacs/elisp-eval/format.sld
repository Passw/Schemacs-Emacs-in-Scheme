(define-library (schemacs elisp-eval format)
  ;; This library defines the algorithm that replicates the behavior
  ;; of the Emacs Lisp `format` function.
  (import
    (scheme base)
    (scheme read)
    (scheme write)
    (only (schemacs elisp-eval environment) eval-error)
    )
  (export format format-to-port format-count)

  (begin

    (define (format-count fstr)
      ;; Count the number of "%"-delimited interpolation items in the
      ;; format string, which must equal the number of arguments that
      ;; would be passed to the `FORMAT` procedure after the format
      ;; string itself.
      ;;------------------------------------------------------------------
      (call-with-port (open-input-string fstr)
        (lambda (port)
          (let loop ((count 0))
            (let ((c (read-char port)))
              (cond
               ((eof-object? c) count)
               ((eqv? #\% c) (loop (+ 1 count)))
               (else (loop count))
               ))))))

    (define (assert-format-count fstr args)
      (let ((arglen (length args))
            (fmtlen (format-count fstr))
            )
        (cond
         ((= arglen fmtlen) #t)
         (else
          (eval-error
           "not enough arguments for format string"
           fmtlen 'args-count arglen))
         )))

    (define (format-to-port outp fstr . args)
      (assert-format-count fstr args)
      (apply %format-to-port outp fstr args)
      )

    (define (format fstr . args)
      (assert-format-count fstr args)
      (apply %format fstr args)
      )

    (define (%format fstr . args)
      (call-with-port (open-output-string)
        (lambda (outp)
          (apply %format-to-port outp fstr args)
          (get-output-string outp)
          )))

    (define (%format-to-port outp fstr . args)
      ;; Replicate the behavior of the Emacs Lisp `format` API, writing
      ;; the format string `FSTR` as is except at "%"-delimiters where
      ;; each delimiter interpolates the next argument in the `ARGS` list
      ;; using the the `PRINC` representation of the value of each
      ;; argument.
      ;;------------------------------------------------------------------
      (call-with-port (open-input-string fstr)
        (lambda (inp)
          (let loop ((c (read-char inp)) (args args))
            (cond
             ((or (eof-object? c)) outp)
             ((and (eqv? #\% c) (pair? args))
              (let ((op (read-char inp)))
                (cond
                 ((eqv? #\s op) (display (car args) outp))
                 ((eqv? #\S op) (write (car args) outp))
                 ((eqv? #\% op) (write-char #\% outp))
                 ((eof-object? op)
                  (eval-error "format string ends in middle of format specifier"))
                 (else
                  (eval-error "invalid format operation" op))))
              (loop (read-char inp) (cdr args)))
             (else
              (write-char c outp)
              (loop (read-char inp) args)
              ))))))

    ;;----------------------------------------------------------------
    ))

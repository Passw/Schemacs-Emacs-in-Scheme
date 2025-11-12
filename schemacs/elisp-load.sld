(define-library (schemacs elisp-load)
  (import
    (scheme base)
    (scheme case-lambda)
    (only (scheme write)
          display
          write
          )
    (only (schemacs pretty)
          print-to-port
          pretty   print  qstr   line-break
          )
    (only (schemacs lens) view)
    (only (schemacs lens vector)
          mutable-vector-type?
          new-mutable-vector
          mutable-vector-length
          mutable-vector-append!
          =>mvector-index!
          )
    (only (schemacs elisp-eval pretty)
          elisp-print
          )
    (only (schemacs elisp-eval environment)
          new-empty-environment
          *elisp-input-port*
          *elisp-output-port*
          *elisp-error-port*
          )
    (only (schemacs elisp-eval)
          new-environment
          *the-environment*
          eval-iterate-forms
          elisp-eval!
          elisp-load!
          )
    (only (schemacs match) match)
    )

  (export
   *verbose*
   elisp-form-buffer
   current-elisp-form-buffer
   elisp-view-form-buffer
   elisp-load-form-buffer!
   elisp-load!
   *the-environment*
   new-environment
   elisp-view-forms vf
   )

  (begin

    (define *verbose* (make-parameter 0))

    (define current-elisp-form-buffer
      (let ((buffer #f))
        (case-lambda
          (() buffer)
          ((new-buffer) (set! buffer new-buffer) buffer)
          )))


    (define elisp-form-buffer
      (case-lambda
        ((filepath) (elisp-form-buffer (new-mutable-vector 64) filepath))
        ((mutvec filepath)
         (let*((verbose (*verbose*))
               (outport (print-to-port (current-output-port))))
           (eval-iterate-forms (new-empty-environment) filepath
            (lambda (form)
              (mutable-vector-append! mutvec form)
              (when (> verbose 1)
                (pretty outport (print (elisp-print form) (line-break))))
              ))
           (when (> verbose 0)
             (pretty
              (print
               ";;read " (mutable-vector-length mutvec)
               " forms from " (qstr filepath) (line-break)
               )))
           (current-elisp-form-buffer mutvec)
           ))))


    (define elisp-view-form-buffer
      (case-lambda
        ((buf)
         (elisp-view-form-buffer buf 0 (mutable-vector-length buf) #t)
         )
        ((buf index/port)
         (if (integer? index/port)
             (elisp-view-form-buffer buf index/port 1 #t)
             (elisp-view-form-buffer buf 0 (mutable-vector-length buf) index/port))
         )
        ((buf i many/port)
         (if (integer? many/port)
             (elisp-view-form-buffer buf i many/port #t)
             (elisp-view-form-buffer buf i 1 many/port)
             )
         )
        ((buf i many outport)
         (display ";; starting on form ")(write i)(display " view ")(write many)(display " forms\n");;DEBUG
         (let ((top (+ i many))
               (outport (print-to-port outport))
               )
           (let loop ((i i))
             (cond
              ((< i top)
               (pretty outport
                (print
                 (elisp-print (view buf (=>mvector-index! i)))
                 (line-break)))
               (loop (+ 1 i))
               )
              (else (values))
              )))
         )))


    (define elisp-load-form-buffer!
      (case-lambda
        (() (check-current-buffer (lambda (buf) (elisp-load-form-buffer! buf))))
        ((buf) (elisp-load-form-buffer! buf 0 (mutable-vector-length buf)))
        ((buf i) (elisp-load-form-buffer! buf i 1))
        ((buf i many)
         (let ((top (+ i many)))
           (let loop ((i i) (result '()))
             (cond
              ((< i top)
               (loop (+ 1 i) (elisp-eval! (view buf (=>mvector-index! i))))
               )
              (else result)
              ))))))


    (define (check-current-buffer run)
      (let ((buf (current-elisp-form-buffer)))
        (cond
         (buf (run buf))
         (else
          (let ((p (current-error-port)))
            (display ";; current buffer is empty, fill by using `elisp-form-buffer`" p)
            (newline p)
            )))))


    (define elisp-view-forms
      ;; Pretty-print to console output the content of the forms that were
      ;; most recently buffered by calling `ELISP-FORM-BUFFER`.
      ;;------------------------------------------------------------------
      (case-lambda
        (() (check-current-buffer (lambda (buf) (elisp-view-form-buffer buf))))
        ((i) (check-current-buffer (lambda (buf) (elisp-view-form-buffer buf i))))
        ((i many) (check-current-buffer (lambda (buf) (elisp-view-form-buffer buf i many))))
        ))

    (define vf elisp-view-forms)

    ;;----------------------------------------------------------------
    ))

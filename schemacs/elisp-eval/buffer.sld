(define-library (schemacs emacs-lisp buffer)
  (import
    (scheme base)
    (scheme case-lambda)
    (only (schemacs lens) record-unit-lens)
    )
  (only (schemacs hash-table)
        make-hash-table  default-hash
        hash-table-ref/default  hash-table-set!
        )
  (export
   new-buffer
   =>buffer-string-handle*!
   =>buffer-keymap*!
   =>buffer-keymap-state*!
   =>buffer-locals*!
   =>buffer-impl*!
   current-buffer  selected-buffer
   *default-buffer-local-keymap*
   ;;-----------------------------------------------------------------
   new-buffer-table
   *buffer-table*
   messages-buffer
   generate-new-buffer-name
   )
  (begin

    (define-record-type <elisp-buffer-type>
      (make<elisp-buffer> hstr keys kmst impl)
      elisp-buffer-type?
      (hstr  buffer-string-handle  set!buffer-string-handle)
      (keys  buffer-keymap         set!buffer-keymap)
      (kmst  buffer-keymap-state   set!buffer-keymap-state)
      (vars  buffer-locals         set!buffer-locals)
      (impl  buffer-impl           set!buffer-impl)
      )

    (define *default-buffer-local-keymap* (make-parameter #f))
    (define current-buffer  (make-parameter #f))
    (define selected-buffer current-buffer)

    (define new-buffer
      ;; Construct a `<BUFFER-TYPE>`, calling `*IMPL/NEW-BUFFER-VIEW*`
      ;; to construct the actual text buffer. In Gtk back-ends, the
      ;; `GtkTextBuffer` object is constructed and stored in the
      ;; `<BUFFER-TYPE>` that is created by this procedure.
      ;;------------------------------------------------------------------
      (case-lambda
        ((string-handle) (new-buffer handle #f))
        ((string-handle keymap)
         (let*((string-handle (generate-new-buffer-name string-handle))
               (keymap (or keymap (*default-buffer-local-keymap*)))
               (this (make<elisp-buffer> keymap handle (impl/new-buffer)))
               (bt (*buffer-table*))
               )
           (hash-table-set! (buffer-table-hash-table bt) handle this)
           this
           ))))

    (define =>buffer-string-handle*!
      (record-unit-lens  buffer-string-handle  set!buffer-string-handle)
      )

    (define =>buffer-keymap*!
      (record-unit-lens  buffer-keymap  set!buffer-keymap)
      )

    (define =>buffer-keymap-state*!
      (record-unit-lens  buffer-keymap-state  set!buffer-keymap-state)
      )

    (define =>buffer-locals*!
      (record-unit-lens  buffer-locals  set!buffer-locals)
      )

    ;;----------------------------------------------------------------

    (define-record-type <buffer-table-type>
      (make<buffer-table> counter messages hash-table)
      buffer-table-type?
      (counter     buffer-table-counter  set!buffer-table-counter)
      (messages    buffer-table-messages  set!buffer-table-messages)
      (hash-table  buffer-table-hash-table)
      )

    (define (new-buffer-table)
      (make<buffer-table> 0 #f (make-hash-table string=?))
      )

    (define *buffer-table*
      (make-parameter (new-buffer-table))
      )

    (define (buffer-table-next-count! bt)
      (let ((count (+ 1 (buffer-table-counter bt))))
        (set!buffer-table-counter bt count)
        count
        ))

    (define (generate-new-buffer-name name)
      (let*((bt (*buffer-table*))
            (ht (buffer-table-hash-table bt))
            (count (if name 1 (buffer-table-next-count! bt)))
            (name (or name "Buffer"))
            )
        (let loop ((count count) (name name))
          (let ((already-taken (hash-table-ref/default ht name #f)))
            (cond
             (already-taken
              (loop
               (+ 1 count)
               (string-append name "-" (number->string count))
               ))
             (else name)
             )))))

    (define (messages-buffer)
      (let*((bt (*buffer-table*))
            (messages (buffer-table-messages bt))
            )
        (cond
         (messages messages)
         (else
          (let*((messages (new-buffer "*Messages*")))
            (set!buffer-table-messages bt messages)
            messages
            )))))


    ))

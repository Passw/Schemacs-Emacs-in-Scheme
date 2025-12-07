(define-library (schemacs ui text-buffer)
  (import
    (scheme base)
    (scheme case-lambda)
    (schemacs ui text-buffer-impl)
    )
  (export
    new-buffer  buffer-type?  new-style  style-type?
    buffer-length  text-load-port  text-dump-port
    *text-load-buffer-size*
    get-cursor-index  set-cursor-index
    move-cursor-index  set-cursor-position
    index->line-column  get-end-of-line  get-start-of-line
    insert-string  insert-char  copy-string  get-char
    delete-range  delete-from-cursor
    get-default-style  set-default-style
    get-text-style  set-text-style
    get-selection  set-selection
    scan-for-char  scan-for-string
    get-tab-stops  set-tab-stops
    get-writing-direction  set-writing-direction
   )
  (begin
    (define (new-buffer . args) (apply (new-buffer*) args))
    (define (buffer-type? . args) (apply (buffer-type?*) args))
    (define (buffer-length . args) (apply (buffer-length*) args))
    (define (text-load-port . args) (apply (text-load-port*) args))
    (define (text-dump-port . args) (apply (text-dump-port*) args))
    (define (new-style . args) (apply (new-style*) args))
    (define (style-type? . args) (apply (style-type?*) args))
    (define (get-cursor-index . args) (apply (get-cursor-index*) args))
    (define (set-cursor-index . args) (apply (set-cursor-index*) args))
    (define (move-cursor-index . args) (apply (move-cursor-index*) args))
    (define (set-cursor-position . args) (apply (set-cursor-position*) args))
    (define (index->line-column . args) (apply (index->line-column*) args))
    (define (get-end-of-line . args) (apply (get-end-of-line*) args))
    (define (get-start-of-line . args) (apply (get-start-of-line*) args))
    (define (insert-string . args) (apply (insert-string*) args))
    (define (insert-char . args) (apply (insert-char*) args))
    (define (copy-string . args) (apply (copy-string*) args))
    (define (get-char . args) (apply (get-char*) args))
    (define (delete-range . args) (apply (delete-range*) args))
    (define (delete-from-cursor . args) (apply (delete-from-cursor*) args))
    (define (get-default-style . args) (apply (get-default-style*) args))
    (define (set-default-style . args) (apply (set-default-style*) args))
    (define (get-text-style . args) (apply (get-text-style*) args))
    (define (set-text-style . args) (apply (set-text-style*) args))
    (define (get-selection . args) (apply (get-selection*) args))
    (define (set-selection . args) (apply (set-selection*) args))
    (define (scan-for-char . args) (apply (scan-for-char*) args))
    (define (scan-for-string . args) (apply (scan-for-string*) args))

    ;;----------------------------------------------------------------

    (define *text-load-buffer-size* (make-parameter 262144))

    ))

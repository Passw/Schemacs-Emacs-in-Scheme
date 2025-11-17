(define-library (schemacs ui text-buffer-impl)
  ;; The high-level Scheme API for editable text buffers.
  ;;------------------------------------------------------------------
  (import
    (scheme base)
    (scheme case-lambda)
    )
  (export
    new-buffer*  buffer-type?*  new-style*  style-type?*
    buffer-length*  text-load*  text-dump*
    get-cursor-index*  set-cursor-index*
    move-cursor-index*  set-cursor-position*
    index->line-column*  get-end-of-line*  get-start-of-line*
    insert-string*  insert-char*  copy-string*  get-char*
    get-default-style*  set-default-style*
    get-text-style*  set-text-style*
    get-selection*  set-selection*
    scan-for-char*  scan-for-string*
    )
  (begin

    (define buffer-type?*
      ;; Returns `#t` only if the applied argument `BUFFER` is an
      ;; implementation-specific opaque data type used to buffer
      ;; editable text data that can be projected onto a graphical
      ;; display.
      ;;--------------------------------------------------------------
      (make-parameter (lambda (buffer) #f))
      )

    (define new-buffer*
      ;; Create a new implementation-specific text buffer opaque data
      ;; structure.
      ;;--------------------------------------------------------------
      (make-parameter (lambda (buffer) #f)))

    (define style-type?*
      ;; Returns `#t` only if the applied argument `STYLE` is an
      ;; implementation-specific opaque data type used to style text
      ;; data in an implementation-specific opaque data type `BUFFER`
      ;; with which this `STYLE` data can be used must cause the
      ;; predicate in the `buffer-type?*` parameter to return `#t`
      ;; when applied to it that predicate.
      ;;--------------------------------------------------------------
      (make-parameter (lambda (style) #f))
      )

    (define new-style*
      ;; Create a new implementation-specific `style` opaque data
      ;; structure from an association list of properties.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (props)
         (error "`new-style` not defined")
         )))

    (define text-load*
      ;; Using a `FILEPATH` of the same type that would be applied to
      ;; `open-input-file`, open a text file for reading at the
      ;; `FILEPATH` and place it's content into the text buffer at the
      ;; current cursor position.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer filepath)
         (error "`text-load` not defined" filepath)
         )))

    (define text-dump*
      ;; Using a `FILEPATH` of the same type that would be applied to
      ;; `open-output-file`, open a text file for writing at the
      ;; `FILEPATH` and write the content of the buffer to the file.
      ;; The `APPEND` argument indicates whether to open a file for
      ;; appending, or for truncation.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer filepath append)
         (error "`text-dump` not defined" buffer filepath append)
         )))

    (define buffer-length*
      ;; Get the number of characters there are in the given `BUFFER`.
      (make-parameter
       (lambda (buffer)
         (error "`buffer-length` not defined" buffer)
         )))

    (define get-cursor-index*
      ;; Get the current cursor position in the text buffer by it's
      ;; character index.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer)
         (error "`get-cursor-index` not defined" buffer)
         )))

    (define set-cursor-index*
      ;; Set the current cursor position in the text buffer to the
      ;; given character `INDEX`.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer index)
         (error "`set-cursor-index` not defined" buffer index)
         )))

    (define move-cursor-index*
      ;; Set the current cursor position by moving it in the text
      ;; buffer by some `OFFSET` number of characters, negative
      ;; numbers moving the cursor toward the start of the buffer,
      ;; positive numbers moving the cursor toward the end of the
      ;; buffer.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer offset)
         (error "`move-cursor-index` not defined" buffer offset)
         )))

    (define set-cursor-position*
      ;; Set the current cursor position by it's line and column
      ;; number.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer line column)
         (error "`set-cursor-position` not defined" buffer line column)
         )))

    (define index->line-column*
      ;; Convert a character index to a line and column number.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer index)
         (error "`index->line-column` not defined" buffer index)
         )))

    (define get-end-of-line*
      ;; Get the character index of the end of the current line on
      ;; which the cursor is currently placed.
      ;;-----------------------------------------------------------
      (make-parameter
       (lambda (buffer)
         (error "`get-end-of-line` not defined" buffer)
         )))

    (define get-start-of-line*
      ;; Get the character index of the start of the current line on
      ;; which the cursor is currently placed.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer)
         (error "`get-start-of-line` not defined" buffer)
         )))

    (define insert-string*
      ;; Insert a Scheme string into the current buffer at the current
      ;; cursor position using the current style.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer str)
         (error "`insert-string` not defined" buffer str)
         )))

    (define insert-char*
      (make-parameter
       (lambda (buffer ch)
         (error "`insert-char` not defined" buffer ch)
         )))

    (define copy-string*
      ;; Create a new string by copying the characters in the `BUFFER`
      ;; starting on the `START-INDEX` and ending on the character
      ;; just before the `END-INDEX`.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer start-index end-index)
         (error "`copy-string` not defined" buffer start-index end-index)
         )))

    (define get-char*
      ;; Get the character under the cursor.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer)
         (error "`get-char` not defined" buffer)
         )))

    (define get-default-style*
      ;; Copy the current default style setting of the `BUFFER`.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer)
         (error "`get-style` not defined" buffer)
         )))

    (define set-default-style*
      ;; Set the current default `STYLE` setting of the `BUFFER`. The
      ;; `STYLE` must be a value created by `new-style`,
      ;; `get-default-style`, or `get-text-style`.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer style)
         (error "`set-style` not defined")
         )))

    (define get-text-style*
      ;; Get the list of styles for the text in between the given
      ;; `START-INDEX` and `END-INDEX`.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer start-index end-index)
         (error "`copy-style` not defined" buffer start-index end-index)
         )))

    (define set-text-style*
      ;; Get the `STYLES` for the text in between the given
      ;; `START-INDEX` and `END-INDEX`.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer style start-index end-index)
         (error "`change-style` not defined" buffer style start-index end-index)
         )))

    (define get-selection*
      ;; Return a selection, this is a CONS cell containing a start
      ;; index and end index indicating the current working
      ;; selection. If multiple selections are possible, all
      ;; selections are returned.
      (make-parameter
       (lambda (buffer)
         (error "`get-selection` not defined" buffer)
         )))

    (define set-selection*
      ;; Set the `SELECTION` of the text given a `START-INDEX` and
      ;; `END-INDEX`.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer start-index end-index)
         (error "`set-selection` not defined" buffer start-index end-index)
         )))

    (define scan-for-char*
      ;; Move the cursor until a character under the cursor is equal
      ;; (according to `char=?`) to the given char `CH`, or until the
      ;; cursor is equal to no less than the given `INDEX`. The cursor
      ;; will move in the direction of `INDEX`, possibly moving
      ;; backwards toward the start of the buffer if the current
      ;; cursor position is after `INDEX`.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer index ch)
         (error "`scan-for-char` not defined" buffer index ch)
         )))

    (define scan-for-string*
      ;; Similar to `scan-for-char* but stops at the first full match
      ;; of the given string `STR`.
      ;;--------------------------------------------------------------
      (make-parameter
       (lambda (buffer index str)
         (error "`scan-for-string` not defined" buffer index str)
         )))

    ;;----------------------------------------------------------------
    ))

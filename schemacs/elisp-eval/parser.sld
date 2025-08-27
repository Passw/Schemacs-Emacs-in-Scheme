(define-library (schemacs elisp-eval parser)
  (import
    (scheme base)
    (scheme write)
    (scheme case-lambda)
    (only (scheme char)
          char-numeric?  char-alphabetic?  digit-value
          )
    (only (scheme file) open-input-file)
    (only (schemacs lens vector)
          mutable-vector-type?
          new-mutable-vector
          mutable-vector->vector
          mutable-vector-append!
          )
    (only (schemacs lens)
          record-unit-lens  lens  =>view-only-lens
          view  update  lens-set
          )
    (only (schemacs lexer)
          run-lexer  lex-all
          any  char  take  eof
          alist->parse-table
          lex-table  parse-table-ref
          lex  look  lex/buffer
          many1  many1/buffer  either
          many   many/buffer   lex-const  optional
          lex-fold  lex-fold-count    lex-fmap
          lex-brackets  lex-apply  lex-first
          scan-for-string  skip-to-next-line
          lex-trace
          lexer-error  new-lexer-error
          lexer-error-type?
          lexer-state  lexer-state-type?
          source-file-location-type?
          lexer-state-get-location
          write-lexer-location
          source-file-line  source-file-column
          *unicode-max-code-point*
          =>lexer-filepath*!
          )
    (only (schemacs hash-table)
          make-hash-table
          hash-table-ref
          hash-table-set!
          hash-table-ref/default
          default-hash
          ))

  (export
   ;;----------------
   ;; The parsing API
   parse-state ;; this constructs a parser state, similar API to 
   elisp-parse-state-type?
   =>parse-state-filepath*! ;; lens to get or set the current filepath
   elisp-read  ;; read a single Elisp form
   elisp-read-all ;; read all forms into a vector
   elisp-read-file-for-each ;; loop on `ELISP-READ`, apply each parsed form to a procedure
   select-elisp-dialect! ;; check for "-*-lexical-binding:t-*-" magic comment 

   ;;----------------
   ;; Selecting individual tokens
   lexer-state ;; re-export schemacs lexer state constructor
   run-elisp-tokenizer ;; lex the next token from a port
   whitespace?

   ;;----------------
   ;; Reporting
   write-parser-location ;; write the parser location to port
   write-location-form-newline ;; write a form's location, the form, and a newline

   ;;----------------
   ;; Working with parts of the abstract syntax tree (AST)
   get-location ;; get location information from various data types

   make<elisp-function-ref>
   elisp-function-ref-type?
   elisp-function-get-ref
   elisp-function-ref-loc

   ;;----------------------------------------
   ;; Forms
   elisp-form-type?
   square-bracketed-form?
   elisp-form ;; construct an Elisp form
   elisp-form-tokens
   elisp-form-dot-element
   elisp-form-locations
   elisp-form-start-loc
   elisp-form-end-loc
   list->elisp-form ;; consruct an Elisp form from a Scheme list
   elisp-form->list
   elisp-form->vector
   elisp-form-equal? ;; compare Elisp form equality
   elisp-form-length ;; return number of elements in a Elisp form
   write-elisp-form ;; write an Elisp form to a port

   ;;----------------------------------------
   ;; Quoting, quasiquoting, and unquoting
   elisp-quote-scheme-type?  elisp-quote-scheme
   elisp-unquote-scheme  elisp-quote-scheme-equal?
   elisp-unquoted-form  elisp-unquoted-form-type?
   elisp-unquoted-get-form  elisp-unquoted-form-equal?
   elisp-spliced-form?  elisp-backquoted-form?
   )
  (include "parser.scm"))

(define-library (schemacs lexer)
  (import
    (scheme base)
    (scheme case-lambda)
    (only (scheme write) display write)
    (only (srfi 14) char-set? char-set-contains?)
    (only (scheme file) open-input-file)
    (only (schemacs lens) record-unit-lens view update lens-set)
    )
  (export
   lex-all  lex-part  lex-resume  run-lexer

   lexer-monad-type?
   lex  either  many  many1  lex-fold  lex-fold-count  lex-fmap
   lex/buffer  many/buffer  many1/buffer
   lex-put  with-buffer-port
   any  look  take  eof  char  lex-const  char-elem-of
   location  set-location!
   lex-brackets  lex-first  lex-apply  lex-join
   lex-trace  lex-error

   scan-for-string
   skip-to-next-line

   lexer-error
   new-lexer-error
   lexer-error-type?
   =>lexer-error-message*!
   =>lexer-error-irritants*!
   =>lexer-error-location*!
   
   parse-table
   alist->parse-table
   parse-table-type?
   lex-table
   parse-table-ref
   set!parse-table
   =>parse-table-index
   set!alist->parse-table
   parse-table-alist-index-bounds
   parse-table-for-each
   parse-table-min-index
   parse-table-index-bounds
   parse-table-default
   parse-table-on-eof
   parse-table-vector

   make<source-file-location>
   source-file-location-type?
   source-file-line
   source-file-column

   lexer-state  lexer-state-type?  lexer-look-ahead
   lexer-feed-string!  lexer-feed-file!
   =>lexer-line*!  =>lexer-column*!  =>lexer-filepath*!
   =>lexer-port*!  =>lexer-continuation*!
   lexer-state-get-location
   write-lexer-location
   with-lexer-location

   *unicode-max-code-point*
   *unicode-min-code-point*
   )
  
  (include "lexer.scm")
  )
    
    

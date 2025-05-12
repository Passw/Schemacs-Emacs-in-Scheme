(cond-expand

  (mit

   (define loader load)

   (define (finish)
     (disk-save "./schemacs-mitscheme.com")
     (newline)
     (display ";;; Now run the command `mit-scheme --band ./schemacs-mitscheme.com`")
     (newline)
     #t)
   )

  (guile

   (define (loader file)
     (compile-file (string-append "./" file ".sld"))
     )

   (define (finish) #t)
   )

  (gauche

   (define (loader file)
     (load (string-append "./" file ".sld"))
     )
   (define (finish) #t)
   )

  (stklos

   (load-path
    (append
     '("."
       "./rapid"
       "./chibi"
       "./match"
       "./schemacs"
       "./schemacs/lens"
       "./schemacs/editor"
       "./schemacs/elisp-eval"
       )
     (filter
      (lambda (p) (not (string=? p ".")))
      (load-path)
      )))

   (define (loader file)
     (let ((src (string-append "./" file ".sld"))
           (obj (string-append "./" file ".ostk"))
           )
       (display "; compile ") (write src) (newline)
       (compile-file src obj)
       (load obj)
       ))
   (define (finish) #t)
   )

  (else
   (error "this Scheme implementation is not supported")
   ))

;;--------------------------------------------------------------------------------------------------

(define file-list
  (list
   "rapid/assume"
   "rapid/format"
   "rapid/test"
   "chibi/match"
   "slib/common"
   "slib/filename"
   "slib/directory"
   "schemacs/test"
   "schemacs/bitwise"
   "schemacs/string"
   "schemacs/vector"
   "schemacs/hash-table"
   "schemacs/lens"
   "schemacs/cursor"
   "schemacs/lens/vector"
   "schemacs/pretty"
   "schemacs/lens/bin-hash-table"
   "schemacs/lexer"
   "schemacs/editor/command"
   "schemacs/keymap"
   "schemacs/bit-stack"
   "schemacs/elisp-eval/pretty"
   "schemacs/elisp-eval/parser"
   "schemacs/elisp-eval/environment"
   "schemacs/elisp-eval/format"
   "schemacs/editor-impl"
   "schemacs/elisp-eval"
   "schemacs/elisp-load"))

(for-each
 (lambda (file) (loader file))
 file-list
 )

(finish)

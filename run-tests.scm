(import
  (scheme base)
  (scheme write)
  (rename (scheme load) (load scheme/load))
  (only (scheme repl) interaction-environment))

(cond-expand
  (guile
   (define (path str)
     (string-append (getcwd) "/" str))
   )
  (else
   (define (path str) str)
   ))

(cond-expand
  (gambit
   (define (load path . env) (scheme/load path)))
  (else
   (define load scheme/load))
  )

(define test-programs
  (list 
   "./schemacs/lens-tests.scm"
   "./schemacs/lens/vector-tests.scm"
   "./schemacs/lens/bin-hash-table-tests.scm"
   "./schemacs/pretty-tests.scm"
   "./schemacs/keymap-tests.scm"
   "./schemacs/concurrent-tests.scm"
   "./schemacs/cursor-tests.scm"
   "./schemacs/lexer-tests.scm"
   "./schemacs/elisp-eval/environment-tests.scm"
   "./schemacs/elisp-eval/format-tests.scm"
   "./schemacs/elisp-eval/parser-tests.scm"
   "./schemacs/elisp-eval-tests.scm"
   ))

(for-each
 (lambda (filepath)
   (display "----------------------------------------\n")
   (display (path filepath))
   (newline)
   (load (path filepath) (interaction-environment))
   )
 test-programs)

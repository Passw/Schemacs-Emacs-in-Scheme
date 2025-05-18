(define-library (schemacs hash-table)

  ;; This library exists to provide a uniform interface to common APIs
  ;; that might be defined in different places depending on which
  ;; Scheme implementation is running this code. APIs exported here
  ;; can be imported exactly once per environment elsehwere in this
  ;; program without having to write a `COND-EXPAND` statement
  ;; everywhere one of these APIs are used.

  ;; IMPORTS
  (import
    (scheme base)
    )
  (cond-expand
    ((or guile gambit)
     (import (srfi 69))
     )
    ((or mit stklos chibi)
     (import (srfi 125))
     (import
       (only (srfi 128)
             default-hash
             string-hash
             ))
     )
    (gauche
     (import
       (scheme case-lambda)
       (only (srfi 69) alist->hash-table)
       (except (srfi 125) alist->hash-table)
       (only (srfi 128)
             default-hash
             string-hash
             ))
     )
    (chez
     (import
       (srfi 125)
       (only (srfi 128)
             default-hash
             string-hash
             ))
     )
    (else
     (cond-expand
       ((library (srfi 125))
        (import (srfi 125))
        ))
     (cond-expand
       ((library (srfi 128))
        (import
          (only (srfi 128)
                default-hash
                string-hash
                ))
        )))
    )

  ;; EXPORTS
  (export
   alist->hash-table
   hash-table->alist
   hash-table-copy
   hash-table-delete!
   hash-table-empty?
   hash-table-fold
   hash-table-ref
   hash-table-ref/default
   hash-table-set!
   hash-table-size
   hash-table-update!/default
   hash-table-for-each
   hash-table?
   make-hash-table
   default-hash  string-hash
   )

  (begin

    (cond-expand

      ((or guile gambit)

       (define (hash-table-empty? ht)
         (= 0 (hash-table-size ht))
         )

       (define default-hash hash)

       (define (hash-table-for-each proc ht)
         (hash-table-walk ht proc)
         )

       )

     (else)
     )))

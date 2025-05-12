(define-library (gypsum hash-table)
  ;; TODO: Remove this library some day, when SRFI-125 becomes more
  ;; ubiquitous, or perhaps just when Guile finally supports the
  ;; SRFI-125 API.
  ;;
  ;; Because hash tables are so ubiquitous in computer programming
  ;; these days, this library provides an implementation of SRFI-125,
  ;; but tries to re-use as much as possible of whatever existing hash
  ;; table implementation is provided by the Scheme implementations
  ;; that run this code.

  ;; IMPORTS
  (import
    (scheme base)
    )
  (cond-expand
    ((or guile gambit)
     (import (srfi  69))
     (begin
       (define default-hash hash)
       ))
    ((or stklos chibi) (import (srfi 125)))
    (mit
     (import (srfi 125))
     (import (only (srfi 128) default-hash))
     )
    (gauche
     (import
       (scheme case-lambda)
       (only (srfi 69) alist->hash-table)
       (except (srfi 125) alist->hash-table)
       (only (srfi 128) default-hash)
       )
     )
    (else
     (cond-expand
       ((library (srfi 125))
        (import (srfi 125))
        ))
     (cond-expand
       ((library (srfi 128))
        (import (only (srfi 128) default-hash))
        ))
     )
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
   hash-table-walk
   hash-table?
   make-hash-table
   default-hash  string-hash
   )

  (begin
    (cond-expand
      ((or guile-3 gambit)
       (define (hash-table-empty? ht)
         (= 0 (hash-table-size ht)))
       )

      (else)))
  )

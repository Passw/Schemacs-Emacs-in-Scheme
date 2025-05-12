(define-library (schemacs string)
  (cond-expand
    ((or guile (library (srfi 13)))
     (import (only (srfi 13) string-fold))
     )
    ((or mit (library (srfi 140)))
     (import (only (srfi 140) string-fold))
     )
    (else
     (begin
       (define (string-fold proc accum instr)
         (let ((len (string-length instr)))
           (let loop ((i 0) (accum accum))
             (cond
              ((< i len)
               (loop (+ 1 i) (proc (string-ref instr i) accum))
               )
              (else accum)
              ))))
       )
     ))
  (export
   string-fold
   ))

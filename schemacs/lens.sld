(define-library (schemacs lens)
  (import
    (scheme base)
    (scheme write)
    (scheme case-lambda)
    (only (schemacs vector) vector-fold)
    (only (schemacs hash-table)
          hash-table-empty?  hash-table?  default-hash
          make-hash-table   alist->hash-table   hash-table->alist
          hash-table-size   hash-table-set!   hash-table-delete!
          hash-table-fold   hash-table-ref/default
          hash-table-update!/default
          )
    )

  (cond-expand
    (guile
     (import
       (only (srfi srfi-9 gnu) set-record-type-printer!)
       (only (srfi 28) format)
       (only (srfi 111) unbox set-box!))
     )
    (stklos
     (import (only (srfi 111) unbox set-box!)))
    (else
     ;; Guile does not seem to recognize the (library ...) clause in
     ;; "cond-expand" statements used in "define-library" statements.
     (cond-expand
       ((library (srfi 111))
        (import 
          (only (srfi 111) unbox set-box!))
        )
       (else))

     (cond-expand
       ((library (srfi 28))
        (import (only (srfi 28) format)))
       (mit)
       (else
        (import (only (rapid format) format)))
       )
     ))

  (export
   ;; -------------------- Main API --------------------
   lens  view  lens-set  lens-swap  update&view  update&  update
   lens-set!  lens-swap!  endo-set  endo-update  endo-view
   lens-compose  endo

   ;; ------------------- Unit lenses ------------------
   ;; When defining your own lenses
   ;; use unit-lens or record-unit-lens
   %unit-lens-type?  %lens-type?  unit-lens
   unit-lens-view  unit-lens-update  unit-lens-set  unit-lens-swap
   record-unit-lens
   unit-lens-getter  unit-lens-setter  unit-lens-updater  unit-lens->expr
   default-unit-lens-setter  default-unit-lens-updater

   ;; ------------- Lenses introspection ---------------
   ;; Inspecting properties of lenses.
   lens-type?  vector->lens  lens->vector  lens-view  lens-unit-count

   ;; -- Extending lens indexing with other data types --
   ;; The `view` and `update&view` procedures can take integer, string, or
   ;; symbol values and automatically construct lenses out of these
   ;; for inspecting vectors and hash tables. If you have your own
   ;; record data which could also be indexed by integers, strings, or
   ;; symbols, you can use these procedures at the top-level of your
   ;; program (or in any statement that evaluates at load time), so
   ;; that `view`-ing or `update&view`-ing on constants will be able to
   ;; inspect structures of the types you constructed.
   declare-rule/index->lens
   get-rule-declaration/index->lens

   ;; ----------- Useful, pre-defined lenses -----------

   ;; Lists, and association Lists
   =>car  =>head  =>cdr
   =>find-tail  =>find  =>bring
   =>assoc-by  =>assoc  =>assv  =>assq

   ;; Vectors
   =>vector-index*! =>vector-index!
   *default-vector-constructor*  *default-vector-copier*

   ;; Hash tables
   =>hash-key*!  =>hash-key!  *default-hash-table-constructor*

   ;; Utility lenses
   =>self  =>const  =>true  =>false
   =>guard  =>on-update  =>canonical  =>encapsulate
   =>view-only-lens

   ;; Debugging lenses
   =>trace
   )

  (cond-expand
    ((or gambit guile stklos chibi)
     (export =>box)
     )
    (else))

  (cond-expand
    (stklos
     (include "./schemacs/lens.scm"))
    (guile
     ;; Bug in Guile R7RS: the "else" statement below is not triggered
     ;; even though none of the other conditions in this "cond-expand"
     ;; statement are met, so I must specify this Guile condition
     ;; explicitly or else the "include" statement does not get
     ;; evaluated.
     (include "lens.scm"))
    (else
     (include "lens.scm"))
    ))

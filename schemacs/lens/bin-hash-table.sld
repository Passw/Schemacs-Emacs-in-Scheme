(define-library (schemacs lens bin-hash-table)
  (import
    (scheme base)
    (scheme case-lambda)
    (scheme write)
    (only (scheme write) display)
    (only (schemacs lens)
          lens record-unit-lens view lens-set
          =>self =>hash-key! =>on-update
          =>canonical =>encapsulate)
    (only (schemacs pretty)
           pretty print bracketed newline-indent line-break)
    (only (schemacs hash-table) ; Standard hash tables
          hash-table-empty?
          default-hash make-hash-table alist->hash-table hash-table->alist
          hash-table-size hash-table-copy hash-table-for-each
          hash-table-update!/default hash-table-set!
          hash-table-fold hash-table?
          )
    )

  (export
   *bin-hash-table-init-size*
   *default-make-hash-table*
   *default-key-hash*
   make<bin-hash-table>
   bin-hash-table-type?
   empty-bin-hash-table
   bin-hash-table
   alist->bin-hash-table
   bin-hash-table->alist
   bin-hash-table-empty?
   bin-hash-table-size
   =>bin-hash-table-store-size!
   =>bin-hash-table-hash*!
   =>bin-hash-table-hash!
   =>bin-hash-key!
   bin-hash-table-fold
   hash-table-copy-with
   bin-hash-table-copy
   bin-hash-table-print
   )

  (include "bin-hash-table.scm"))

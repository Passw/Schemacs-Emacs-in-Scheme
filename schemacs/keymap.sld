(define-library (gypsum keymap)
  (import
    (scheme base)
    (scheme char)
    (scheme write)
    (scheme lazy)
    (scheme case-lambda)
    (only (gypsum lens)
          lens view update update&view lens-set
          unit-lens record-unit-lens
          =>self  =>trace  =>hash-key!
          =>on-update  =>canonical
          =>head  =>encapsulate
          =>trace
          )
    (only (gypsum pretty)
          pretty print qstr repeat join-by bracketed
          indent-by newline-indent line-break)
    (only (gypsum lens vector) vector-copy-with)
    (only (gypsum lens bin-hash-table)
          make<bin-hash-table>
          empty-bin-hash-table
          bin-hash-table-size
          bin-hash-table-empty?
          =>bin-hash-table-store-size!
          =>bin-hash-table-hash*!
          =>bin-hash-table-hash!
          =>bin-hash-key!
          hash-table-copy-with
          bin-hash-table-copy
          bin-hash-table->alist)
    (only (gypsum editor command) command-type? command-procedure)
    (only (srfi 1) fold concatenate find)
    (only (gypsum string) string-fold)
    (only (gypsum bitwise) bitwise-ior bitwise-and)
    (only (gypsum hash-table)
          hash-table-empty?
          string-hash alist->hash-table hash-table->alist
          hash-table? hash-table-size make-hash-table
          hash-table-fold hash-table-ref/default
          hash-table-copy hash-table-walk hash-table-set!)
    )

  (cond-expand
    (guile-3
     (import
       (only (srfi srfi-9 gnu) set-record-type-printer!)))
    (else))

  (cond-expand
    (gauche
     (import
       (only (srfi 114)
             eq-comparator
             eqv-comparator
             equal-comparator
             comparator-comparison-procedure
             comparator-hash-function
             ))
     )
    (else)
    )

  (export
   char-table
   char-table-type?
   empty-char-table
   char-table-empty?
   char-table-size
   char-table-view
   char-table-set!
   char-table-update!
   char-table->alist
   =>char-table-char!
   char-table-copy

   alist->keymap-layer
   keymap-index-type?
   keymap-index
   keymap-index-append
   keymap-index->list
   string->keymap-index
   reverse-list->keymap-index
   keymap-index->ascii
   =>kbd! =>keymap-layer-index!
   keymap-index-print
   keymap-index-to-char
   mod-index char-index next-index
   ctrl-bit meta-bit super-bit hyper-bit alt-bit
   modifier->integer

   keymap-layer-type?
   keymap-layer
   keymap-layer->alist
   keymap-layer-copy
   keymap-layer-print
   keymap-layer-action
   map-key
   keymap-layer-lookup
   keymap-layer-ref
   keymap-layer-update!
   prefer-new-bindings
   prefer-old-bindings
   *unicode-max-code-point*
   *unicode-min-code-point*

   make<keymap-index-predicate>
   keymap-index-predicate-type?
   new-self-insert-keymap-layer
   apply-keymap-index-predicate

   keymap-type?
   =>keymap-layers*!
   =>keymap-label!
   =>keymap-top-layer!
   keymap keymap-lookup keymap->layers-list
   keymap-print

   modal-lookup-state-type?
   new-modal-lookup-state
   modal-lookup-state-key-index
   modal-lookup-state-keymap
   modal-lookup-state-step!
   )
  (include "keymap.scm"))

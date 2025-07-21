(define-library (schemacs elisp-eval environment)
  ;; This library defines functions and record data types that are
  ;; essential to the operation of an Emacs Lisp interpreter, but the
  ;; interpreter itself is not defined here. The interpreter includes
  ;; many built-in procedures which are defined across a few different
  ;; Scheme libraries, all of which import this library. The
  ;; interpreter itself is defined in the `(SCHEMACS ELISP-EVAL)`
  ;; library.
  (import
    (scheme base)
    (scheme eval)
    (scheme cxr)
    (scheme case-lambda)
    (only (scheme write) display write)
    (only (schemacs editor command) command-type? command-procedure)
    (only (schemacs bit-stack)
          new-bit-stack
          bit-stack-push!
          bit-stack-pop!
          bit-stack-look)
    (only (schemacs hash-table)
          hash-table?
          hash-table-empty?
          make-hash-table
          alist->hash-table
          hash-table->alist
          hash-table-set!
          hash-table-delete!
          hash-table-ref/default
          hash-table-for-each
          string-hash
          )
    (only (schemacs lens)
          unit-lens  record-unit-lens  lens
          lens-set  lens-set!  endo-view  view
          update  endo-update  update&view
          *default-hash-table-constructor*
          default-unit-lens-updater  default-unit-lens-setter
          =>car  =>canonical  =>view-only-lens
          =>hash-key!  =>hash-key*!
          )
    (only (schemacs pretty) pretty print bracketed line-break qstr)
    (only (schemacs lens vector) mutable-vector-type?)
    (only (schemacs lexer) source-file-location-type?)
    (only (schemacs elisp-eval parser)
          elisp-quote-scheme-type?
          write-parser-location
          get-location
          elisp-form-type?
          elisp-form->list
          )
    (only (schemacs match) match)
    )

  (export
   ;; Converting data between Scheme and Elisp
   scheme->elisp  elisp->scheme
   pure  pure*  pure*-typed  pure*-numbers  pure-raw

   ;; Type predicates
   type-predicate    elisp-null?  elisp-pair?  elisp-list?
   elisp-string?  elisp-number?  elisp-integer?  elisp-float?
   elisp-procedure?  elisp-symbol?

   ;; Emacs Lisp constant symbols
   nil t

   ;;----------------------------------------
   ;; Environment objects
   new-empty-environment
   elisp-environment-type?
   env-push-trace!  env-pop-trace!  env-trace!
   env-push-new-elstkfrm!
   env-pop-elstkfrm!
   env-resolve-function
   =>env-symbol!
   env-intern!    ;; implements the ELisp `intern` function
   env-setq-bind! ;; implements the ELisp `setq` macro
   env-alist-defines!
   env-reset-stack!
   *default-obarray-size*
   *elisp-input-port*
   *elisp-output-port*
   *elisp-error-port*

   =>env-obarray-key!
   =>env-lexstack*!
   =>env-obarray*!
   =>env-lexical-mode?!
   =>env-stack-trace*!

   ;;----------------------------------------
   ;; Symbol objects
   sym-type?  sym-name  new-symbol  new-symbol-value
   =>sym-value*!  =>sym-function*!  =>sym-plist*!
   =>sym-name  =>sym-value!  =>sym-function!  =>sym-plist!
   ensure-string  symbol/string?  any-symbol?
   sym-name-equal?

   ;;----------------------------------------
   ;; Function objects
   lambda-type?  new-lambda  lambda-copy-into!
   =>lambda-kind!  =>lambda-args!
   =>lambda-optargs!  =>lambda-rest!
   =>lambda-docstring!  =>lambda-declares!
   =>lambda-lexenv!  =>lambda-body!
   =>lambda-declares*!  =>lambda-interactive*!
   =>lambda-body*!  =>lambda-kind*!
   =>lambda-docstring*!  =>lambda-location*!

   ;;----------------------------------------
   ;; Macro objects
   make<macro>  macro-type?
   macro-procedure  elisp-void-macro
   make<syntax> syntax-type? syntax-eval elisp-void-syntax

   ;;----------------------------------------
   ;; Error objects
   raise-error-impl*  eval-raise  eval-error
   make<elisp-eval-error>
   elisp-eval-error-type?
   elisp-eval-error-equal?
   =>elisp-eval-error-message
   =>elisp-eval-error-irritants
   =>elisp-eval-error-stack-trace
   write-elisp-eval-error

   ;;----------------------------------------
   ;; Stack frames
   new-elstkfrm  stack-lookup
   elstkfrm-from-args
   elstkfrm-sym-intern!

   ;;----------------------------------------
   ;; Stack traces
   new-stack-trace-frame  stack-trace-frame-type?
   =>stack-trace-location*!  =>stack-trace-symbol*!
   =>stack-trace-func*!  =>stack-trace-frames*!
   =>env-trace-location*!  =>env-trace-symbol*!
   =>env-trace-func*!  =>env-trace-frames*!
   write-stack-trace-frame
   print-stack-frame  print-all-stack-frames

   ;; A whole stack trace
   env-get-stack-trace
   elisp-stack-trace-type?
   elisp-stack-trace->vector
   elisp-stack-trace-ref
   write-elisp-stack-trace
   env-get-location
   )

  (include "environment.scm")
  )

(define-library (schemacs elisp-eval)
  ;; This library defines functions and record data types that are
  ;; essential to the operation of an Emacs Lisp interpreter. When
  ;; evaluating Emacs Lisp code, it is first translated to Scheme
  ;; code, or rather a list of Scheme forms. These Scheme forms are
  ;; then evaluated using the Scheme `EVAL` function. The APIs in this
  ;; library are always available to be used by `EVAL` when evaluating
  ;; Scheme forms that have been translated from Emacs Lisp. although
  ;; the actual Scheme `ENVIRONMENT` object used to `EVAL` Emacs Lisp
  ;; is defined in the `(SCHEMACS ELISP)` library.
  (import
    (scheme base)
    (scheme cxr)
    (scheme case-lambda)
    (only (scheme file) open-input-file)
    (only (scheme write) display write)
    (only (schemacs editor command) command-type? command-procedure)
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
          default-hash
          string-hash
          )
    (only (schemacs pretty) pretty print line-break)
    (only (schemacs lens)
          unit-lens  record-unit-lens  lens
          lens-set  lens-set!  endo-view  view
          update  endo-update  update&view
          *default-hash-table-constructor*
          default-unit-lens-updater  default-unit-lens-setter
          =>canonical  =>view-only-lens  =>encapsulate
          =>hash-key!  =>hash-key*!  =>find
          )
    (only (schemacs lens vector) mutable-vector-type?)
    (only (schemacs cursor)
          new-cursor  cursor-ref  cursor-step!
          cursor-end?  cursor-type?
          cursor-collect-list  new-cursor-if-iterable
          )
    (prefix (schemacs editor-impl) *impl/)
    (only (schemacs elisp-eval parser)
          elisp-read  select-elisp-dialect!
          parse-state  =>parse-state-filepath*!
          elisp-form->list  elisp-form->vector
          write-parser-location  write-elisp-form
          elisp-parse-state-type?  elisp-function-ref-type?
          elisp-quote-scheme  elisp-unquote-scheme
          elisp-quote-scheme-type?  elisp-backquoted-form?
          elisp-unquoted-form-type?  elisp-spliced-form?
          elisp-unquoted-get-form
          elisp-form-type?  square-bracketed-form?
          elisp-form-start-loc  elisp-function-get-ref
          elisp-form-tokens
          )
    (only (schemacs elisp-eval environment)
          pure  pure*  pure*-typed  pure*-numbers  pure-raw
          scheme->elisp  elisp->scheme
          type-predicate    elisp-null?  elisp-pair?  elisp-list?
          elisp-string?  elisp-number?  elisp-integer?  elisp-float?
          elisp-procedure?  elisp-symbol?
          new-empty-environment   elisp-environment-type?  env-alist-defines!
          env-with-elstkfrm!  env-trace!  
          env-resolve-function   env-intern!   env-setq-bind!  env-reset-stack!
          elstkfrm-from-args   elstkfrm-sym-intern!
          *elisp-input-port*  *elisp-output-port*  *elisp-error-port*
          *default-obarray-size*  *max-lisp-eval-depth*
          =>env-obarray-key!   =>env-symbol!  =>env-trace-max*!
          =>env-stack-trace*!  =>stack-trace-location*!  =>env-trace-depth*!
          =>env-lexstack*!  =>env-obarray*!  =>env-lexical-mode?!
          sym-type?  sym-name  new-symbol  new-symbol-value
          =>sym-name  =>sym-value*!  =>sym-function*!  =>sym-plist*!
          =>sym-value!  =>sym-function!  =>sym-plist!
          ensure-string  symbol/string?  any-symbol?
          nil  t
          lambda-type?  new-lambda  lambda-copy-into!
          =>lambda-kind!  =>lambda-args!
          =>lambda-optargs!  =>lambda-rest!
          =>lambda-docstring!  =>lambda-declares!
          =>lambda-lexenv!  =>lambda-body!
          =>lambda-declares*!  =>lambda-interactive*!
          =>lambda-body*!  =>lambda-kind*!
          =>lambda-docstring*!  =>lambda-location*!
          env-get-location
          make<syntax>  syntax-type?  macro-type?
          syntax-eval  elisp-void-syntax
          elisp-eval-error-type?  raise-error-impl*
          =>elisp-eval-error-message
          =>elisp-eval-error-irritants
          =>elisp-eval-error-stack-trace
          eval-raise  eval-error
          env-get-stack-trace  write-elisp-eval-error
          print-stack-frame  print-all-stack-frames
          )
    (only (schemacs elisp-eval format) format format-to-port)
    (only (schemacs keymap)
          keymap  keymap-type?  keymap-layer
          =>keymap-label!
          =>keymap-layer-index!
          =>keymap-top-layer!
          )
    (only (schemacs match) match)
    )

  (export
   ;; Initializing environments
   new-environment  elisp-reset-init-env!

   ;; The interpreter
   elisp-eval!  elisp-load!
   eval-iterate-forms  elisp-eval-reset-stack!

   =>elisp-symbol!
     ;; ^ lens on a string that looks-up a sym-type object by name in
     ;; *the-environment*, which is very useful for debugging.

   ;; Re-exporting symbols from (SCHEMACS ELISP-EVAL ENVIRONMENT):
   ;;------------------------------------------------------------

   ;; Converting data between Scheme and Elisp
   scheme->elisp  elisp->scheme  elisp-null?  pure  pure*

   ;; Environment objects
   elisp-environment-type?  elisp-intern!   =>env-obarray-key!
   =>env-symbol!

   *the-environment*
   *elisp-init-env*
   *elisp-input-port*
   *elisp-output-port*
   *elisp-error-port*
   
   ;; Symbol objects
   sym-type?  new-symbol
   =>sym-name  =>sym-value!  =>sym-function!  =>sym-plist!
   =>sym-value*!
   nil  t

   ;; Macro objects
   make<macro>  macro-procedure

   ;; Error handling
   handle-scheme-exceptions*
   elisp-eval-error-type?
   =>elisp-eval-error-message
   =>elisp-eval-error-irritants
   new-elisp-raise-impl
   elisp-write-stack-frames
   elisp-debug-write-obarray

   ;; Debugging
   elisp-debug-eval  debugger-state-type?
   elisp-debug-step!  elisp-debug-view-step!
   elisp-debug-step-value!
   elisp-debug-continue!  elisp-debug-skip! ;; <- FIXME: these two don't work correctly yet
   elisp-debug-show-form  elisp-debug-show-result
   elisp-debug-set-break!
   elisp-debug-clear-break!
   elisp-debug-show-breaks
   =>debugger-breakpoints*!
   =>debugger-current-form*!
   =>debugger-last-value*!
   )

  (include "elisp-eval.scm")
  )

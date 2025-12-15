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
          hash-table-keys
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
          write-location-form-newline
          elisp-parse-state-type?  elisp-function-ref-type?
          elisp-quote-scheme  elisp-unquote-scheme
          elisp-quote-scheme-type?  elisp-backquoted-form?
          elisp-unquoted-form-type?  elisp-spliced-form?
          elisp-unquoted-get-form
          elisp-form-type?  square-bracketed-form?
          elisp-form-start-loc  elisp-function-get-ref
          elisp-form-tokens  elisp-form-locations
          list->elisp-form  elisp-form-gather-symbols
          )
    (only (schemacs elisp-eval environment)
          pure  pure*  pure*-typed  pure*-numbers  pure-raw
          scheme->elisp  elisp->scheme
          type-predicate    elisp-null?  elisp-pair?  elisp-list?
          elisp-string?  elisp-number?  elisp-integer?  elisp-float?
          elisp-procedure?  elisp-symbol?
          new-empty-environment   elisp-environment-type?  env-alist-defines!
          env-with-elstkfrm!  env-trace!  
          env-resolve-function  env-reset-stack!  env-reset-obarray!
          env-intern!  env-setq-bind!  env-lex-sym-lookup
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
          elisp-eval-error-type?  make<elisp-eval-error>
          raise-error-impl*
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

  (begin
    
    (define *the-environment*
      (make-parameter
       (new-empty-environment *default-obarray-size* "env-main")
       ))

    (define (elisp-write-stack-frames)
      (let ((st (*the-environment*)))
        (pretty
         (print
          (print-stack-frame st)
          "==== stack frames ================"
          (line-break)
          (print-all-stack-frames st)
          ))))

    (define =>elisp-symbol!
      ;; This lens looks-up a symbol in the environment, including the
      ;; stack, so it can operate on an environment returned by a
      ;; breakpoint within a debugger and will resolve the same symbols
      ;; that would be resolved by the chain of function evaluation up to
      ;; the breakpoint.
      ;;------------------------------------------------------------------
      (let ((getter
             (lambda (sym)
               (view
                (*the-environment*)
                (=>env-symbol! (symbol->string sym)))))
            (updater
             (lambda (updater sym)
               (update&view
                updater
                (*the-environment*)
                (=>env-symbol! (symbol->string sym)))
               )))
        (unit-lens
         getter
         (default-unit-lens-setter updater)
         updater
         '=>elisp-symbol!
         )))

    (define (elisp-intern! . assocs)
      ;; This procedure is exported, so mostly used by users of this
      ;; library to directly update an Emacs Lisp environment from within
      ;; a Scheme procedure without having to use `ELISP-EVAL!`. It takes
      ;; an arbitrary number of pair arguments (cons cells) associating a
      ;; symbol or string to an arbitrary value. This procedure then
      ;; creates a symbol for each value and stores the value into the
      ;; symbol. The value will be stored into the `=>SYM-VALUE!` field of
      ;; a `<SYM-TYPE>` object unless it is a `<LAMBDA-TYPE>`, a Scheme
      ;; procedure, or a `<MATCHER-MONAD-TYPE>`, in which case it is
      ;; stored into the `=>SYM-FUNCTION!`
      ;;------------------------------------------------------------------
      (let ((st (*the-environment*)))
        (for-each
         (lambda (pair)
           (let*-values
               (((name) (ensure-string (car pair)))
                ((st sym)
                 (update&view
                  (lambda (sym)
                    (cond
                     (sym (values sym sym))
                     (else (let ((sym (new-symbol name))) (values sym sym)))
                     ))
                  st (=>env-obarray-key! name)))
                ((val) (cdr pair))
                )
             (cond
              ((or (lambda-type? val)
                   (procedure? val)
                   (macro-type? val)
                   )
               (lens-set val sym =>sym-function*!)
               )
              (else (lens-set val sym =>sym-value*!))
              )))
         assocs
         )))

    (define new-elisp-raise-impl
      ;; This is a constructor for a procedure that raises an
      ;; exception when an exception occurs in the Emacs Lisp
      ;; evaluator. The procedure returned by this constructor should
      ;; be used to parameterize the `RAISE-ERROR-IMPL*` API from the
      ;; `(SCHEMACS ELISP-EVAL ENVIRONMENT)` library.
      ;;
      ;; The implementation returned by this procedure takes an error
      ;; object to be raised, it then updates the error object with a
      ;; stack trace taken from the `ENV` argument applied to this
      ;; constructor. The updated error object is then applied to the
      ;; `HALT-EVAL` procedure applied to this constructor. The
      ;; `HALT-EVAL` procedure is typically constructed with `CALL/CC`
      ;; which forces computation to resume at the point in the
      ;; program where `CALL/CC` was applied, returning the error
      ;; object.
      ;;
      ;; Use this constructor when you want to modify the behavior of
      ;; the Emacs Lisp evaluator with regard to how it throws
      ;; exceptions.
      ;;------------------------------------------------------------------
      (case-lambda
        ((env halt-eval)
         (new-elisp-raise-impl env halt-eval (current-output-port))
         )
        ((env halt-eval port)
         (let ((env (or env (*the-environment*))))
           (lambda (err-obj)
             (when (elisp-eval-error-type? err-obj)
               (lens-set
                (env-get-stack-trace env)
                err-obj =>elisp-eval-error-stack-trace
                ))
             ;;(write-elisp-eval-error err-obj env port)
             (env-reset-stack! env)
             (halt-eval err-obj)
             )))))

    (define handle-scheme-exceptions*
      ;; This is a parameter that can alter the behavior of the Elisp
      ;; evaluator when an exception is raised by Scheme, not by
      ;; Elisp. If this parameter is set to `#F` (the default), the
      ;; Scheme exception is handled, the Elisp interpreter is set
      ;; into a consistent state, and then the Scheme exception is
      ;; re-raised -- when running in the REPL, this will trigger the
      ;; Scheme REPL execption handler and (usually) allow you to
      ;; inspect the backtrace and debug the interpreter. If you set
      ;; this to `#t` the exception is handled but not re-raised
      ;; (effectively, ignored in the REPL).
      ;;
      ;; If you set this parameter to hold a procedure, the procedure
      ;; will be applied with two arguments when a Scheme exception
      ;; occurs during Emacs Lisp evaluation:
      ;;
      ;;  1. the continuation that can optionally be applied a single
      ;;     arbitrary argument (such as an error object) to signal
      ;;     that the error condition has been handled, and
      ;;
      ;;  2. the Scheme error object that was caught by the Elisp
      ;;     exception handler.
      ;;--------------------------------------------------------------
      (make-parameter #f)
      )

    (define new-elisp-error-handler
      ;; This procedure is similar to `new-elisp-raise-impl` in that
      ;; it creates an error handler procedure. However the returned
      ;; procedure is designed to be used as an exception handler to
      ;; be used with the Scheme `with-exception-handler` syntax.
      ;;--------------------------------------------------------------
      (case-lambda
        ((env halt-eval)
         (new-elisp-error-handler env halt-eval (current-output-port))
         )
        ((env halt-eval port)
         (let ((env (or env (*the-environment*))))
           (lambda (err-obj)
             (when (elisp-eval-error-type? err-obj)
               (update
                (lambda (tr) (or tr (env-get-stack-trace env)))
                err-obj =>elisp-eval-error-stack-trace
                ))
             ;;(write-elisp-eval-error err-obj env port)
             (env-reset-stack! env)
             (let ((user-handler (handle-scheme-exceptions*)))
               (cond
                ((boolean=? #t user-handler) (halt-eval err-obj))
                ((procedure? user-handler) (user-handler halt-eval err-obj))
                (else (values))
                )))))))

    (define (%elisp-eval! expr env)
      (define (run)
        (call/cc
         (lambda (halt-eval)
           (let*((handler (new-elisp-error-handler env halt-eval))
                 (raise-impl (new-elisp-raise-impl env halt-eval))
                 (return
                  (parameterize ((raise-error-impl* raise-impl))
                    (with-exception-handler handler
                      (lambda () (eval-form expr (env-get-location expr)))
                      ))))
             (cond
              ((elisp-eval-error-type? return) (raise-impl return))
              (else return)
              )))))
      (define (run-with-mode new-lxmode)
        (let*((env (*the-environment*))
              (old-lxmode (view env =>env-lexical-mode?!))
              (sym-name "max-lisp-eval-depth")
              (trace-max (view env (=>env-symbol! sym-name) (=>sym-value! sym-name)))
              )
          (when trace-max
            (lens-set
             (max 100 (or trace-max (*max-lisp-eval-depth*)))
             env =>env-trace-max*!
             ))
          (lens-set new-lxmode env =>env-lexical-mode?!)
          (let ((result (run)))
            (lens-set old-lxmode env =>env-lexical-mode?!)
            result
            )))
      (cond
       ((not env) (run-with-mode #f))
       ((elisp-environment-type? env)
        (parameterize ((*the-environment* env)) (run))
        )
       ((or (hash-table? env) (pair? env))
        (let ((new-env (new-empty-environment *default-obarray-size*))
              )
          (parameterize
              ((*the-environment* new-env))
            (run-with-mode #t)
            )))
       (else (run-with-mode #t))
       ))

    (define elisp-eval!
      ;; Evaluate an Emacs Lisp expression that has already been parsed
      ;; from a string into a list or vector data structure. You can pass
      ;; an optional environment object, or a hash table. Note that if a
      ;; hash table is given, a new environment is created with the
      ;; lexical scoping mode enabled. If no environment is given, the
      ;; current environment is used.
      ;;------------------------------------------------------------------
      (case-lambda
        ((expr) (elisp-eval! expr #f))
        ((expr env) (%elisp-eval! expr env))
        ))

    (define (eval-iterate-forms env port use-form)
      (let-values
          (((port parst close-on-end)
            (cond
             ((input-port? port)
              (values port (parse-state port) #f)
              )
             ((string? port)
              (let*((port (open-input-file port))
                    (parst (parse-state port))
                    )
                (lens-set port parst =>parse-state-filepath*!)
                (values port parst #t)
                ))
             ((elisp-parse-state-type? port)
              (values #f port #f)
              )
             (else (error "not a filepath or input port" port))
             ))
           )
        (define (loop result)
          (let ((form (elisp-read parst)))
            (cond
             ((eq? form #t) (loop result))
             ((or (not form) (eof-object? form)) result)
             ((elisp-form-type? form) (loop (use-form form)))
             (else (loop (use-form form)))
             )))
        (cond
         (close-on-end
          (call-with-port port (lambda (port) (loop #t)))
          )
         (else (loop #t))
         )))

    (define (%elisp-load! filepath env)
      (let*((name "load-file-name")
            (port (open-input-file filepath))
            (env (or env (*the-environment*)))
            )
        ;; TODO: check the `ENV` properly, should create a new
        ;; environment if given an alist or hash table.
        (define (setq-load-file-name val)
          (lens-set val env
                    (=>env-obarray-key! name)
                    (=>sym-value! name)
                    ))
        (call-with-port port
          (lambda (port)
            (setq-load-file-name filepath)
            (let*((parst
                   (let ((parst (parse-state port)))
                     (lens-set filepath parst =>parse-state-filepath*!)
                     parst
                     ))
                  (old-dialect (view env =>env-lexical-mode?!))
                  (dialect (select-elisp-dialect! parst))
                  (env
                   (lens-set
                    (case dialect
                      ((dynamic-binding) #f)
                      (else #t))
                    env =>env-lexical-mode?!
                    ))
                  (result
                   (call/cc
                    (lambda (halt-eval)
                      (let ((handler (new-elisp-error-handler env halt-eval))
                            (raise-impl (new-elisp-raise-impl env halt-eval))
                            )
                        (parameterize ((raise-error-impl* raise-impl))
                          (with-exception-handler handler
                            (lambda ()
                              (eval-iterate-forms
                               env parst
                               (lambda (form)
                                 (eval-form form (env-get-location form))
                                 ))))))))))
              (exec-run-hooks 'after-load-functions (list filepath))
              (setq-load-file-name nil)
              (lens-set old-dialect env =>env-lexical-mode?!)
              result
              )))))

    (define elisp-load!
      (case-lambda
        ((filepath) (%elisp-load! filepath #f))
        ((filepath env) (%elisp-load! filepath env))
        ))

    (define subfeature-key "subfeature")

    (define (elisp-provide . args)
      (match args
        (() (eval-error "wrong number of arguments" 0 'min 1))
        ((feature subfeatures ...)
         (let ()
           (define (check-features features)
             (cond
              ((pair? features)
               (let ((feature (car features)))
                 (cond
                  ((symbol? feature) (check-features (cdr features)))
                  (else (eval-error "wrong type argument" feature 'expecting "symbol"))
                  )))
              ((null? features) #t)
              ((symbol? features) #t)
              (else (eval-error "wrong type argument" features 'expecting "list of symbols"))
              ))
           (check-features (cons feature subfeatures))
           (let ((name (symbol->string feature)))
             (update
              (lambda (obj)
                (let ((obj (if obj obj (new-symbol name))))
                  (when (pair? subfeatures)
                    (let ((subfeatures (map symbol->string subfeatures)))
                      (update
                       (lambda (subs)
                         (cond
                          ((or (not subs) (null? subs)) subfeatures)
                          (else (append subfeatures subs)
                                ;; ^ TODO: should be set union, not append
                                )))
                       obj
                       (=>sym-plist! name)
                       (=>hash-key! subfeature-key)
                       ;; ^ TODO: Canonical lens constructs a hash table, set
                       ;; the number of bins to a smaller number here.
                       )))
                  obj
                  ))
              (*the-environment*)
              (=>env-obarray-key! name))
             #f
             )))
        ))

    (define eval-featurep
      (case-lambda
        ((sym) (eval-featurep sym #f))
        ((sym sub)
         (let*((name (symbol->string sym))
               (subname (if sub (symbol->string sub) #f))
               (st (*the-environment*))
               (obj (view st (=>env-obarray-key! name)))
               )
           (and obj
                (or (not sub)
                    (let ((subf-list
                           (view obj
                                 (=>sym-plist! name)
                                 (=>hash-key! subfeature-key)
                                 )))
                      (and subf-list
                           (not
                            (null?
                             (view subf-list
                                   (=>find (lambda (a) (equal? a subname))))))
                           )))
                #t)
           ))))

    (define (elisp-featurep . args)
      (match args
        ((sym) (eval-featurep sym))
        ((sym sub) (eval-featurep sym sub))
        (any (eval-error "wrong number of arguments" (length any) 'min 1 'max 2))
        ))

    (define eval-locate-file-internal
      (case-lambda
        ((filename path)
         (eval-locate-file-internal filename path '()))
        ((filename path suffixes)
         (eval-locate-file-internal filename path suffixes #f))
        ((filename path suffixes predicate)
         ;; TODO: make this work the same way it does in GNU Emacs. This
         ;; current implementation is just temporary.
         (string-append "./elisp/lisp/emacs-lisp/" filename ".el")
         )))

    (define eval-require
      (case-lambda
        ((sym) (eval-require sym #f #f))
        ((sym filename) (eval-require sym filename #f))
        ((sym filename noerror)
         (cond
          ((not (symbol? sym))
           (eval-error "wrong type argument" sym 'expecting "symbol")
           )
          ((and filename (not (string? filename)))
           (eval-error "wrong type argument" filename 'expecting "string")
           )
          (else
           (cond
            ((eval-featurep sym) sym)
            (else
             (let ((filename (if filename filename (symbol->string sym))))
               (cond
                ((not (eval-featurep sym))
                 (let ((fullpath (eval-locate-file-internal filename "")))
                   (cond
                    ((elisp-eval-error-type? fullpath)
                     (if noerror #f (eval-raise fullpath)))
                    ((string? fullpath) (elisp-load! fullpath) sym)
                    ((not noerror)
                     (eval-error
                      "cannot open load file"
                      "no such file or directory"
                      filename))
                    (else #f)
                    )))
                (else #f)
                )))))))))

    (define (elisp-require . args)
      (match args
        ((sym) (eval-require sym))
        ((sym filename) (eval-require sym filename))
        ((sym filename noerror) (eval-require sym filename noerror))
        (any (eval-error "wrong number of arguments" (length any) 'min 1 'max 3))
        ))


    (define exec-run-hooks
      (case-lambda
        ((hook-list args-list)
         (exec-run-hooks #f hook-list args-list)
         )
        ((until-val hook-list args-list)
         (define (until-failure result loop hook-list)
           (if (elisp-null? result) #f (loop hook-list))
           )
         (define (until-success result loop hook-list)
           (if (elisp-null? result) (loop hook-list) #f)
           )
         (define (until-end result loop hook-list)
           (loop hook-list)
           )
         (let ((st (*the-environment*))
               (recurse
                (cond
                 ((not until-val) until-end)
                 ((eq? until-val 'failure) until-failure)
                 ((eq? until-val 'success) until-success)
                 (else (error "loop control symbol" until-val))
                 ))
               )
           (define (third hook)
             ;; Final level of indirection: `HOOK` must be a symbol
             ;; object, must contain a callable function.
             (let*((name (symbol->string hook))
                   (func (view st
                               (=>env-symbol! name)
                               (=>sym-function! name)
                               ))
                   )
               (cond
                (func (%elisp-apply func args-list (view func =>lambda-location*!)))
                (else (eval-error "void variable" hook)))
               ))
           (define (second hook)
             ;; Second level of indirection. A hook must be a symbol or a
             ;; list of symbols, each symbol must resolve to a function
             ;; that can be evaluated.
             (cond
              ((not   hook) #f)
              ((null? hook) #f)
              ((and (pair? hook) (symbol? (car hook)))
               (recurse (third (car hook)) second (cdr hook)))
              ((symbol? hook) (third hook))
              (else
               (eval-error
                "wrong type argument" hook
                'expecting "symbol or list"
                ))))
           (define (first hook-list)
             ;; First level of indirection. A hook must be a symbol that
             ;; resolves to a symbol or list of symbols.
             (cond
              ((pair? hook-list)
               (let*((hook-name (car hook-list))
                     (hook-list (cdr hook-list))
                     )
                 (cond
                  ((symbol? hook-name)
                   (let*((name (symbol->string hook-name))
                         (hook
                          (view st
                                (=>env-symbol! name)
                                (=>sym-value! name)))
                         (result (second hook))
                         )
                     (recurse result first hook-list)
                     ))
                  (else
                   (eval-error "wrong type argument" hook-name 'expecting "symbol")
                   ))
                 ))
              (else #f)
              ))
           (cond
            ((null? hook-list) #f)
            ((pair? hook-list) (first hook-list))
            ((symbol? hook-list) (first (list hook-list)))
            (else
             (eval-error "wrong type argument" hook-list
                         'expecting "symbol or symbol list"))
            )))
        ))


    (define (elisp-run-hooks . args) (exec-run-hooks args '()))

    (define (elisp-hook-runner name control)
      (lambda args
        (match args
          (() (eval-error "wrong number of arguments" name 0 'min 1))
          ((hook args ...) (exec-run-hooks control (list hook) args))
          ))
      )

    (define elisp-run-hooks-with-args (elisp-hook-runner "run-hooks-with-args" #f))

    (define elisp-run-hook-with-args-until-failure
      (elisp-hook-runner "run-hook-with-args-until-failure" 'failure))

    (define elisp-run-hook-with-args-until-success
      (elisp-hook-runner "run-hook-with-args-until-success" 'success))

    ;;====================================================================
    ;; Abstract interpreter:
    ;; ---------------------
    ;;
    ;; The algorithm for the interpreter is split into a group of few
    ;; different mutually recursive procedures. These same procedures can
    ;; be used to implement different versions of interpreter: firstly,
    ;; the usual interpreter which should evaluate each form as quickly as
    ;; possible, and secondly, the debugging interpreter that pauses after
    ;; each form evaluation and allows the operator to inspect the stack.
    ;;
    ;; Both versions of the interpreter should run the exact same
    ;; algorithm, but these algorithms need to be defined such that the
    ;; ordinary non-debugging interpreter can still run as fast as
    ;; possible, so it would not be a good approach to insert conditional
    ;; statements into the algorithm that check if we are in "debug mode"
    ;; at every single evaluation step.
    ;;
    ;; So the approach taken here is instead to take a data structure
    ;; which references the group of procedures that implement each part
    ;; of the interpreter algorithm. The fast interpreter can jump
    ;; immediately to these procedures in the data structure without
    ;; computing any conditional expressions.  The debugging interpreter
    ;; call into the fast interpreter procedures through wrapper
    ;; procedures that allow the operator to inspect the interpreter
    ;; state before actually computing that step of the evaluation.

    (define-record-type <interpreter-type>
      (make<interpreter> apply eval eval-qq on-args on-body wrap new-frame)
      interpreter-type?
      (apply       interpret-apply       set!interpret-apply)
      (eval        interpret-eval        set!interpret-eval)
      (eval-qq     interpret-eval-qq     set!interpret-eval-qq)
      (on-args     interpret-args        set!interpret-args)
      (on-body     interpret-body        set!interpret-body)
      (wrap        interpret-wrap        set!interpret-wrap)
      (new-frame   interpret-new-frame   set!interpret-new-frame)
      )

    (define (new-interpreter) (make<interpreter> #f #f #f #f #f #f #f))

    ;;====================================================================
    ;; The interpreting evaluator. Matches patterns on the program and
    ;; immediately executes each form or symbol. The entrypoint is
    ;; `EVAL-FORM`.

    (define (%unpack expr)
      (cond
       ((elisp-form-type? expr) (elisp-form->list expr))
       (else expr)
       ))

    (define elisp-eval-reset-stack!
      (case-lambda
        (() (elisp-eval-reset-stack! (*the-environment*)))
        ((env) (env-reset-stack! env))
        ))

    (define (i-push-stack-frame-eval-body interp)
      ;; This applies arguments to a `FUNC` which must be of type
      ;; `LAMBDA-TYPE?`, without evaluating the arguments `ARGS`. It
      ;; creates and pushes a stack frame with the `ARGS` as they are
      ;; given. Because function arguments are always lexical, only the
      ;; lexical stack is modified.
      ;;------------------------------------------------------------------
      (lambda (func args)
        (let ((elstkfrm (elstkfrm-from-args func args)))
          (cond
           ((not elstkfrm) (error "elstkfrm-from-args returned #f"))
           ((elisp-eval-error-type? elstkfrm) (eval-raise elstkfrm))
           (else
            (let*((st (*the-environment*))
                  (old-stack (view st =>env-lexstack*!))
                  (lexenv (view func =>lambda-lexenv!))
                  (new-stack
                   (if lexenv (list lexenv elstkfrm) (list elstkfrm))
                   )
                  (st (lens-set new-stack st =>env-lexstack*!))
                  (return ((interpret-body interp) (view func =>lambda-body*!))) ;; apply
                  )
              (lens-set old-stack st =>env-lexstack*!)
              return
              ))))))

    (define (i-scheme-lambda->elisp-lambda interp)
      ;; Constructs a Scheme procedure that takes Elisp arguments and
      ;; returns an Elisp result. This is useful for implementing
      ;; higher-order Emacs Lisp functions such as `MAPCAR`.
      ;;
      ;; This may seem confusing because this procedure returns a Scheme
      ;; procedure, not an Elisp Lambda (as in a data value that satisfies
      ;; the `LAMBDA-TYPE?` predicate) although the name implies it
      ;; returns an "elisp-lambda". It is helpful to instead think about
      ;; the inputs and outputs of the procedure that is returned by this
      ;; procedure -- what is returned is a procedure that takes Elisp
      ;; values and returns Elisp values, as opposed to a Scheme lambda
      ;; which takes Scheme values and returns Scheme values.
      ;;
      ;; Furthermore, if you apply an actual `LAMBDA-TYPE?` values as the
      ;; `FUNC` argument, the returned Scheme procedure simply applies
      ;; it's arguments to the `FUNC` using the `ELISP-APPLY procedure
      ;; without any `elisp->scheme` value conversions. So regardless of
      ;; the type of the `FUNC` argument that is applied (whether it be a
      ;; Scheme procedure or an Elisp Lambda), the result is a procedure
      ;; that takes Elisp values and returns Elisp values.
      ;;------------------------------------------------------------------
      (lambda (func)
        (lambda args
          (cond
           ((lambda-type? func)
            (cond
             ((eq? 'macro (view func =>lambda-kind*!))
              ((interpret-eval interp) ((interpret-new-frame interp) func args))
              )
             (else
              ((interpret-new-frame interp) func args)
              )))
           ((procedure? func)
            (scheme->elisp (apply func (map elisp->scheme args)))
            )
           ((command-type? func)
            (scheme->elisp
             (apply (command-procedure func) (map elisp->scheme args))
             ))
           (else (eval-error "wrong type argument" func 'expecting "function"))
           ))))

    (define (i-%elisp-apply interp)
      ;; This is the actual `APPLY` procedure for Emacs Lisp. The `HEAD`
      ;; argument is resolved to a procedure, macro, command, or lambda.
      ;; If `HEAD` resolves to a lambda, and `LAMBDA-KIND` is `'MACRO`,
      ;; the macro is expanded by evaluating the result with `eval-form`
      ;; before a value is returned.
      (lambda (head arg-exprs loc)
        (let*((st (*the-environment*))
              (func (env-resolve-function st head))
              (func (cond
                     ((pair? func) ((interpret-eval interp) func #f))
                     ((elisp-form-type? func)
                      ((interpret-eval interp) func (env-get-location func))
                      )
                     (else func)
                     ))
              (sym  (if (lambda-type? head) 'lambda head))
              (loc  (or loc (and (lambda-type? func) (view func =>lambda-location*!))))
              )
          (cond
           ((elisp-eval-error-type? func) (eval-raise func))
           (else
            (env-trace!
             loc sym func st eval-error
             (lambda ()
               (cond
                ((syntax-type?  func)
                 (apply (syntax-eval func) head arg-exprs))
                ((lambda-type?  func)
                 (cond
                  ((macro-type? func)
                   ((interpret-eval interp)
                    ((interpret-new-frame interp) func arg-exprs)
                    (env-get-location func)
                    ))
                  (else
                   ((interpret-new-frame interp) func ((interpret-args interp) arg-exprs))
                   )))
                ((command-type? func)
                 (apply (command-procedure func) ((interpret-args interp) arg-exprs))
                 )
                ((procedure?    func)
                 (apply func ((interpret-args interp) arg-exprs))
                 )
                ((pair?         func)
                 (match func
                   (('lambda arg-exprs body ...)
                    (let ((func (apply (syntax-eval elisp-lambda) func)))
                      ((interpret-new-frame interp)
                       (defun-make-lambda 'lambda (%unpack arg-exprs) body)
                       ((interpret-args interp) arg-exprs))
                      ))
                   (('macro . func)
                    (cond
                     ((lambda-type? func)
                      (let ((interim ((interpret-new-frame interp) func arg-exprs)))
                        ((interpret-eval interp)
                         interim
                         (env-get-location func)
                         )))
                     (else
                      (eval-error "Invalid macro" 'expected 'lambda 'actual func)
                      )
                     ))
                   (any (eval-error "Invalid function" func))
                   ))
                (else (eval-error "Invalid function" head))
                ))))))))


    (define (i-eval-form interp)
      (lambda (expr loc)
        (match expr
          (() '())
          ((head args ...) ((interpret-apply interp) head args loc))
          (('quote expr) expr)
          (('quote exprs ...)
           (eval-error
            "wrong number of arguments"
            "quote" 'expected 1 'got (length exprs)
            ))
          (literal
           (cond
            ((symbol? literal)
             (let ((str (symbol->string literal)))
               (cond
                ((and (> (string-length str) 0) (char=? #\: (string-ref str 0)))
                 literal ;; symbols starting with : are self-evaluating "keyword" symbols
                 )
                (else
                 (let ((return (view literal =>elisp-symbol!)))
                   (cond
                    ((not return) (eval-error "void variable" literal))
                    ((sym-type? return) (view return =>sym-value*!))
                    (else return)
                    ))))))
            ((elisp-quote-scheme-type? literal)
             (cond
              ((elisp-backquoted-form? literal)
               (car ((interpret-eval-qq interp) (elisp-unquote-scheme literal)))
               )
              (else (elisp-unquote-scheme literal))
              ))
            ((elisp-form-type? literal)
             (cond
              ((square-bracketed-form? literal)
               (elisp-form->vector literal)
               )
              (else
               ((interpret-eval interp)
                (elisp-form->list literal)
                (elisp-form-start-loc literal)
                ))))
            ((elisp-function-ref-type? literal)
             (eval-function-ref (elisp-function-get-ref literal))
             )
            (else literal)
            )))))


    (define (i-eval-args-list interp)
      (lambda (arg-exprs)
        (let ((result
               (let loop ((exprs arg-exprs))
                 (match exprs
                   (() '())
                   ((expr more ...)
                    (let ((result
                           ((interpret-eval interp)
                            expr
                            (env-get-location arg-exprs)
                            )))
                      (if (elisp-eval-error-type? result) result
                          (cons result (loop more)))
                      ))))))
          result
          )))


    (define (i-eval-progn-body interp)
      (lambda (body-exprs)
        (let loop ((exprs body-exprs))
          (match exprs
            (() '())
            ((final) ((interpret-eval interp) final (env-get-location final)))
            ((head more ...)
             ((interpret-eval interp) head (env-get-location head))
             (loop more)
             )
            (exprs (error "no function body" exprs))
            ))))


    (define (i-eval-backquote interp)
      (lambda (expr)
        (let expr-loop ((expr expr))
          (define (splice-loop elems resume)
            (cond
             ((null? elems) (expr-loop resume))
             ((pair? elems)
              (cons
               (car elems)
               (splice-loop (cdr elems) resume)
               ))
             (else (cons elems (expr-loop resume)))
             ))
          (define (next result remaining)
            (if remaining (cons result (expr-loop remaining)) result))
          (define (single expr remaining)
            (unless expr (error "backquoted null expression"))
            (cond
             ((elisp-form-type? expr)
              (next (expr-loop (elisp-form->list expr)) remaining)
              )
             ((elisp-quote-scheme-type? expr)
              (next
               (elisp-quote-scheme
                (expr-loop (elisp-unquote-scheme expr))
                (elisp-backquoted-form? expr)
                )
               remaining
               ))
             ((elisp-unquoted-form-type? expr)
              (let*((form (elisp-unquoted-get-form expr))
                    (loc (env-get-location form))
                    (splice-elems ((interpret-eval interp) form loc))
                    )
                (cond
                 ((elisp-spliced-form? expr)
                  (splice-loop splice-elems remaining)
                  )
                 (else (next splice-elems remaining))
                 )))
             (remaining (cons expr (expr-loop remaining)))
             (else expr)
             ))
          (match expr
            (() '())
            ((('|,| unq) remaining ...)
             (cons
              ((interpret-eval interp) (expr-loop unq) (env-get-location unq))
              (expr-loop remaining)
              ))
            ((('|,@| splice) remaining ...)
             (splice-loop
              ((interpret-eval interp) (expr-loop splice) (env-get-location splice))
              remaining
              ))
            (((sub-expr ...) remaining ...)
             (next (expr-loop sub-expr) remaining)
             )
            ((expr remaining ...) (single expr remaining))
            (expr (single expr #f))
            ))))


    (define ordinary-interpreter
      (let ((i (new-interpreter)))
        (set!interpret-apply      i (i-%elisp-apply                 i))
        (set!interpret-eval       i (i-eval-form                    i))
        (set!interpret-eval-qq    i (i-eval-backquote               i))
        (set!interpret-args       i (i-eval-args-list               i))
        (set!interpret-body       i (i-eval-progn-body              i))
        (set!interpret-wrap       i (i-scheme-lambda->elisp-lambda  i))
        (set!interpret-new-frame  i (i-push-stack-frame-eval-body   i))
        i))

    (define *current-interpreter* (make-parameter ordinary-interpreter))

    ;;--------------------------------------------------------------------
    ;; A debugger state is an interpreter that contains a function which
    ;; stores the currently evaluating form and breaks out of the current
    ;; continuation on each call to `APPLY`. A debugger is an interpreter
    ;; state in which each of the interpreter functions can inspect this
    ;; debugger state.

    (define-record-type <debugger-state-type>
      (make<debugger-state>
       interp      pause       step
       cur-form   last-value   trace-mode
       skip-mode  resume-mode  breaks
       )
      debugger-state-type?
      (interp       debugger-interpreter)
      (pause        debugger-pause          set!debugger-pause) ;;continuation used to pause evaluation
      (step         debugger-stepper        set!debugger-stepper)
      (cur-form     debugger-current-form   set!debugger-current-form)
      (last-value   debugger-last-value     set!debugger-last-value)
      (trace-mode   debugger-trace-mode     set!debugger-trace-mode)
      (skip-mode    debugger-skip-mode      set!debugger-skip-mode)
      (resume-mode  debugger-continue-mode  set!debugger-continue-mode)
      (breaks       debugger-breakpoints    set!debugger-breakpoints)
      )

    (define =>debugger-breakpoints*!
      (record-unit-lens
       debugger-breakpoints
       set!debugger-breakpoints
       '=>debugger-breakpoints*!
       ))

    (define =>debugger-current-form*!
      (record-unit-lens
       debugger-current-form
       set!debugger-current-form
       '=>debugger-current-form*!
       ))

    (define =>debugger-last-value*!
      (record-unit-lens
       debugger-last-value
       set!debugger-last-value
       '=>debugger-last-value*!
       ))

    (define elisp-debug-show-form
      (case-lambda
        ((debug-state)
         (elisp-debug-show-form debug-state (current-output-port))
         )
        ((debug-state port)
         (let ((form (debugger-current-form debug-state)))
           (cond
            ((and (elisp-form-type? form) (elisp-form-locations form))
             (write-location-form-newline form port)
             )
            (else
             (write-elisp-form form port)
             (newline port)
             ))))))


    (define elisp-debug-show-result
      (case-lambda
        ((debug-state)
         (elisp-debug-show-result debug-state (current-output-port))
         )
        ((debug-state port)
         (write-elisp-form (debugger-last-value debug-state) port)
         (newline port)
         )))


    (define (new-debugger-state interp)
      (make<debugger-state> interp #f #f #f #f #f #f #f '()))


    (define (form-head form)
      (and
       (elisp-form-type? form)
       (let*((vec (elisp-form-tokens form))
             (len (vector-length vec))
             (sym (if (> len 0) (vector-ref vec 0) #f))
             )
         (if (symbol? sym) sym #f)
         )))


    (define (%debug-eval debug-state)
      (let ((i (debugger-interpreter debug-state)))
        (lambda (form loc)
          (let*((cont-mode (debugger-continue-mode debug-state))
                (sym (or (and (symbol? form) form)
                         (and (pair? form) (symbol? (car form)) (car form))
                         (form-head form)
                         ))
                (on-break
                 (and sym
                      (member
                       (ensure-string sym)
                       (debugger-breakpoints debug-state)
                       ))))
            (cond
             ((and cont-mode (not on-break)) ((i-eval-form i) form loc))
             (else
              (set!debugger-current-form debug-state form)
              (call/cc
               (lambda (resume)
                 (set!debugger-stepper
                  debug-state
                  (lambda ()
                    (set!debugger-last-value debug-state #f)
                    (let*((return ((i-eval-form i) form loc)))
                      (set!debugger-last-value debug-state return)
                      (set!debugger-current-form debug-state form)
                      (set!debugger-stepper debug-state (lambda () (resume return)))
                      ((debugger-pause debug-state) #t)
                      )))
                 ((debugger-pause debug-state) #t)
                 ))))))))


    (define (new-debugger)
      (let*((i  (new-interpreter))
            (st (new-debugger-state i))
            )
        (set!interpret-eval       i (%debug-eval  st))
        (set!interpret-eval-qq    i (i-eval-backquote i))
        (set!interpret-apply      i (i-%elisp-apply i))
        (set!interpret-args       i (i-eval-args-list i))
        (set!interpret-body       i (i-eval-progn-body i))
        (set!interpret-wrap       i (i-scheme-lambda->elisp-lambda i))
        (set!interpret-new-frame  i (i-push-stack-frame-eval-body i))
        (set!debugger-stepper st #t)
        st))


    (define (elisp-debug-step! debug-state)
      ;; Single-step the evaluator in the `DEBUG-STATE`.
      ;;------------------------------------------------------------------
      (let ((stepper (debugger-stepper debug-state))
            (i   (debugger-interpreter debug-state))
            )
        (cond
         ((eq? stepper #f) #f)
         ((eq? stepper #t)
          (call/cc
           (lambda (pause)
             (set!debugger-pause debug-state pause)
             (let*((form   (debugger-current-form debug-state))
                   (return ((%debug-eval debug-state) form (env-get-location form)))
                   )
               (set!debugger-last-value debug-state return)
               (set!debugger-stepper debug-state #f)
               #f
               ))))
         (else
          (parameterize ((*current-interpreter* i))
            (call/cc
             (lambda (pause)
               (set!debugger-pause debug-state pause)
               (stepper)
               )))))))


    (define (elisp-debug-view-step! debug-state)
      ;; Like `ELISP-DEBUG-STEP!` except it shows which form is being
      ;; evaluated, and what value the form returns, unless the form does
      ;; not return in which case the returned value from the previous
      ;; evaluation step is shown again.
      ;;------------------------------------------------------------------
      (dynamic-wind
        (lambda () (write-elisp-form (view debug-state =>debugger-current-form*!)) (newline))
        (lambda () (elisp-debug-step! debug-state))
        (lambda () (write-elisp-form (view debug-state =>debugger-last-value*!)) (newline))
        ))


    (define (elisp-debug-step-value! debug-state)
      ;; Perform a step and then return the value it returned to the
      ;; calling context. Obviously this is only useful when manipulating
      ;; the debugger state directly from the Scheme REPL.
      ;;------------------------------------------------------------------
      (elisp-debug-step! debug-state)
      (debugger-last-value debug-state)
      )


    (define (elisp-debug-skip! debug-state)
      ;; FIXME: this does not work correctly yet, see `ELISP-DEBUG-CONTINUE!`.
      ;;
      ;; This is supposed to note the current stack depth and allow
      ;; `ELISP-DEBUG-CONTINUE!` through the current expression until the
      ;; stack depth becomes less than or equal to the noted stack depth.
      (set!debugger-skip-mode
       debug-state
       (view (*the-environment*) =>env-trace-depth*!)
       )
      (elisp-debug-step! debug-state)
      )


    (define (elisp-debug-continue! debug-state)
      ;; FIXME: this does not work correctly yet, fails to stop at breakpoints.
      (set!debugger-continue-mode debug-state #t)
      (elisp-debug-step! debug-state)
      )


    (define (elisp-debug-set-break! debug-state sym)
      ;; FIXME: this does not work correctly yet, see `ELISP-DEBUG-CONTINUE!`.
      (let ((sym    (ensure-string sym))
            (breaks (view debug-state =>debugger-breakpoints*!))
            )
        (update
         (lambda (breaks)
           (cond
            ((member sym breaks) breaks)
            (else (cons sym breaks))
            ))
         debug-state =>debugger-breakpoints*!
         )))


    (define (elisp-debug-clear-break! debug-state sym)
      (let ((sym (ensure-string sym)))
        (update
         (lambda (breaks)
           (cond
            ((integer? sym)
             (let loop ((i 0) (breaks breaks))
               (cond
                ((null? breaks) breaks)
                ((= i sym) (cdr breaks))
                (else (cons (car breaks) (loop (+ 1 i) (cdr breaks))))
                )))
            ((string? sym)
             (let loop ((breaks breaks))
               (cond
                ((null? breaks) breaks)
                ((string=? sym (car breaks)) (cdr breaks))
                (else (cons (car breaks) (loop (cdr breaks))))
                )))
            (else
             (error "breakpoint ID be a symbol, string, or integer" sym)
             )))
         debug-state =>debugger-breakpoints*!
         )))


    (define elisp-debug-show-breaks
      (case-lambda
        ((debug-state)
         (elisp-debug-show-breaks debug-state (current-output-port))
         )
        ((debug-state port)
         (let ((breaks (view debug-state =>debugger-breakpoints*!)))
           (let loop ((breaks breaks))
             (cond
              ((null? breaks) (display "----\n" port))
              (else
               (display (car breaks) port)
               (newline port)
               (loop (cdr breaks))
               )))))))


    (define elisp-debug-eval
      ;; Construct a new debugger state containing the given `EXPR`
      ;; argument and return it.  It is also possible to reused a debugger
      ;; state by passing it as the second argument to this
      ;; procedure. With the returned debugger state you may use
      ;; `ELISP-DEBUG-STEP!`, `ELISP-DEBUG-SKIP!`, `DEBUG-CONTINUE!`,
      ;; `ELISP-DEBUG-SET-BREAK!`, `DEBUG-CLEAR-BREAK!`, and
      ;; `ELISP-DEBUG-SHOW-BREAKS`.
      (case-lambda
        ((expr) (elisp-debug-eval expr (new-debugger)))
        ((expr debug-state)
         (set!debugger-current-form debug-state expr)
         (set!debugger-last-value debug-state #f)
         (elisp-debug-step! debug-state)
         debug-state
         )))

    ;;--------------------------------------------------------------------
    ;; The following interfaces are used by built-in macros such as
    ;; `progn` or `let*`, the `*current-interpreter*` parameter is used to
    ;; select the correct interpreter. This indirection through the
    ;; parameter may be slightly inefficient, but is only used when a
    ;; built-in macro is evaluated.

    (define (%elisp-apply . args)
      (apply (interpret-apply (*current-interpreter*)) args))

    (define (eval-args-list . args)
      (apply (interpret-args (*current-interpreter*)) args))

    (define (eval-progn-body . args)
      (apply (interpret-body (*current-interpreter*)) args))

    (define (scheme-lambda->elisp-lambda . args)
      (apply (interpret-wrap (*current-interpreter*)) args))

    (define (push-stack-frame-eval-body . args)
      (apply (interpret-new-frame (*current-interpreter*)) args))

    (define (eval-backquote . args)
      (apply (i-eval-backquote (*current-interpreter*)) args))

    (define eval-form
      (case-lambda
        ((form)
         ((interpret-eval (*current-interpreter*)) form (env-get-location form))
         )
        ((form loc)
         ((interpret-eval (*current-interpreter*)) form loc)
         )))

    ;;--------------------------------------------------------------------
    ;; Built-in macros
    ;;
    ;; Note that these macros always begin with a (next) statement. This
    ;; is because the macro evaluator for built-in macros does not skip
    ;; over the head of the form before evaluating the macro. This makes
    ;; built-in macro bahvior a bit more Scheme-like.

    (define elisp-progn
      (make<syntax>
       (lambda expr
         (eval-progn-body (cdr expr)))))

    (define elisp-prog1
      (make<syntax>
       (lambda expr
         (match (cdr expr)
           (() (eval-error "wrong number of arguments" "prog1" 0))
           ((first more ...)
            (let ((return (eval-form first)))
              (eval-progn-body more)
              return
              ))))))

    (define elisp-prog2
      (make<syntax>
       (lambda expr
         (define (fail-nargs n)
           (eval-error "wrong number of arguments" "prog2" n))
         (match (cdr expr)
           (() (fail-nargs 0))
           ((any) (fail-nargs 1))
           ((first second more ...)
            (eval-form first)
            (let ((return (eval-form second)))
              (eval-progn-body more)
              return
              ))))))


    (define (elisp-logic conjunction)
      (make<syntax>
       (lambda exprs
         (let loop ((exprs (cdr exprs)))
           (match exprs
             (() (if conjunction #t '()))
             ((final) (eval-form final))
             ((next more ...)
              (let ((success (eval-form next)))
                (if conjunction
                    (if (elisp-null? success) '() (loop more))
                    (if (elisp-null? success) (loop more) #t))
                )))))))

    (define elisp-and (elisp-logic #t))
    (define elisp-or  (elisp-logic #f))


    (define elisp-if
      (make<syntax>
       (lambda exprs
         (match exprs
           (() (eval-error "wrong number of arguments" "if" 0))
           (('if cond-expr) (eval-error "wrong number of arguments" "if" 1))
           (('if cond-expr then-exprs else-exprs ...)
            (let ((result (eval-form cond-expr)))
              (if (not (elisp-null? result))
                  (eval-form then-exprs)
                  (eval-progn-body else-exprs)
                  )))))))


    (define elisp-when-unless
      (make<syntax>
       (lambda exprs
         (define (is   cond) (not (elisp-null? cond)))
         (define (isnt cond) (elisp-null? cond))
         (define (eval on-bool cond-expr body-expr)
           (let ((result (eval-form cond-expr)))
             (if (not (on-bool result)) #f
                 (eval-progn-body body-expr))
             ))
         (match exprs
           (() (eval-error "wrong number of arguments" "when or unless" 'min 1))
           (('when   cond-expr body-expr ...)
            (eval is   cond-expr body-expr))
           (('unless cond-expr body-expr ...)
            (eval isnt cond-expr body-expr))
           (any (eval-error "wrong number of arguments" (car any) 'min 1))
           ))))


    (define elisp-cond
      (make<syntax>
       (lambda exprs
         (define (eval-cond test body)
           (let ((result (eval-form test)))
             (if (not (elisp-null? result))
                 (values #t (eval-progn-body body))
                 (values #f #f))))
         (let loop ((exprs (map %unpack (cdr exprs))))
           (match exprs
             (() '())
             ((() more ...) (loop more))
             (((cond-expr body ...) more ...)
              (let-values
                  (((success return) (eval-cond cond-expr body)))
                (if success return (loop more))
                ))
             (any (eval-error "invalid cond branch expression" (car exprs)))
             )))))


    (define elisp-while
      (make<syntax>
       (lambda exprs
         (match (cdr exprs)
           (() (eval-error "wrong number of arguments" "while" 0))
           ((cond-expr body ...)
            (let loop ()
              (let ((success (eval-form cond-expr)))
                (if (not (elisp-null? success))
                    (let () (eval-progn-body body) (loop))
                    '() ;; while forms always evaluate to nil
                    ))))))))


    (define elisp-dotimes
      (make<syntax>
       (lambda exprs
         (define (fail-nargs n)
           (eval-error "wrong number of arguments" "dotimes" n))
         (match (map %unpack (cdr exprs))
           (() (fail-nargs 0))
           ((any) (fail-nargs 1))
           ((() any ...) (eval-error "expecting variable name" "dotimes" '()))
           (((var) any ...) (eval-error "expecting initial number" "dotimes" var))
           (((var limit-expr final-exprs ...) body ...)
            (cond
             ((symbol? var)
              (let ((limit (eval-form limit-expr)))
                (cond
                 ((integer? limit)
                  (cond
                   ((<= limit 0) (eval-progn-body final-exprs))
                   (else
                    (let ((st (*the-environment*)))
                      (env-with-elstkfrm!
                       st 1 '()
                       (lambda (elstkfrm)
                         (let ((obj (elstkfrm-sym-intern! elstkfrm (symbol->string var) 0)))
                           (let loop ((n 0))
                             (cond
                              ((>= n limit) (eval-progn-body final-exprs))
                              (else
                               (lens-set n obj =>sym-value*!)
                               (eval-progn-body body)
                               (loop (+ n 1))
                               ))))))))))
                 (else (eval-error "wrong type argument" "dotimes" limit 'expecting "integer"))
                 )))
             (else (eval-error "wrong type argument" "dotimes" var 'expecting "symbol"))
             ))))))


    (define elisp-dolist
      (make<syntax>
       (lambda exprs
         (define (fail-nargs n)
           (eval-error "wrong number of arguments" "dolist" n))
         (define (run var list-expr result-expr body)
           (define (final) (if result-expr (eval-form result-expr) '()))
           (cond
            ((symbol? var)
             (let*((elems (%unpack (eval-form list-expr))))
               (cond
                ((or (null? elems)
                     (and (vector? elems)
                          (>= 0 (vector-length elems))))
                 (final)
                 )
                ((or (pair? elems)
                     (and (vector? elems)
                          (< 0 (vector-length elems))))
                 (let*((cur (new-cursor elems))
                       (st (*the-environment*))
                       )
                   (env-with-elstkfrm!
                    st 1 '()
                    (lambda (elstkfrm)
                      (let ((obj (elstkfrm-sym-intern! elstkfrm (symbol->string var) 0)))
                        (let loop ()
                          (cond
                           ((cursor-end? cur) (final))
                           (else
                            (lens-set (cursor-ref cur) obj =>sym-value*!)
                            (eval-progn-body body)
                            (cursor-step! cur)
                            (loop)
                            ))))))))
                (else
                 (eval-error "wrong type argument" "dolist" elems 'expecting "list")
                 ))))
            (else (eval-error "wrong type argument" "dolist" var 'expecting "symbol")))
           )
         (match (map %unpack (cdr exprs))
           (((var list-expr) body ...)
            (run var list-expr #f body))
           (((var list-expr result-expr) body ...)
            (run var list-expr result-expr body))
           (any (eval-error "wrong number of arguments" "dolist" any))
           ))))


    (define elisp-setq
      (make<syntax>
       (lambda  args
         (let ((bind!
                (lambda (sym val)
                  (cond
                   ((symbol? sym)
                    (env-setq-bind! ;; lookup the symbol object for `SYM`, or intern a new one.
                     (*the-environment*)
                     (lambda (sym-obj)
                       (values
                        (lens-set val sym-obj =>sym-value*!) ;; Update the interned symbol,
                        sym-obj))
                     (symbol->string sym)))
                   (else
                    (eval-error "wrong type argument, expecting symbol" sym)
                    ))))
               (argc (length args))
               )
           (cond
            ((even? argc) ;; args includes "setq" symbol, argc should be odd
             (eval-error "wrong number of arguments, setq" (- argc 1))
             )
            (else
             (let loop
                 ((args (cdr args))
                  (argc 0)
                  (last-bound '())
                  )
               (match args
                 (() last-bound)
                 ((sym expr more ...)
                  (let ((val (eval-form expr)))
                    (cond
                     ((elisp-eval-error-type? val) val)
                     (else
                      (bind! sym val)
                      (loop more (+ 2 argc) val)
                      ))))))))))))


    (define elisp-let
      (make<syntax>
       (lambda expr
         (let ((st (*the-environment*)))
           (match (cdr expr)
             (() '())
             ((bindings-form progn-body ...)
              (let loop
                  ((unbound-exprs (%unpack bindings-form))
                   (bound '())
                   (size 0)
                   )
                (match unbound-exprs
                  (()
                   (env-with-elstkfrm!
                    st size (reverse bound)
                    (lambda _ (eval-progn-body progn-body))
                    ))
                  ((binding-expr more ...)
                   (match (%unpack binding-expr)
                     (() (eval-error "empty bindings form" binding-expr))
                     ((sym) (loop more (cons (new-symbol sym '()) bound) (+ 1 size)))
                     ((sym expr)
                      (let ((result (eval-form expr)))
                        (loop more
                              (cons (new-symbol (ensure-string sym) result) bound)
                              (+ 1 size)
                              )))
                     ((sym expr extra ...)
                      (eval-error "bindings can have only one value form" binding-expr)
                      )
                     ((? symbol? sym)
                      (loop more (cons (new-symbol sym '()) bound) (+ 1 size))
                      )
                     (otherwise
                      (eval-error "wrong type argument, expecting list" otherwise)
                      ))))))
             (otherwise
              (eval-error "wrong type argument, expecting list" otherwise)
              ))))))


    (define elisp-let*
      (make<syntax>
       (lambda expr
         (let ((st (*the-environment*)))
           (match (cdr expr)
             (() '())
             ((bindings-form progn-body ...)
              (let ((bindings (%unpack bindings-form)))
                (env-with-elstkfrm!
                 st (length bindings) '()
                 (lambda (elstkfrm)
                   (let loop ((unbound-exprs bindings))
                     (match unbound-exprs
                       (() (eval-progn-body progn-body))
                       ((binding-expr more ...)
                        (match (elisp-form->list binding-expr)
                          (() (eval-error "empty bindings form" binding-expr))
                          ((sym) (elstkfrm-sym-intern! elstkfrm sym '()))
                          ((sym expr)
                           (elstkfrm-sym-intern! elstkfrm sym (eval-form expr))
                           )
                          ((sym expr extra ...)
                           (eval-error "bindings can have only one value form" binding-expr)
                           )
                          ((? symbol? sym)
                           (elstkfrm-sym-intern! elstkfrm sym '())
                           )
                          (otherwise
                           (eval-error "wrong type argument, expecting list" otherwise))
                          )
                        (loop more)
                        )
                       ((? symbol? sym)
                        (elstkfrm-sym-intern! elstkfrm sym '())
                        )
                       (otherwise
                        (eval-error "wrong type argument, expecting bindings" otherwise)
                        )))))))
             (otherwise
              (eval-error "wrong type argument, expecting list" otherwise)
              ))))))


    (define elisp-lambda
      (make<syntax>
       (lambda expr
         (let*((expr (cdr expr)))
           (match expr
             (() (new-lambda))
             ((args body ...)
              (defun-make-lambda 'lambda (%unpack args) body))
             (args (eval-error "invalid lambda" args))
             )))))


    (define variable-documentation "variable-documentation")


    (define elisp-defvar
      (make<syntax>
       (lambda expr
         (let ((def (car expr))
               (expr (cdr expr)))
           (define (defvar sym-expr val-expr docstr)
             (let*((sym (eval-ensure-interned sym-expr))
                   (val (if val-expr (eval-form val-expr) #f))
                   )
               (lens-set val sym =>sym-value*!)
               (when docstr
                 (lens-set docstr sym
                           (=>sym-plist! (sym-name sym))
                           (=>hash-key! variable-documentation)))
               val
               ))
           (match expr
             (() (eval-error "wrong number of arguments" def '()))
             ((sym-expr) (defvar sym-expr #f #f))
             ((sym-expr val-expr) (defvar sym-expr val-expr #f))
             ((sym-expr val-expr docstr) (defvar sym-expr val-expr docstr))
             (any (eval-error "wrong number of arguments" def any))
             )))))


    (define elisp-defun-defmacro
      (make<syntax>
       (lambda expr
         (let ((def  (car expr))
               (expr (cdr expr))
               )
           (match expr
             (() (eval-error "wrong number of arguments" def '()))
             ((sym) (eval-error "wrong number of arguments" def sym))
             ((sym args-list-expr body ...)
              (match (%unpack args-list-expr)
                ((args ...)
                 (cond
                  ((symbol? sym)
                   (let*((func   (defun-make-lambda def args body))
                         (name   (symbol->string sym))
                         (=>name (=>env-obarray-key! name))
                         (st     (*the-environment*))
                         (obj    (view st =>name))
                         (obj    (if obj obj
                                     (let ((obj (new-symbol name)))
                                       (lens-set! obj st =>name)
                                       obj)))
                         )
                     (cond
                      ((or (eq? def 'defun) (eq? def 'defsubst)) func)
                      ((eq? def 'defmacro) (lens-set 'macro func =>lambda-kind*!))
                      (else (error "expecting function head to be 'defun or 'defmacro" def))
                      )
                     (cond
                      ((sym-type? obj) ;; update and return the interned symbol object
                       (let ((old-func (view obj =>sym-function*!)))
                         (cond
                          ((lambda-type? old-func)
                           (lambda-copy-into! old-func func))
                          (else
                           (lens-set func obj =>sym-function*!)))
                         obj))
                      (else (eval-error "failed to intern symbol" sym obj)))
                     ))
                  (else (eval-error "wrong type argument" sym 'expecting "symbol"))
                  ))
                (args (eval-error "malformed arglist" args))
                )))))))

    (define (eval-defalias sym-expr val-expr docstr)
      (let*((st (*the-environment*))
            (sym (eval-ensure-interned (eval-form sym-expr)))
            (val (eval-form val-expr))
            (func
             (let loop ((val val))
               (cond
                ((symbol? val)
                 (loop (view st (=>env-obarray-key! (symbol->string val)))))
                ((sym-type? val) (view val =>sym-function*!))
                ((lambda-type? val) val)
                ((procedure? val) val)
                ((command-type? val) val)
                ((pair? val) val)
                (else #f)
                ))))
        (cond
         ((not func) (eval-error "void function" val))
         ((not sym) (eval-error "wrong type argument" sym 'expecting "symbol"))
         (else
          (lens-set! func sym =>sym-function*!)
          ))))

    (define elisp-defalias
      (make<syntax>
       (lambda expr
         (match (cdr expr)
           ((sym-expr val-expr) (eval-defalias sym-expr val-expr #f))
           ((sym-expr val-expr docstr) (eval-defalias sym-expr val-expr docstr))
           (any (eval-error
                 "wrong number of arguments" "defalias"
                 (length any) 'min 2 'max 3
                 ))))))

    (define (set-function-body! func body-exprs)
      ;; This procedure scans through a `BODY-EXPR` for `DECLARE` and
      ;; `INTERACTIVE` forms and both executes them, updating the given
      ;; `FUNC` in place, and also removes them from the `BODY-EXPRS` so
      ;; that the evaluator never executes them directly.
      (define (filter body-exprs)
        (match (%unpack body-exprs)
          (() '())
          ((('declare args ...) body-exprs ...)
           (let loop ((args args))
             (match args
               (() #t)
               (((sym vals ...) args ...)
                (cond
                 ((symbol? sym)
                  (lens-set
                   vals func
                   =>lambda-declares*! (=>hash-key! (symbol->string sym))
                   )
                  (loop args))
                 (else
                  (loop args) ;; ignored malformed declarations
                  )))
               ))
           (filter body-exprs)
           )
          ((('interactive args ...) body-exprs ...)
           (lens-set args func =>lambda-interactive*!)
           (filter body-exprs)
           )
          ((any-expr body-exprs ...) ;; anything else, leave it alone
           (cons any-expr (filter body-exprs)))
          ))
      (lens-set
       (filter body-exprs) ;; <- TODO: this should be (list->elisp-form (filter body-exprs))
       func =>lambda-body*!
       )
      func
      )

    (define (defun-make-lambda kind arg-exprs body-expr)
      (define (non-symbol-error sym)
        (eval-error "invalid function, arguments declaration expect symbol" sym)
        )
      (define (get-body func)
        (match body-expr
          ((docstr body-expr ...)
           (cond
            ((string? docstr)
             (lens-set docstr func =>lambda-docstring*!)
             (set-function-body! func body-expr)
             )
            (else (set-function-body! func (cons docstr body-expr)))
            ))
          (body-expr (set-function-body! func body-expr))
          )
        (let*((capture-vars
               (elisp-form-gather-symbols
                (list->elisp-form (view func =>lambda-body*!))
                string=? symbol->string
                ))
              (restvar (view func =>lambda-rest!))
              (remove-captured
               (lambda (args)
                 (when args
                   (for-each
                    (lambda (arg) (hash-table-delete! capture-vars arg))
                    (cond
                     ((vector? args) (vector->list args))
                     (else args)
                     ))))))
          (remove-captured (view func =>lambda-args!))
          (remove-captured (view func =>lambda-optargs!))
          (when restvar (hash-table-delete! capture-vars restvar))
          (let ((env (*the-environment*)))
            (for-each
             (lambda (name)
               (let ((sym (env-lex-sym-lookup env name)))
                 (cond
                  (sym (hash-table-set! capture-vars name sym))
                  (else (hash-table-delete! capture-vars name))
                  )))
             (hash-table-keys capture-vars)
             )
            (lens-set capture-vars func =>lambda-lexenv!)
            func
            )))
      (define (get-rest-args arg-exprs args optargs)
        (match arg-exprs
          (() (eval-error "invalid function, no argument declaration after &rest delimiter"))
          ((sym)
           (cond
            ((symbol? sym)
             (get-body (new-lambda kind (reverse args) (reverse optargs) sym)))
            (else (non-symbol-error sym))
            ))
          ))
      (define (get-opt-args arg-exprs args optargs)
        (match arg-exprs
          (() (get-body (new-lambda kind (reverse args) (reverse optargs))))
          (('&optional more ...)
           (eval-error "invalid function, more than one &optional argument delimiter" arg-exprs))
          (('&rest arg-exprs ...)
           (get-rest-args arg-exprs args optargs))
          ((sym arg-exprs ...)
           (cond
            ((symbol? sym) (get-opt-args arg-exprs args (cons sym optargs)))
            (else (non-symbol-error sym))
            ))
          ))
      (define (get-args arg-exprs args)
        (match arg-exprs
          (() (get-body (new-lambda kind (reverse args))))
          (('&optional arg-exprs ...) (get-opt-args arg-exprs args '()))
          (('&rest arg-exprs ...) (get-rest-args arg-exprs args '()))
          ((sym arg-exprs ...)
           (cond
            ((symbol? sym) (get-args arg-exprs (cons sym args)))
            (else (non-symbol-error sym))
            ))
          ))
      (get-args (%unpack arg-exprs) '())
      )

    ;;--------------------------------------------------------------------------------------------------
    ;; Procedures that operate on procedures. These are called by the
    ;; evaluator after pattern matching determines that the correct number
    ;; and type of arguments have been supplied.

    (define (view-on-symbol st name/sym viewer)
      (cond
       ((sym-type? name/sym) (viewer name/sym))
       ((symbol/string? name/sym)
        (view st (=>env-obarray-key! (symbol->string name/sym)))
        )
       (else (eval-error "wrong type argument" name/sym 'expecting "symbol"))
       ))

    (define (update-on-symbol st name/sym updater)
      ;; A procedure that operates on a symbol given it's name or the
      ;; `<SYM-TYPE>` object itself.
      (cond
       ((sym-type? name/sym)
        (let-values
            (((obj return) (updater name/sym)))
          return
          ))
       ((symbol/string? name/sym)
        (let-values
            (((obj return)
              (update&view updater st
                           (=>env-obarray-key! (ensure-string name/sym)))))
          return
          ))))


    (define (eval-ensure-interned sym)
      ;; A procedure that returns a <SYM-TYPE> object or creates a new
      ;; interned symbol and returns it.
      ;;------------------------------------------------------------------
      ;; TODO: place this API into (schemacs elisp-eval environment) instead?
      (let ((st (*the-environment*)))
        (cond
         ((symbol? sym)
          (let*((name (symbol->string sym))
                (obj (view st (=>env-obarray-key! name))))
            (cond
             (obj obj)
             (else
              (let ((obj (new-symbol name)))
                (lens-set obj st (=>env-obarray-key! name))
                obj
                )))))
         ((sym-type? sym) sym)
         (else (eval-error "wrong type argument" sym 'expecting "symbol"))
         )))


    (define (eval-make-symbol name) (new-symbol (ensure-string name)))

    (define (eval-symbol-name st sym)
      (view-on-symbol st sym (lambda (obj) (view obj =>sym-name))))

    (define (eval-intern st name)
      (let-values
          (((obj _)
            (env-intern! (lambda (obj) (values obj obj)) st (ensure-string name))))
        obj
        ))

    (define (eval-intern-soft st name)
      (view-on-symbol st name (lambda (obj) obj)))

    (define (eval-unintern st name)
      (update&view
       (lambda (obj) (if (not obj) (values #f #f) (values #f #t)))
       (=>env-obarray-key! name)
       ))

    (define eval-mapatoms
      (case-lambda
        ((func) (eval-mapatoms func (*the-environment*)))
        ((func st)
         (let ((hash (view st =>env-obarray*!)))
           (cond
            ((hash-table? hash)
             (hash-table-for-each (lambda (_key obj) (func obj)) hash)
             #f)
            (else #f)
            )))))

    (define (eval-symbol-plist st sym)
      (update-on-symbol st sym (lambda (obj) (view obj (=>sym-plist! sym)))))

    (define (plist-to-dict plist)
      (define (filter plist)
        (cond
         ((null? plist) '())
         ((pair? plist)
          (let*((assoc (car plist))
                (name (car assoc))
                (name (if (symbol? name) (symbol->string name) name))
                (val (cdr assoc))
                (next (lambda () (filter (cdr plist))))
                )
            (cond
             ((string? name) (cons (cons name val) (next)))
             (else (next))
             )))
         (else '())))
      (cond
       ((hash-table? plist) plist)
       ((pair? plist) (alist->hash-table (filter plist) string=? string-hash))
       (else #f)
       ))

    (define (eval-setplist st name plist)
      ;; Unlike Elisp, the type of `PLIST` is checked and a hash table is
      ;; constructed. Non-alist or non-hash-table values are silently
      ;; ignored.
      (let*((plist (plist-to-dict plist)))
        (when plist
          (update-on-symbol st name (lambda (obj) (lens-set plist obj (=>sym-plist! name))))
          )
        plist
        ))

    (define (eval-set st sym val)
      ;; TODO: check if `SYM` satisfies `SYMBOL?` or `SYM-TYPE?` and act accordingly.
      (update-on-symbol
       st sym
       (lambda (obj)
         (values (lens-set val obj (=>sym-value! (ensure-string sym))) val)
         )))

    (define (eval-get st sym prop)
      (view-on-symbol
       st sym
       (lambda (obj)
         (view obj (=>sym-plist! (ensure-string sym)) (=>hash-key! prop))
         )))

    (define (eval-put st sym prop val)
      (let ((prop (ensure-string prop)))
        (update-on-symbol
         st sym
         (lambda (obj)
           (values
            (lens-set
             val obj
             (if obj =>sym-plist*! (=>sym-plist! (ensure-string sym)))
             (=>hash-key! prop)
             )
            val)
           ))))

    (define (eval-boundp st sym)
      (view-on-symbol st sym (lambda (obj) (not (not (view obj (=>sym-value! sym)))))))

    (define (eval-makunbound st sym)
      (update-on-symbol st sym
                        (lambda (obj) (values (lens-set #f obj (=>sym-value! sym)) obj))))

    (define (eval-symbol-function st sym)
      (view-on-symbol st sym (lambda (obj) (view obj (=>sym-function! sym)))))

    (define (eval-fboundp st sym)
      (not (not (eval-symbol-function st sym))))

    (define (eval-fset st sym func)
      (let ((sym (ensure-string sym)))
        (update-on-symbol st sym
                          (lambda (obj)
                            (values
                             (lens-set (elisp->scheme func) obj (=>sym-function! sym))
                             func
                             )))))

    (define (eval-fmakunbound st sym)
      (view-on-symbol st sym (lambda (obj) (view st (=>env-obarray-key! sym)))))

    (define (|1+| n) (+ n 1))
    (define (|1-| n) (- n 1))

    ;;--------------------------------------------------------------------------------------------------
    ;; Built-in functions

    (define (re-collect-args args)
      ;; The `APPLY` function can take any number of arguments, but it
      ;; must take a list as the final argument. This procedure takes the
      ;; final list argument and appends it to the list of preceeding
      ;; arguments.
      (match args
        (() '())
        (((args ...)) args)
        ((arg (args ...)) (cons arg args))
        ((arg args ...) (cons arg (re-collect-args args)))
        ))

    (define (elisp-apply-or-funcall which collect args)
      (match args
        (() (eval-error "wrong number of arguments" which 'min 1 args))
        ((head args ...)
         ;; The `APPLY` or `FUNCALL` are themselves functions, and
         ;; therefore all of the arguments passed to these functions
         ;; have already been evaluated, so we should not call
         ;; `%ELISP-APPLY` because that would evaluate all of the
         ;; arguments again. Instead we call `PUSH-STACK-FRAME-EVAL-BODY`.
         (let ((func (eval-function-ref head)))
           (cond
            ((lambda-type? func)
             (push-stack-frame-eval-body func (collect (map %unpack args)))
             )
            ((procedure? func) (apply func (collect (map %unpack args))))
            (else (error "internal error, eval-function-ref returned unexpected value" func))
            )))))

    (define (elisp-funcall . args) (elisp-apply-or-funcall "funcall" (lambda (id) id) args))

    (define (elisp-apply . args) (elisp-apply-or-funcall "apply" re-collect-args args))

    (define (eval-function-ref arg)
      (let*((arg (if (elisp-form-type? arg) (elisp-form->list arg) arg))
            (is-lambda? (lambda (o) (and (pair? o) (symbol? (car o)) (eq? 'lambda (car o)))))
            (make-lambda (lambda (o) (apply (syntax-eval elisp-lambda) o)))
            )
        (cond
         ((lambda-type? arg) arg)
         ((symbol? arg)
          (let ((result (view arg =>elisp-symbol!)))
            (cond
             ((sym-type? result) (view result =>sym-function*!))
             ((lambda-type? result) result)
             ((is-lambda? result) (make-lambda result))
             (else arg)
             )))
         ((is-lambda? arg) (make-lambda arg))
         ((procedure? arg) arg)
         (else
          (eval-error "wrong type argument" arg 'expecting "function")
          ))))

    (define elisp-function
      (make<syntax>
       (lambda args
         (match args
           (('function arg) (eval-function-ref arg))
           (any (eval-error "wrong number of arguments" "function" 'expecting 1 'value any))
           ))))


    (define (elisp-symbol-op name type? op)
      (lambda args
        (match args
          ((sym)
           (cond
            ((type? sym)
             (scheme->elisp (op (*the-environment*) sym)))
            (else
             (eval-error
              "wrong type argument" name
              sym 'expecting "symbol")
             )))
          (any
           (eval-error
            "wrong number of arguments" name
            (length any) 'expecting 1))
          )))

    (define (elisp-symbol-op2 name type? op)
      (lambda args
        (match args
          ((sym val)
           (cond
            ((type? sym)
             (scheme->elisp
              (op (*the-environment*) sym (scheme->elisp val))
              ))
            (else
             (eval-error
              "wrong type argument" name
              sym 'expecting "symbol"))
            ))
          (any
           (eval-error
            "wrong number of arguments" name
            (length any) 'expecting 2)
           ))))

    (define (elisp-make-symbol . args)
      (match args
        ((name)
         (cond
          ((symbol/string? name)
           (eval-make-symbol (ensure-string name)))
          (else
           (eval-error "wrong type argument" name 'expecting "symbol or string"))
          ))
        (any
         (eval-error "wrong number of arguments" "make-symbol"
                     (length any) 'expecting 1))
        ))

    (define elisp-symbol-name
      (elisp-symbol-op "symbol-name" any-symbol? eval-symbol-name))

    (define (elisp-bare-symbol . args)
      (match args
        ((sym)
         (cond
          ((sym-type? sym) sym)
          ((symbol? sym)
           (view (*the-environment*) (=>env-symbol! (symbol->string sym)))
           )
          (else (eval-error "wrong type argument" sym 'expecting "symbol"))
          ))
        (any
         (eval-error
          "wrong number of arguments" "bare-symbol"
          (length any) 'expecting 1))
        ))

    (define elisp-boundp
      (elisp-symbol-op "unboundp" any-symbol? eval-boundp))

    (define elisp-makunbound
      (elisp-symbol-op "makunbound" any-symbol? eval-makunbound))

    (define elisp-intern
      (elisp-symbol-op "intern" symbol/string? eval-intern))

    (define elisp-intern-soft
      (elisp-symbol-op "intern-soft" symbol/string? eval-intern-soft))

    (define elisp-unintern
      (elisp-symbol-op "unintern" symbol/string? eval-unintern))

    (define (elisp-mapatoms . args)
      (match args
        ((func) (eval-mapatoms (scheme-lambda->elisp-lambda func)))
        ((func obarray)
         (eval-mapatoms (scheme-lambda->elisp-lambda func) obarray))
        ((any ...)
         (eval-error "wrong number of arguments" "mapatoms" (length any)))
        ))

    (define elisp-symbol-plist
      (elisp-symbol-op "symbol-plist" any-symbol? eval-symbol-plist))

    (define elisp-setplist
      (elisp-symbol-op2 "setplist" any-symbol? eval-setplist))

    (define elisp-set
      (elisp-symbol-op2 "set" any-symbol? eval-set))

    (define elisp-get
      (elisp-symbol-op2 "get" symbol? eval-get))

    (define (elisp-put . args)
      (match args
        ((sym prop val)
         (eval-put (*the-environment*) sym prop val)
         )
        (args
         (eval-error
          "wrong number of arguments" "put"
          (length args) 'expecting 3))
        ))

    (define elisp-symbol-function
      (elisp-symbol-op "symbol-function" symbol? eval-symbol-function))

    (define elisp-fboundp
      (elisp-symbol-op "fboundp" symbol? eval-fboundp))

    (define elisp-fmakunbound
      (elisp-symbol-op "fmakunbound" symbol? eval-fmakunbound))

    (define elisp-fset
      (elisp-symbol-op2 "fset" symbol? eval-fset))

    (define elisp-sxhash-equal (pure 1 "sxhash-equal" default-hash))

    (define elisp-make-hash-table
      (scheme-lambda->elisp-lambda
       (lambda args
         (let loop ((testfunc #f) (weakness #f) (size #f) (args args))
           (match args
             (('|:test|     testfunc args ...) (loop testfunc weakness size args))
             (('|:weakness| weakness args ...) (loop testfunc weakness size args))
             (('|:size|     size     args ...) (loop testfunc weakness size args))
             (()
              ;; TODO: need to do something about the weakness settings,
              ;; this is going to be difficult because not all Scheme
              ;; implementations support hash tables with weak keys and/or
              ;; values.
              (case weakness
                ((key value key-and-value key-or-value)
                 (write-string
                  "WARNING (ELisp): make-hash-table \":weakness\" argument is ignored\n"
                  (current-error-port)
                  ))
                (else (values)))
              (let ((key-compare
                     (case testfunc
                       ((eq) eval-eq)
                       ((eq?) eq?)
                       ((eql) eval-eql)
                       ((eql?) eqv?)
                       ((equal) eval-equal)
                       ((equal?) equal?)
                       (else
                        (cond
                         ((eq? testfunc eq?) eq?)
                         ((eq? testfunc eqv?) eqv?)
                         ((eq? testfunc equal?) equal?)
                         ((eq? testfunc elisp-equal) elisp-equal)
                         ((eq? testfunc elisp-eql) elisp-eql)
                         ((eq? testfunc elisp-eq) elisp-eq)
                         (else #f)
                         )))))
                (cond
                 (key-compare (make-hash-table key-compare))
                 (else
                  (eval-error "invalid hash table test" testfunc)
                  ))))
             ((keyword val args ...)
              (eval-error "invalid keyword argument" keyword)
              )
             (any (eval-error "odd number of arguments"))
             )))))


    (define (elisp-native-comp-function-p . args)
      (match args
        ((obj) #f)
        (any
         (eval-error
          "wrong number of arguments"
          "native-comp-function-p" 'expected 1 any)
         )))

    ;;--------------------------------------------------------------------------------------------------

    (define (elisp-list . args) (map scheme->elisp args))

    (define (elisp-car lst)
      (cond
       ((eq? lst nil) '())
       ((null? lst) '())
       ((pair? lst) (car lst))
       (else (eval-error "wrong type argument" "car" lst))
       ))

    (define (elisp-car-safe lst)
      (cond
       ((pair? lst) (car lst))
       (else '())
       ))

    (define (elisp-cdr lst)
      (cond
       ((eq? lst nil) '())
       ((null? lst) '())
       ((pair? lst) (cdr lst))
       (else (eval-error "wrong type argument" "cdr" lst))
       ))

    (define (elisp-nth n lst)
      (cond
       ((null? lst) #f)
       ((<= n 0) (car lst))
       (else (elisp-nth (- n 1) (cdr lst)))
       ))

    (define (elisp-nconc . args)
      (cond
       ((null? args) '())
       (else
        (let ((head (car args))
              (tail (cdr args))
              )
          (cond
           ((null? tail) head)
           ((null? head) (apply elisp-nconc tail))
           ((pair? head)
            (let find-end ((back '()) (this head) (next (cdr head)))
              (cond
               ((memq this back) (eval-error "list contains a loop" head))
               ((null? next)
                (set-cdr! this (car tail))
                (apply elisp-nconc tail)
                )
               (else
                (find-end (cons back this) next (cdr next))
                ))))
           (else (eval-error "wrong type argument" head))
           )))))

    (define (drop-while compare on-fail elt lst)
      (cond
       ((null? lst) '())
       ((pair? lst)
        (cond
         ((compare elt (car lst))
          (drop-while compare on-fail elt (cdr lst))
          )
         (else lst)
         ))
       (else (on-fail))))

    (define (elisp-delq . args)
      (match args
        ((elt lst0)
         (let ((on-fail
                (lambda ()
                  (eval-error "wrong type argument" "delq" 'expected 'list lst0)
                  )))
           (let loop ((anchor (drop-while eq? on-fail elt lst0)))
             (cond
              ((null? anchor) '())
              ((pair? anchor)
               (let loop ((this anchor) (next (cdr anchor)))
                 (cond
                  ((null? next) anchor)
                  (else
                   (let ((next (drop-while eq? on-fail elt next)))
                     (set-cdr! this next)
                     (cond
                      ((null? next) anchor)
                      (else (loop next (cdr next)))
                      ))))))
              ((elisp-form-type? lst0)
               (loop (elisp-form->list lst0))
               )
              (else (on-fail))
              ))))
        (any (eval-error "wrong number of arguments" "delq" 'expected 2 any))
        ))

    (define (elisp-mapcar . args)
      (match args
        ((func seq)
         (let loop ((seq seq))
           (cond
            ((null? seq) '())
            ((pair? seq)
             (let loop ((seq seq))
               (cond
                ((null? seq) '())
                ((pair? seq)
                 (cons ((scheme-lambda->elisp-lambda func) (car seq))
                       (loop (cdr seq))))
                (else (eval-error "wrong type argument" "mapcar" 'expected "listp" seq))
                )))
            ((elisp-form-type? seq) (loop (elisp-form-tokens seq)))
            ((vector? seq)
             (let ((len (vector-length seq)))
               (let loop ((i 0))
                 (cond
                  ((< i len)
                   (cons ((scheme-lambda->elisp-lambda func) (vector-ref seq i))
                         (loop (+ 1 i))))
                  (else '())
                  ))))
            (else (eval-error "wrong type argument" "mapcar" 'expected "listp" seq))
            )))
        (any (eval-error "wrong number of arguments" "mapcar" 'expected 2 any))
        ))

    (define (eval-eq a b)
      (cond
       ((or (null? a) (eq? a nil) (eq? a 'nil))
        (or (null? b) (eq? b nil) (eq? b 'nil))
        )
       (else (eq? a b))
       ))

    (define (eval-eql a b) (or (eval-eq a b) (eqv? a b)))

    (define (eval-equal a b) (or (eval-eq a b) (equal? a b)))

    (define (%equality fname compare)
      (pure
       2 fname
       (lambda args
         (match args
           ((a b) (compare a b))
           (any (eval-error  "wrong numbet of arguments" fname 'expected 2 any))
           ))))

    (define elisp-eq    (%equality "eq"    eval-eq))
    (define elisp-eql   (%equality "eql"   eval-eql))
    (define elisp-equal (%equality "equal" eval-equal))

    (define (eval-member fname compare)
      (lambda args
        (match args
          ((elt lst)
           (let ((lst
                  (cond
                   ((pair? lst) lst)
                   ((null? lst) lst)
                   ((elisp-form-type? lst) (elisp-form->list lst))
                   (else (eval-error "wrong type argument" 'expected "list" lst))
                   )))
             (member elt lst compare)
             ))
          (any (eval-error "wrong number of arguments" fname 'expected 2 any))
          )))

    (define elisp-memq   (eval-member "memq"   eval-eq))
    (define elisp-member (eval-member "member" eval-equal))

    (define (eval-assq name select)
      (lambda args
        (match args
          ((key alist)
           (let loop ((alist alist))
             (cond
              ((null? alist) '())
              ((pair? alist)
               (let ((head (car alist)))
                 (cond
                  ((and (pair? head) (eval-eq key (select head))) head)
                  (else (loop (cdr alist)))
                  )))
              (else
               (eval-error
                "wrong type argument"
                '(expected . "list") '(got . alist)
                )))))
          (any
           (eval-error
            "wrong number of arguments" name
            '(expected . 2) (cons 'got (length args))
            )))))

    (define elisp-assq (eval-assq "assq" car))
    (define elisp-rassq (eval-assq "rassq" cdr))

    (define elisp-identity
      (lambda args
        (match args
          ((o) o)
          (any
           (eval-error
            "wrong number of arguments" "identity"
            '(expected . 1) (cons 'got (length args))
            )))))

    ;;--------------------------------------------------------------------------------------------------
    ;; Formatting, output, and errors

    (define (elisp-error . args)
      (cond
       ((null? args) (eval-error "Elisp \"error\" occurred"))
       (else (apply eval-error args))
       ))

    (define (elisp-format . args)
      (match args
        ((fstr args ...)
         (cond
          ((string? fstr) (scheme->elisp (apply format fstr args)))
          (else
           (eval-error
            "wrong type argument" fstr
            '(expecting . "string")
            ))))))

    (define (elisp-prin1 . args)
      (match args
        ((val) ((*impl/prin1*) val) val)
        ((val port) ((*impl/prin1*) val port) val)
        ((val port overrides) ((*impl/prin1*) val port overrides) val)
        (any
         (eval-error
          "wrong number of arguments" "prin1"
          (cons 'nargs (length any))
          '(min 1) '(max 3)
          ))))

    (define (elisp-princ . args)
      (match args
        ((val) ((*impl/princ*) val) val)
        ((val port) ((*impl/princ*) val port) val)
        (any
         (eval-error
          "wrong number of arguments"
          "princ"
          '(min . 1) '(max . 2)
          ))))

    (define eval-print
      (case-lambda
        ((val) (eval-print val *elisp-output-port*))
        ((val port)
         (newline port)
         ((*impl/prin1*) val port)
         (newline port)
         )))

    (define (elisp-print . args)
      (match args
        ((val) (eval-print val))
        ((val port) (eval-print val port))
        (any
         (eval-error
          "wrong number of arguments"
          "print"
          '(min 1) '(max 2)))
        ))

    (define (elisp-message . args)
      (match args
        (() (eval-error "wrong number of arguments" "message" 'min 1))
        ((format-str args ...)
         (let ((port (*elisp-error-port*)))
           (apply format-to-port port format-str args)
           (newline port)
           '()
           ))))

    (define (elisp-load . args)
      (match args
        ((filepath)
         (cond
          ((string? filepath) (elisp-load! filepath (*the-environment*)))
          (else (eval-error "wrong type argument" filepath 'expecting "string"))
          ))
        (any
         (eval-error
          "wrong number of arguments" "load"
          (cons 'nargs (length any))
          '(min . 1) '(max . 2)
          ))))

    ;;--------------------------------------------------------------------------------------------------


    (define (elisp-make-keymap . args)
      (match args
        (() (keymap))
        ((name)
         (let ((km (keymap)))
           (lens-set name km =>keymap-label!) km))
        (any
         (eval-error
          "wrong number of arguments"
          "make-keymap" 'min 0 'max 1))
        ))


    (define (elisp-define-key . args)
      (define (define-key keymap key binding remove)
        (let ((binding (if binding binding nil))
              (=>lens (lens =>keymap-top-layer! (=>keymap-layer-index! key)))
              )
          (cond
           ((not (keymap-type? keymap))
            (eval-error "wrong type argument" keymap 'expecting "keymapp"))
           (remove (lens-set #f keymap =>lens))
           (else (lens-set binding keymap =>lens)))
          binding
          ))
      (match args
        ((keymap key binding)
         (define-key keymap key binding #f))
        ((keymap key binding remove)
         (define-key keymap key binding remove))
        (any
         (eval-error
          "wrong number of arguments"
          "define-key" 'min 3 'max 4))
        ))

    ;;--------------------------------------------------------------------------------------------------

    (define elisp-quote
      (make<syntax>
       (lambda args
         (match (cdr args)
           ((expr) expr)
           ((expr extra ...) (eval-error "wrong number of arguments" "quote" extra))
           (any any)
           ))))

    (define elisp-backquote
      (make<syntax>
       (lambda exprs
         (match (cdr exprs)
           (() '())
           ((expr) (eval-backquote expr))
           ((exprs ...) (eval-backquote exprs))
           ))))

    (define *macroexpand-max-depth* 16)

    (define (eval-macroexpander all depth fail-depth)
      (case-lambda
        ((expr) ((eval-macroexpander all depth fail-depth) expr (*the-environment*)))
        ((expr st)
         (let loop ((depth depth) (expr expr))
           (match expr
             (() '())
             ((? elisp-form-type? expr)
              (loop depth (elisp-form->list expr))
              ;; -- ^ do NOT increment depth here. The depth counter is
              ;; only for macro lookups and expansions, this recursion is
              ;; used to unpack part of the AST which does not count
              ;; against the depth limit.
              )
             ((label args ...)
              (let ((args (if all (map (lambda (expr) (loop depth expr)) args) args)))
                (cond
                 ((symbol? label)
                  (let ((func (env-resolve-function st label)))
                    (cond
                     ((and (lambda-type? func)
                           (eq? 'macro (view func =>lambda-kind*!)))
                      (let ((result (push-stack-frame-eval-body func args)))
                        (if (> depth 0)
                            (loop (- depth 1) result)
                            (if fail-depth
                                (eval-error "macro expansion exceeds recursion limit" label args)
                                result
                                ))))
                     (else (cons label args))
                     )))
                 (else (cons label args))
                 )))
             (any any)
             )))))

    (define (elisp-macroexpander all depth fail-depth)
      (lambda expr
        (match expr
          ((expr) ((eval-macroexpander all depth fail-depth) expr))
          ((expr env) ((eval-macroexpander all depth fail-depth) expr env))
          (any (eval-error "wrong number of arguments" "macroexpand" any))
          )))

    (define elisp-macroexpand
      (elisp-macroexpander #f *macroexpand-max-depth* #t))

    (define elisp-macroexpand-1
      (elisp-macroexpander #f 1 #f))

    (define elisp-macroexpand-all
      (elisp-macroexpander #t *macroexpand-max-depth* #t))

    ;;--------------------------------------------------------------------------------------------------

    (define (elisp-debug-print-stack . args)
      (match args
        (() (pretty (print-all-stack-frames (*the-environment*)))
         )
        ((port)
         (cond
          ((eq? port #t)
           (pretty (current-output-port) (print-all-stack-frames (*the-environment*)))
           )
          ((eq? port #f)
           (pretty #f (print-all-stack-frames (*the-environment*)))
           )
          ((output-port-open? port)
           (pretty port (print-all-stack-frames (*the-environment*)))
           )))
        (args (eval-error "wrong number of arguments" "debug-print-stack" args))
        ))

    ;;--------------------------------------------------------------------------------------------------

    (define *elisp-init-env*
      ;; A parameter containing the default Emacs Lisp evaluation
      ;; environment. This environment is an ordinary association list
      ;; mapping strings (or symbols) to values. Any values satisfying the
      ;; predicate `LAMBDA-TYPE?` are automatically interned as functions
      ;; rather than ordinary values.
      ;;------------------------------------------------------------------
      (make-parameter
       `( ;; ---- beginning of association list ----

         ,nil
         ,t
         ,(new-symbol "load-path" '("./elisp/lisp" "./elisp/lisp/emacs-lisp"))
         ,(new-symbol "load-suffixes" '(".el"))
         ,(new-symbol "load-file-rep-suffixes" '("" ".gz"))
         ,(new-symbol "load-file-name")
         ,(new-symbol "noninteractive" #t)
         ,(new-symbol "after-load-functions" '())
         ,(new-symbol "features" '())
         ,(new-symbol "max-lisp-eval-depth" (*max-lisp-eval-depth*))

         (lambda    . ,elisp-lambda)
         (apply    . ,elisp-apply)
         (funcall  . ,elisp-funcall)
         (defun    . ,elisp-defun-defmacro)
         (defsubst . ,elisp-defun-defmacro)
         (defmacro . ,elisp-defun-defmacro)
         (defalias . ,elisp-defalias)
         (defvar   . ,elisp-defvar)
         (function . ,elisp-function)
         (progn    . ,elisp-progn)
         (prog1    . ,elisp-prog1)
         (prog2    . ,elisp-prog2)
         (setq     . ,elisp-setq)
         (let      . ,elisp-let)
         (let*     . ,elisp-let*)

         (cond     . ,elisp-cond)
         (if       . ,elisp-if)
         (when     . ,elisp-when-unless)
         (unless   . ,elisp-when-unless)
         (or       . ,elisp-or)
         (and      . ,elisp-and)
         (while    . ,elisp-while)
         (dotimes  . ,elisp-dotimes)
         (dolist   . ,elisp-dolist)

         (eq       . ,elisp-eq)
         (eql      . ,elisp-eql)
         (equal    . ,elisp-equal)

         (|1+| . ,(pure 1 "1+" |1+|))
         (|1-| . ,(pure 1 "1-" |1-|))
         (+    . ,(pure*-numbers "+" +))
         (-    . ,(pure*-numbers "-" -))
         (*    . ,(pure*-numbers "*" *))
         (=    . ,(pure*-numbers "=" =))
         (<    . ,(pure*-numbers "<" <))
         (<=   . ,(pure*-numbers "<=" <=))
         (>    . ,(pure*-numbers ">" >))
         (>=   . ,(pure*-numbers ">=" >=))

         (cons     . ,(pure 2 'cons cons))
         (car      . ,elisp-car)
         (cdr      . ,elisp-cdr)
         (car-safe . ,elisp-car-safe)
         (list     . ,elisp-list)
         (concat   . ,(pure* string-append))
         (setcar   . ,(pure-raw 2 "setcar" (lambda args (apply set-car! args) #f)))
         (setcdr   . ,(pure-raw 2 "setcdr" (lambda args (apply set-cdr! args) #f)))
         (nth      . ,(pure 2 'nth elisp-nth))
         (nconc    . ,elisp-nconc)
         (delq     . ,elisp-delq)
         (mapcar   . ,elisp-mapcar)
         (memq     . ,elisp-memq)
         (member   . ,elisp-member)
         (assq     . ,elisp-assq)
         (rassq    . ,elisp-rassq)
         (identity . ,elisp-identity)
         (purecopy . ,elisp-identity)

         ,(type-predicate 'null      elisp-null?)
         ,(type-predicate 'consp     elisp-pair?)
         ,(type-predicate 'listp     elisp-list?)
         ,(type-predicate 'stringp   elisp-string?)
         ,(type-predicate 'numberp   elisp-number?)
         ,(type-predicate 'integerp  elisp-integer?)
         ,(type-predicate 'floatp    elisp-float?)
         ,(type-predicate 'functionp elisp-procedure?)
         ,(type-predicate 'symbolp   elisp-symbol?)

         (quote     . ,elisp-quote)
         (backquote . ,elisp-backquote)
         (|`|       . ,elisp-backquote)

         (macroexpand      . ,elisp-macroexpand)
         (macroexpand-1    . ,elisp-macroexpand-1)
         (macroexpand-all  . ,elisp-macroexpand-all)

         (make-symbol      . ,elisp-make-symbol)
         (symbol-name      . ,elisp-symbol-name)
         (bare-symbol      . ,elisp-bare-symbol)
         (boundp           . ,elisp-boundp)
         (makunbound       . ,elisp-makunbound)
         (intern           . ,elisp-intern)
         (intern-soft      . ,elisp-intern-soft)
         (unintern         . ,elisp-unintern)
         (mapatoms         . ,elisp-mapatoms)
         (symbol-plist     . ,elisp-symbol-plist)
         (setplist         . ,elisp-setplist)
         (set              . ,elisp-set)
         (get              . ,elisp-get)
         (put              . ,elisp-put)
         (symbol-function  . ,elisp-symbol-function)
         (fboundp          . ,elisp-fboundp)
         (fmakunbound      . ,elisp-fmakunbound)
         (fset             . ,elisp-fset)
         (declare          . ,elisp-void-syntax) ;; pattern matcher special symbol
         (interactive      . ,elisp-void-syntax) ;; pattern matcher special symbol
         (make-hash-table  . ,elisp-make-hash-table)

         (format           . ,elisp-format)
         (message          . ,elisp-message)
         (format-message   . ,elisp-format)
         (prin1            . ,elisp-prin1)
         (princ            . ,elisp-princ)
         (print            . ,elisp-print)
         (error            . ,elisp-error)

         (eval             . ,elisp-eval!)
         (load             . ,elisp-load)
         (provide          . ,elisp-provide)
         (featurep         . ,elisp-featurep)
         (require          . ,elisp-require)

         (sxhash-equal       . ,elisp-sxhash-equal)
         (make-keymap        . ,elisp-make-keymap)
         (make-sparse-keymap . ,elisp-make-keymap)
         (define-key         . ,elisp-define-key)

         (subr-native-elisp-p    . ,elisp-native-comp-function-p)
         (native-comp-function-p . ,elisp-native-comp-function-p)
         (debug-print-stack .      ,elisp-debug-print-stack)

         (run-hooks                        . ,elisp-run-hooks)
         (run-hooks-with-args              . ,elisp-run-hooks-with-args)
         (run-hook-with-args-until-failure . ,elisp-run-hook-with-args-until-failure)
         (run-hook-with-args-until-success . ,elisp-run-hook-with-args-until-success)
         ;; ------- end of assocaition list -------
         )))

    (define elisp-reset-init-env!
      (case-lambda
        (() (elisp-reset-init-env! #f #f #f))
        ((init-env) (elisp-reset-init-env! init-env #f #f))
        ((init-env env) (elisp-reset-init-env! init-env env #f))
        ((init-env env size)
         (let*((init-env (or init-env (*elisp-init-env*)))
               (size (or size *default-obarray-size*))
               (env (or env (*the-environment*)))
               )
           (env-reset-obarray! env size)
           (env-reset-stack! env)
           (env-alist-defines! env init-env)
           ))))

    (define new-environment
      ;; Construct a new Emacs Lisp environment object, which is a bit
      ;; like an obarray.
      (case-lambda
        (() (new-environment #f #f #f))
        ((inits) (new-environment inits #f #f))
        ((inits size) (new-environment inits size #f))
        ((inits size label)
         (let*((size (if (integer? size) size *default-obarray-size*))
               (inits (if (and inits (pair? inits)) inits (*elisp-init-env*)))
               (env (new-empty-environment size label))
               (errors (elisp-reset-init-env! inits env size))
               )
           (for-each
            (lambda (err) (display ";;Warning, not a declaration: ") (write err) (newline))
            errors
            )
           env
           ))))

    (define elisp-debug-write-obarray
      (case-lambda
        (() (elisp-debug-write-obarray (*the-environment*)))
        ((env) (elisp-debug-write-obarray env (current-output-port)))
        ((env port)
         (let ((i 0))
           (hash-table-for-each
            (lambda (key val)
              (set! i (+ i 1))
              (display
               (cond
                ((< i 10) "    ")
                ((< i 100) "   ")
                ((< i 1000) "  ")
                ((< i 10000) " ")
                (else ""))
               port
               )
              (write i port)
              (display ": " port)
              (write key port)
              (let*((func
                     (and (sym-type? val)
                          (lambda-type? (view val =>sym-value*!))
                          ))
                    (loc (and func (view func =>lambda-location*!)))
                    )
                (when func
                  (display "  ->  " port)
                  (write-parser-location loc port)
                  ))
              (newline port))
            (view env =>env-obarray*!)
            )))))

    (elisp-reset-init-env!)

    ;;----------------------------------------------------------------
    ))

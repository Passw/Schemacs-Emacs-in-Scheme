
(define *the-environment*
  (make-parameter (new-empty-environment *default-obarray-size*)))

(define debug-print-eval #f)
  ;; ^ WARNING: setting this to #t prints every single form that is
  ;; evaluated, even long `PROGN` expressions hundreds of lines
  ;; long. Be sure that when you set this to #t you are only going to
  ;; execute a very small example.

(define (elisp-write-stack-frames)
  (let ((st (*the-environment*)))
    (pretty
     (print
      (print-stack-frame st)
      "==== stack frames ================"
      (line-break)
      (print-all-stack-frames st)))))


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
     '=>elisp-symbol!))
  )


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
               (macro-type? val))
           (lens-set val sym =>sym-function*!))
          (else (lens-set val sym =>sym-value*!)))
         ))
     assocs)))


(define (new-elisp-raise-impl env halt-eval)
  ;; This is a constructor for a procedure that raises an exception
  ;; when an exception occurs in the Emacs Lisp evaluator. The
  ;; procedure returned by this constructor should be used to
  ;; parameterize the `RAISE-ERROR-IMPL*` API from the
  ;; `(SCHEMACS ELISP-EVAL ENVIRONMENT)` library.
  ;;
  ;; The implementation returned by this procedure takes an error
  ;; object to be raised, it then updates the error object with a
  ;; stack trace taken from the `ENV` argument applied to this
  ;; constructor. The updated error object is then applied to the
  ;; `HALT-EVAL` procedure applied to this constructor. The
  ;; `HALT-EVAL` procedure is typically constructed with `CALL/CC`
  ;; which forces computation to resume at the point in the program
  ;; where `CALL/CC` was applied, returning the error object.
  ;;
  ;; Use this constructor when you want to modify the behavior of the
  ;; Emacs Lisp evaluator with regard to how it throws exceptions.
  ;;------------------------------------------------------------------
  (let ((env (or env (*the-environment*))))
    (lambda (err-obj)
      (cond
       ((elisp-eval-error-type? err-obj)
        (lens-set
         (env-get-stack-trace env)
         err-obj =>elisp-eval-error-stack-trace
         )
        (halt-eval err-obj)
        )
       (else
        (halt-eval err-obj)
        )))))

(define handle-scheme-exceptions*
  ;; This is a parameter that can alter the behavior of the Elisp
  ;; evaluator when an exception is raised by Scheme, not by Elisp. If
  ;; this parameter is set to `#F` (the default), the Scheme exception
  ;; is handled, the Elisp interpreter is set into a consistent state,
  ;; and then the Scheme exception is re-raised -- when running in the
  ;; REPL, this will trigger the Scheme REPL execption handler and
  ;; (usually) allow you to inspect the backtrace and debug the
  ;; interpreter. If you set this to `#t` the exception is handled but
  ;; not re-raised (effectively, ignored in the REPL).
  ;;
  ;; If you set this parameter to hold a procedure, the procedure will
  ;; be applied with two arguments when a Scheme exception occurs
  ;; during Emacs Lisp evaluation:
  ;;
  ;;  1. the continuation that can optionally be applied a single
  ;;     arbitrary argument (such as an error object) to signal that
  ;;     the error condition has been handled, and
  ;;
  ;;  2. the Scheme error object that was caught by the Elisp
  ;;     exception handler.
  ;;------------------------------------------------------------------
  (make-parameter #f))

(define new-elisp-error-handler
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
         (write-elisp-eval-error err-obj env port)
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
        (let ((handler (new-elisp-error-handler env halt-eval))
              (raise-impl (new-elisp-raise-impl env halt-eval))
              )
          (parameterize ((raise-error-impl* raise-impl))
            (with-exception-handler handler
              (lambda () (eval-form expr (env-get-location expr)))
              ))))))
  (define (run-with-mode new-lxmode)
    (let*((env (*the-environment*))
          (old-lxmode (view env =>env-lexical-mode?!))
          )
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
    (let ((new-env (new-empty-environment)))
      (env-push-new-elstkfrm! new-env #f env)
      (parameterize ((*the-environment* new-env)) (run-with-mode #t))
      ))
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
    ((expr env) (%elisp-eval! expr env))))


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


(define elisp-load!
  (case-lambda
    ((filepath)
     (elisp-load! filepath (*the-environment*))
     )
    ((filepath env)
     (let*((name "load-file-name")
           (port (open-input-file filepath))
           )
       (define (setq-load-file-name val)
         (lens-set val env
          (=>env-obarray-key! name)
          (=>sym-value! name))
         )
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
                             (eval-iterate-forms env parst
                              (lambda (form)
                                (%elisp-eval! form env)
                                )))))))))
                 )
             (exec-run-hooks 'after-load-functions (list filepath))
             (setq-load-file-name nil)
             (lens-set old-dialect env =>env-lexical-mode?!)
             result
             )))))))


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
            (hook
             (%elisp-apply func args-list))
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
;; The interpreting evaluator. Matches patterns on the program and
;; immediately executes each form or symbol. The entrypoint is
;; `EVAL-FORM`.

(define (%unpack expr)
  (cond
   ((elisp-form-type? expr) (elisp-form->list expr))
   (else expr)
   ))

(define (eval-push-new-elstkfrm! size bindings)
  ;; Inspect the lexical binding mode and push a new stack frame on
  ;; the appropriate stack (lexical or dynamic stack). Return the
  ;; empty stack frame that was pushed so it can be updated by the
  ;; calling procedure.
  (env-push-new-elstkfrm! (*the-environment*) size bindings))


(define (eval-pop-elstkfrm!)
  (env-pop-elstkfrm! (*the-environment*)))


(define elisp-eval-reset-stack!
  (case-lambda
    (() (elisp-eval-reset-stack! (*the-environment*)))
    ((env) (env-reset-stack! env))
    ))


(define (push-stack-frame-eval-body func args)
  ;; This applies arguments to a `FUNC` which must be of type
  ;; `LAMBDA-TYPE?`, without evaluating the arguments `ARGS`. It
  ;; creates and pushes a stack frame with the `ARGS` as they are
  ;; given.
  ;;------------------------------------------------------------------
  (let ((elstkfrm (elstkfrm-from-args func args)))
    (cond
     ((not elstkfrm) (error "elstkfrm-from-args returned #f"))
     ((elisp-eval-error-type? elstkfrm) (eval-raise elstkfrm))
     (else
      (let*((st (*the-environment*))
            (old-stack (view st =>env-lexstack*!))
            (st (lens-set (list elstkfrm) st =>env-lexstack*!))
            (return (eval-progn-body (view func =>lambda-body*!))) ;; apply
            )
        (lens-set old-stack st =>env-lexstack*!)
        return
        )))))


(define (scheme-lambda->elisp-lambda func)
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
  ;; ------------------------------------------------------------------
  (lambda args
    (cond
     ((lambda-type? func)
      (cond
       ((eq? 'macro (view func =>lambda-kind*!))
        (push-stack-frame-eval-body func args)
        )
       (else (%elisp-apply func args))
       ))
     ((procedure? func)
      (scheme->elisp (apply func (map elisp->scheme args))))
     ((command-type? func)
      (scheme->elisp (apply (command-procedure func) (map elisp->scheme args))))
     (else (eval-error "wrong type argument" func 'expecting "function"))
     )))


(define %elisp-apply
  ;; This is the actual `APPLY` procedure for Emacs Lisp. The `HEAD`
  ;; argument is resolved to a procedure, macro, command, or lambda.
  ;; If `HEAD` resolves to a lambda, and `LAMBDA-KIND` is `'MACRO`,
  ;; the macro is expanded by evaluating the result with `eval-form`
  ;; before a value is returned.
  (case-lambda
    ((head arg-exprs) (%elisp-apply head arg-exprs #f))
    ((head arg-exprs loc)
     (when debug-print-eval
       (display "; apply ") (write head) (newline)
       (display "; - args: ") (write arg-exprs) (newline)
       )
     (let*((st (*the-environment*))
           (func (env-resolve-function st head))
           (func (cond
                  ((pair? func) (eval-form func))
                  ((elisp-form-type? func) (eval-form func (env-get-location func)))
                  (else func)
                  ))
           (sym  (if (lambda-type? head) 'lambda head))
           (loc  (or loc (and (lambda-type? func) (view func =>lambda-location*!))))
           )
       (when debug-print-eval
         (display "; - resolved function ") (write func) (newline)
         )
       (env-trace!
        loc sym func st
        (lambda ()
          (cond
           ((syntax-type?  func)
            (apply (syntax-eval func) head arg-exprs))
           ((lambda-type?  func)
            (cond
             ((macro-type? func)
              (eval-form
               (push-stack-frame-eval-body func arg-exprs)
               (env-get-location func)
               ))
             (else
              (push-stack-frame-eval-body func (eval-args-list arg-exprs))
              )))
           ((command-type? func)
            (apply (command-procedure func) (eval-args-list arg-exprs))
            )
           ((procedure?    func)
            (apply func (eval-args-list arg-exprs))
            )
           ((pair?         func)
            (match func
              (('lambda arg-exprs body ...)
               (let ((func (apply (syntax-eval elisp-lambda) func)))
                 (push-stack-frame-eval-body
                  (defun-make-lambda 'lambda (%unpack arg-exprs) body)
                  (eval-args-list arg-exprs))
                 ))
              (('macro . func)
               (cond
                ((lambda-type? func)
                 (eval-form
                  (push-stack-frame-eval-body func arg-exprs)
                  (env-get-location func)
                  ))
                (else (eval-error "invalid macro" 'expected 'lambda 'actual func))
                ))
              (any (eval-error "invalid function" func))
              ))
           (else (eval-error "invalid function" head))
           )))))))


(define eval-form
  ;; This is where evaluation begins. This is the actual `EVAL`
  ;; procedure for Emacs Lisp.
  (case-lambda
    ((expr) (eval-form expr #f))
    ((expr loc)
     (when debug-print-eval
       (display "; eval ") (write expr) (newline)
       )
     (match expr
       (() '())
       ((head args ...) (%elisp-apply head args loc))
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
            (eval-backquote (elisp-unquote-scheme literal))
            )
           (else (elisp-unquote-scheme literal))
           ))
         ((elisp-form-type? literal)
          (cond
           ((square-bracketed-form? literal)
            (elisp-form->vector literal)
            )
           (else
            (eval-form
             (elisp-form->list literal)
             (elisp-form-start-loc literal)
             ))))
         ((elisp-function-ref-type? literal)
          (eval-function-ref (elisp-function-get-ref literal))
          )
         (else literal)
         ))))))


(define (eval-args-list arg-exprs)
  (let ((result
         (let loop ((exprs arg-exprs))
           (match exprs
             (() '())
             ((expr more ...)
              (let ((result (eval-form expr (env-get-location arg-exprs))))
                (if (elisp-eval-error-type? result) result
                    (cons result (loop more)))
                ))))))
    (when debug-print-eval
      (display "; - args-list ") (write result) (newline)
      )
    result
    ))


(define (eval-progn-body body-exprs)
  (let loop ((exprs body-exprs))
    (match exprs
      (() '())
      ((final) (eval-form final (env-get-location final)))
      ((head more ...)
       (eval-form head (env-get-location head))
       (loop more)
       )
      (exprs (error "no function body" exprs))
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
                (let*((st (*the-environment*))
                      (elstkfrm (env-push-new-elstkfrm! st 1 '()))
                      (obj (elstkfrm-sym-intern! elstkfrm (symbol->string var) 0))
                      )
                  (let loop ((n 0))
                    (cond
                     ((>= n limit)
                      (let ((return (eval-progn-body final-exprs)))
                        (env-pop-elstkfrm! st)
                        return
                        ))
                     (else
                      (lens-set n obj =>sym-value*!)
                      (eval-progn-body body)
                      (loop (+ n 1))
                      )))))))
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
                   (elstkfrm (env-push-new-elstkfrm! st 1 '()))
                   (obj (elstkfrm-sym-intern! elstkfrm (symbol->string var) 0))
                   )
               (let loop ()
                 (cond
                  ((cursor-end? cur)
                   (env-pop-elstkfrm! st)
                   (final))
                  (else
                   (lens-set (cursor-ref cur) obj =>sym-value*!)
                   (eval-progn-body body)
                   (cursor-step! cur)
                   (loop)
                   )))))
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
               (env-push-new-elstkfrm! st size (reverse bound))
               (let ((result (eval-progn-body progn-body)))
                 (env-pop-elstkfrm! st)
                 result
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
          (let*((bindings (%unpack bindings-form))
                (elstkfrm (env-push-new-elstkfrm! st (length bindings) '()))
                )
            (let loop ((unbound-exprs bindings))
              (match unbound-exprs
                (()
                 (let ((result (eval-progn-body progn-body)))
                   (env-pop-elstkfrm! st)
                   result))
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
                 )))))
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
            )))
        )
    (cond
     ((not func) (eval-error "void function" val))
     ((not sym) (eval-error "wrong type argument" sym 'expecting "symbol"))
     (else
      (lens-set! func sym =>sym-function*!))
     )))


(define elisp-defalias
  (make<syntax>
   (lambda expr
     (match (cdr expr)
       ((sym-expr val-expr) (eval-defalias sym-expr val-expr #f))
       ((sym-expr val-expr docstr) (eval-defalias sym-expr val-expr docstr))
       (any
        (eval-error "wrong number of arguments" "defalias"
         (length any) 'min 2 'max 3))
       ))))


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
              (lens-set vals
               func =>lambda-declares*! (=>hash-key! (symbol->string sym)))
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
   (filter body-exprs)
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
         func)
        (else
         (set-function-body! func (cons docstr body-expr))
         func)))
      (body-expr
       (set-function-body! func body-expr)
       func)
      ))
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
  (get-args (%unpack arg-exprs) '()))

;;--------------------------------------------------------------------------------------------------
;; Procedures that operate on procedures. These are called by the
;; evaluator after pattern matching determines that the correct number
;; and type of arguments have been supplied.

(define (view-on-symbol st name/sym viewer)
  (cond
   ((sym-type? name/sym) (viewer name/sym))
   ((symbol/string? name/sym)
    (view st (=>env-obarray-key! (symbol->string name/sym))))
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
      (update-on-symbol st name (lambda (obj) (lens-set plist obj (=>sym-plist! name)))))
    plist
    ))

(define (eval-set st sym val)
  ;; TODO: check if `SYM` satisfies `SYMBOL?` or `SYM-TYPE?` and act accordingly.
  (update-on-symbol st sym
   (lambda (obj)
     (values (lens-set val obj (=>sym-value! (ensure-string sym))) val))))

(define (eval-get st sym prop)
  (view-on-symbol st sym
   (lambda (obj)
     (view obj (=>sym-plist! (ensure-string sym)) (=>hash-key! prop)))))

(define (eval-put st sym prop val)
  (let ((prop (ensure-string prop)))
    (update-on-symbol
     st sym
     (lambda (obj)
       (values
        (lens-set val obj
         (if obj =>sym-plist*! (=>sym-plist! (ensure-string sym)))
         (=>hash-key! prop))
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
                   ((eq) eq?)
                   ((eql) eqv?)
                   ((equal) equal?)
                   (else #f)
                   )))
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
        ((elisp-form-type? seq) (loop (elisp-form->list seq)))
        ((vector? seq) (seq (vector->list seq)))
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

(define (eval-equal a b) (or (eval-eq a b) (equal? a b)))

(define (%equality fname compare)
  (lambda args
    (match args
      ((a b) (compare a b))
      (any (eval-error  "wrong numbet of arguments" fname 'expected 2 any))
      )))

(define elisp-eq    (%equality "eq"    eval-eq))
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
      (else (eval-error "wrong type argument" fstr 'expecting "string"))
      ))
    ))

(define (elisp-prin1 . args)
  (match args
    ((val) ((*impl/prin1*) val) val)
    ((val port) ((*impl/prin1*) val port) val)
    ((val port overrides) ((*impl/prin1*) val port overrides) val)
    (any
     (eval-error
      "wrong number of arguments" "prin1"
      (length any) 'min 1 'max 3))
    ))

(define (elisp-princ . args)
  (match args
    ((val) ((*impl/princ*) val) val)
    ((val port) ((*impl/princ*) val port) val)
    (any (eval-error "wrong number of arguments" "princ" 'min 1 'max 2))
    ))

(define eval-print
  (case-lambda
    ((val) (eval-print val *elisp-output-port*))
    ((val port)
     (newline port)
     ((*impl/prin1*) val port)
     (newline port))
    ))

(define (elisp-print . args)
  (match args
    ((val) (eval-print val))
    ((val port) (eval-print val port))
    (any (eval-error "wrong number of arguments" "print" 'min 1 'max 2))
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
     (eval-error "wrong number of arguments" "load"
      (length any) 'min 1 'max 2))
    ))

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

(define (eval-backquote . exprs)
  (match exprs
    ((exprs)
     (let expr-loop ((exprs (%unpack exprs)))
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
       (match exprs
         (() '())
         ((('|,| unq) exprs ...)
          (cons (eval-form (expr-loop unq)) (expr-loop exprs))
          )
         ((('|,@| splice) exprs ...)
          (splice-loop (eval-form (expr-loop splice)) exprs)
          )
         (((sub-exprs ...) exprs ...)
          (cons (expr-loop sub-exprs) (expr-loop exprs))
          )
         ((elem exprs ...)
          (cond
           ((elisp-form-type? elem)
            (cons
             (expr-loop (elisp-form->list #t elem))
             (expr-loop exprs)
             ))
           ((elisp-unquoted-form-type? elem)
            (cond
             ((elisp-spliced-form? elem)
              (splice-loop (eval-form (elisp-unquoted-get-form elem)) exprs)
              )
             (else
              (cons
               (eval-form (elisp-unquoted-get-form elem))
               (expr-loop exprs)
               ))))
           (else (cons elem (expr-loop exprs)))
           ))
         (elem elem)
         )))
   (any (eval-error "wrong number of arguments" "backquote" any))
   ))

(define elisp-backquote
  (make<syntax> (lambda exprs (apply eval-backquote (cdr exprs)))))

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
          (let ((args
                 (if all
                     (map (lambda (expr) (loop depth expr)) args)
                     args)))
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

(define *elisp-init-env*
  ;; A parameter containing the default Emacs Lisp evaluation
  ;; environment. This environment is an ordinary association list
  ;; mapping strings (or symbols) to values. Any values satisfying the
  ;; predicate `LAMBDA-TYPE?` are automatically interned as functions
  ;; rather than ordinary values.
   ;;------------------------------------------------------------------
  (make-parameter
   `(;; ---- beginning of association list ----

     ,nil
     ,t
     ,(new-symbol "load-path" '("./elisp/lisp" "./elisp/lisp/emacs-lisp"))
     ,(new-symbol "load-suffixes" '(".el"))
     ,(new-symbol "load-file-rep-suffixes" '("" ".gz"))
     ,(new-symbol "load-file-name")
     ,(new-symbol "noninteractive" #t)
     ,(new-symbol "after-load-functions" '())
     ,(new-symbol "features" '())

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

     (eq       . ,(pure 2 "eq" elisp-eq))
     (equal    . ,(pure 2 "equal" elisp-equal))

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

     (run-hooks                        . ,elisp-run-hooks)
     (run-hooks-with-args              . ,elisp-run-hooks-with-args)
     (run-hook-with-args-until-failure . ,elisp-run-hook-with-args-until-failure)
     (run-hook-with-args-until-success . ,elisp-run-hook-with-args-until-success)
     ;; ------- end of assocaition list -------
     )))


(define elisp-reset-init-env!
  (case-lambda
    (() (elisp-reset-init-env! (*elisp-init-env*)))
    ((init-env) (elisp-reset-init-env! init-env (*the-environment*)))
    ((init-env env) (env-alist-defines! env init-env))))


(define new-environment
  ;; Construct a new Emacs Lisp environment object, which is a bit
  ;; like an obarray.
  (case-lambda
    (() (new-environment #f))
    ((inits) (new-environment inits #f))
    ((inits size)
     (let*((size (if (integer? size) size *default-obarray-size*))
           (inits (if (and inits (pair? inits)) inits (*elisp-init-env*)))
           (env (new-empty-environment size))
           (errors (elisp-reset-init-env! inits env))
           )
       (for-each
        (lambda (err) (display ";;Warning, not a declaration: ") (write err) (newline))
        errors)
       env))))


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

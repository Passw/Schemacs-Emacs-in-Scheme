(define-library (schemacs record-type)
  ;; This library provides a simple interface for defining declarative
  ;; record-type constructors.
  ;;------------------------------------------------------------------
  (import
    (scheme base)
    (scheme case-lambda)
    (only (scheme lazy)  delay  promise?  force)
    )
  (export
   record-updater-type?
   record-updater  record-type-tag  run-record-updater
   new-constructor
   )
  (begin

    (define-record-type <record-updater-type>
      ;; A constructor is a procedure `PROC` that takes a value of a
      ;; type predicate and returns a value for that same type
      ;; predicate. The `FOR` argument **must** be a type predicate
      ;; defined and **must** return #t` for the argument consumed and
      ;; returned by `PROC`. For example, suppose you are defining a
      ;; constructor which takes a `SIZE` parameter, you can define
      ;; `SIZE` as a `RECORD-UPDATER` that sets the size of some
      ;; object. You can then use this record-updating constructor as
      ;; an argument to any procedure that was defined using
      ;; `NEW-CONSTRUCTOR`.
      ;;
      ;; ```
      ;; (define (size s)
      ;;   (record-updater
      ;;    object-type?
      ;;    (lambda (object) (set!object-size object s))
      ;;    ))
      ;; ```
      ;;--------------------------------------------------------------
      (record-updater for proc)
      record-updater-type?
      (for  record-type-tag)
      (proc run-record-updater)
      )

    (define new-constructor
      ;; This is the method constructor, i.e. a procedure that
      ;; constructs a method and it's relationship to a record
      ;; type. The method returned by this procedure takes an
      ;; arbitrary number of arguments values that can update a value
      ;; of some type, starting with a type produced by calling the
      ;; `INIT-VAL` thunk with no arguments.
      ;;
      ;; If an argument value to the resultant constructor satisfies
      ;; the `CONSTRUCRTOR-TYPE?` predicate (usually defined by
      ;; `RECORD-UPDATER`) then `CONSTRUCT-FOR?` is applied to
      ;; `IN-CLASS?` and the constructor to ensure the constructor for
      ;; a value that satisfies `IN-CLASS?`, and then the
      ;; `RUN-RECORD-UPDATER` procedure is applied the current value
      ;; being constructed.
      ;;
      ;; If any argument value is `#f` it is ignored, this allows for
      ;; guards to return `#f` when an argument value is safe to
      ;; ignore, allowing for a way to have optional arguments.
      ;;
      ;; If an argument value is neither `#f` nor a
      ;; `RECORD-UPDATER-TYPE?`, then the `INIT-VAL` and this argument
      ;; value are both applied to `OTHER-TYPE-HANDLER`, which must
      ;; return a value that satisfies the `IN-CLASS?` predicate.
      ;;
      ;; If the `COMPLETION` procedure is provided, it is called on
      ;; the constructed value after all other constructors have been
      ;; evaluated.
      ;;--------------------------------------------------------------
      (case-lambda
        ((in-class? init-val) (new-constructor in-class? init-val #f #f))
        ((in-class? init-val other-type-handler)
         (new-constructor in-class? init-val other-type-handler #f)
         )
        ((in-class? init-val other-type-handler completion)
         (lambda args
           (let ((init-val
                  (cond
                   ((promise? init-val) init-val)
                   ((procedure? init-val) (delay (init-val)))
                   (else
                    (error
                     "init-val not a procedure or promise value"
                     init-val
                     )))))
             (let loop ((val (force init-val)) (args args))
               (cond
                ((null? args) (if completion (completion val) val))
                (else
                 (let ((constr (car args)))
                   (cond
                    ((not constr) (loop val (cdr args)))
                    ((record-updater-type? constr)
                     (cond
                      ((in-class? (record-type-tag constr))
                       (loop ((run-record-updater constr) val) (cdr args))
                       )
                      (else
                       (error
                        "value cannot be used to initialize"
                        in-class? constr
                        ))))
                    ((procedure? other-type-handler)
                     (let ((val (other-type-handler val constr)))
                       (loop val (cdr args))
                       ))
                    (else
                     (error
                      "value cannot be used to initialize"
                      in-class? constr
                      ))))))))))))

    ;;----------------------------------------------------------------
    ))

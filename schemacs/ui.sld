(define-library (schemacs ui)
  ;; This library defines the primitive elements of an abstract 2D
  ;; GUI. The elements are a "DIV" somewhat similar to that of the Web
  ;; browser platform's "Document Object Model" (DOM).
  ;;
  ;;   - Each "Div" (short for "Division" or "subdivision" of a
  ;;     2D-plane) is a plane bounded by a width and height, offset in
  ;;     2D space by some offset vector, and has a layer index (also
  ;;     sometimes called a Z-index).
  ;;
  ;;   - Every `DIV` has a slot for arbitrary content, including a
  ;;     subdivision structure making a tree. Every `DIV` can be
  ;;     subdivided either by a grid mesh, or by box packing oriented
  ;;     along a horizontal or vertical axis (but not both, and not
  ;;     along the same axis as it's parent).
  ;;
  ;;   - Every `DIV` has a slot for a pointer to some implementation
  ;;     specific data. In this way, each DIV can contain a pointer to
  ;;     an implementation-specific widget tree. This makes the `DIV`
  ;;     tree similar to a "virtual DOM" such as React, Vue, or
  ;;     Van.js.
  ;;
  ;;   - Unlike the Web DOM, whether subdivisions have borders,
  ;;     padding, or margins is not in the scope of this API, it must
  ;;     be attached to the arbitrary content slot.
  ;;
  ;;   - The units for `DIV` sizes and offsets are completely
  ;;     arbitrary, it could be pixels, centimeters, or characters on
  ;;     a video terminal.
  ;;
  ;;   - Parent `DIV` elements containing other child `DIV` elements
  ;;     can choose whether to "contain" the child elements, meaning
  ;;     if child elements are larger than the containing parent
  ;;     `DIV`, the parent `DIV` can mask-off all parts of the child
  ;;     elements that do not fit in the rectangle of the parent.
  ;;
  ;;   - There are generic algorithms in this library for computing
  ;;     rectangle sizes, but need not be used, the sizes can be
  ;;     computed by the implementation-specific widget library.
  ;;
  ;;   - There are generic algorithms to compute which `DIV` element
  ;;     contains a point, such as for mouse clicks, but the
  ;;     implementation-specific widget library can provide this
  ;;     functionality as well.
  ;;
  ;; It is expected that implementations which use this library will
  ;; provide functionality to translate a `DIV` tree sturcture into an
  ;; implementation-specific widget tree. The implementation must
  ;; install event handlers to update the tree in response to touch,
  ;; mouse-click, or keyboard events.
  ;;
  ;; This library is designed to allows Scheamcs users to programming
  ;; user interfaces in Scheme with a somewhat consistent API to
  ;; program declarative user interfaces.
  ;;------------------------------------------------------------------
  (import
    (scheme base)
    (scheme case-lambda)
    (only (scheme write) display write)
    (only (schemacs record-type)
          record-updater-type?
          record-updater  record-type-tag  run-record-updater
          new-constructor
          )
    (only (schemacs vbal) ;;(schemacs vbal)
          vbal-type?  alist->vbal  vbal-copy  vbal-assq
          vbal-length  vbal-for-each
          print-vbal-with
          )
    (only (schemacs ui rectangle)
          rect2D-type?  rect2D  copy-rect2D  print-rect2D
          size2D-type?  size2D  copy-2D      rect2D-size
          point2D  point2D-type?  print-point2D
          )
    (only (schemacs lens)
          record-unit-lens  lens  update
          =>view-only-lens  =>canonical
          )
    (only (schemacs pretty)
          pretty  print  qstr  line-break  form
          indent-by  join-by  join-lines
          display-lines
          )
    )
  (export
   state-var-type?  use-vars-type?
   state-var  use-vars  get-var  update-var  signal-var
   =>state-var-value*!  div-event-handler  use-vars-value
   ;;------------------------------------------------------------------
   div-type?   div-record-type?  div   copy-div  print-div
   div-parent  div-view-type  div-content  div-from-var?
   div-on-update  div-equality-test
   view-type  on-update  equality-test  widget   properties  content
   =>div-widget*!  =>div-on-delete*!  =>div-on-update*!
   =>div-rect*!  =>div-properties*!
   div-prop-lookup  prop-lookup  variable-size?  expand  enclose
   for-each-div  draw-div-tree  div-run-update  div-delete
   *default-copy-widget-ref*
   ;;------------------------------------------------------------------
   div-monad-type?  run-div-monad
   ;;------------------------------------------------------------------
   div-grid-type? div-grid  x-sizes  y-sizes
   row-major  column-major  grid-elems  increasing  decreasing
   div-grid-x-sizes  div-grid-y-sizes  div-grid-subdivs
   for-each-div-grid
   ;;------------------------------------------------------------------
   div-pack-type?   div-pack  div-pack-subdivs  div-pack-subdiv-sizes
   pack-elem   div-pack-flags  div-pack-flag?  wrapping
   cut-orientation?  cut-horizontal  cut-vertical
   div-pack-orientation  align-vertical  align-horizontal
   pack-direction?  from-start  from-end  from-middle  div-pack-from
   for-each-div-pack
   ;;------------------------------------------------------------------
   div-space-type?  div-space  div-space-elements
   floater-type?  floater  floater-div  floater-z-index  floater-rect
   div-space-inner-align  inner-align
   div-space-outer-align  outer-align
   for-each-div-space  for-each-floater
   ;;------------------------------------------------------------------
   push-button  check-box  text-input  radio-group  labeled-group
   text-editor  canvas  composite  tiled-windows
   )

  (begin

    (define-record-type <div-monad-type>
      ;; This is a monad for constructing the `DIV` tree.
      ;;--------------------------------------------------------------
      (make<div-monad> proc)
      div-monad-type?
      (proc div-monad-proc)
      )

    (define (%%run-div-monad log-output parent m)
      (when log-output               ;;LOG
        (display "PROC " log-output) ;;LOG
        (write m log-output)         ;;LOG
        (newline log-output)         ;;LOG
        )                            ;;LOG
      ((div-monad-proc m) log-output parent)
      )

    (define (%run-div-monad log-output parent o)
      (cond
       ((div-monad-type? o)
        (div-resolve log-output o (%%run-div-monad log-output parent o))
        )
       ((use-vars-type? o)
        (let ((return (apply-use-vars log-output o)))
          (cond
           ((div-monad-type? return)
            (when log-output                    ;;LOG
              (display "VAR->PROC " log-output) ;;LOG
              (write return log-output)         ;;LOG
              (newline log-output)              ;;LOG
              )                                 ;;LOG
            (div-resolve
             log-output return
             (%%run-div-monad log-output parent return)
             )
            ((use-vars-type? return)
             (error "`use-vars` node resolved to another `use-vars` node" return 'from: o)
             ))
           (else (div-resolve log-output o return))
           )))
       (else (error "not a `div-monad-type?` value" o))
       ))

    (define run-div-monad
      ;; This procedure takes a procedure of type `div-monad-type?`
      ;; and constructs a `DIV` tree. It takes three arguments:
      ;;
      ;;  1. (optional), and need not be passed as an argument at all,
      ;;     but if a value is given it must be an `output-port?` for
      ;;     logging the construction of the tree object, useful for
      ;;     debugging.
      ;;
      ;;  2. (optional) the parent `DIV` tree to which the new `DIV`
      ;;     tree will be grafted.
      ;;
      ;;  3. (required) the `div-monad-type?` that was constructed by
      ;;     one of `div`, `div-pack`, `div-space`, or `floater`. This
      ;;     is what gets evaluated to produce the `DIV` tree. It is
      ;;     also possible to pass a `use-vars-type?` for this
      ;;     argument as well.  The variables will be collected and
      ;;     applied to the use-vars node's internal updating function
      ;;     to produce a `DIV` node or DIV monad, if a DIV monad is
      ;;     the result, the `DIV` monad is evaluated to produce a
      ;;     `DIV` node.
      ;;--------------------------------------------------------------
      (case-lambda
        ((o) (%run-div-monad #f #f o))
        ((parent o)
         (cond
          ((output-port? parent) (%run-div-monad parent #f o))
          (else (%run-div-monad #f parent o))
          ))
        ((log-output parent o) (%run-div-monad log-output parent o))
        ))

    (define (div-resolve log-output from o)
      ;; Ensure that the value `O` has resolved to a `DIV` node, and
      ;; is not a `use-vars-type?` or `div-monad-type?`. The `FROM`
      ;; argument should be the `use-vars-type?` or `div-monad-type?`
      ;; that produced the argument `O`.
      (cond
       ((or (div-record-type? o) (floater-type? o))
        (when log-output                   ;;LOG
          (pretty                          ;;LOG
           log-output                      ;;LOG
           (print                          ;;LOG
            "DIV "                         ;;LOG
            (indent-by 4 (%print-div 0 o)) ;;LOG
            (line-break)                   ;;LOG
            )))                            ;;LOG
        o)
       ((or (not o) (string? o) (number? o))
        (when log-output              ;;LOG
          (display "DIV " log-output) ;;LOG
          (write o log-output)        ;;LOG
          (newline log-output)        ;;LOG
          )                           ;;LOG
        (%content->div o)
        )
       ((use-vars-type? o)
        (error "resolved to use-vars rather than `DIV` node" o 'from: from)
        )
       ((div-monad-type? o)
        (error "recursive div constructor" o 'from: from)
        )
       (else
        (error "resolved to unknown type" o 'from: from)
        )))

    ;;================================================================

    (define-record-type <state-var-type>
      (make<state-var> label equality-test value subscribers)
      state-var-type?
      (label          state-var-label          set!state-var-label)
      (equality-test  state-var-equality-test  set!state-var-equality-test)
      (value          state-var-value          set!state-var-value)
      (subscribers    state-var-subscribers    set!state-var-subscribers)
      )

    (define (print-state-var var)
      (form
       1 "state-var"
       (qstr (state-var-label var))
       (qstr (state-var-equality-test var))
       (qstr (state-var-value var))
       ))

    (define =>state-var-value*!
      (record-unit-lens state-var-value set!state-var-value '=>state-var-value*!)
      )

    (define (var-subscribe state-var var-user)
      (set!state-var-subscribers
       state-var (cons var-user (state-var-subscribers state-var))
       ))

    (define *push-to-var-list*
      ;; This parameter is set to contain a box for every event
      ;; handler that can call `WITH-STATE`. Every time `WITH-STATE`
      ;; is called, the state variable that is updated is placed into
      ;; a list in the box in this parameter. When the `WITH-STATE`
      ;; procedure finished executing, this list is checked for
      ;; modified state variables, and all of the subscribers to those
      ;; variables are updated.
      (make-parameter #f)
      )

    (define (%update-var test-eq var update-proc)
      (cond
       ((not (state-var-type? var))
        (error "not a state-var-type" var)
        )
       (else
        (let ((update-proc
               (cond
                ((procedure? update-proc) update-proc)
                ((div-monad-type? update-proc)
                 (lambda (old-value) update-proc))
                (else (error "not a procedure" update-proc))
                )))
          ;; return a thunk
          (lambda args
            (let ((push-to-var-list (*push-to-var-list*)))
              (cond
               ((procedure? push-to-var-list)
                (let*((old-val (state-var-value var))
                      (new-val (apply update-proc old-val args))
                      )
                  (set!state-var-value var new-val) ;; Always update the state value.
                  (unless (and test-eq ((state-var-equality-test var) old-val new-val))
                    ;; Enqueue this state var to dispatch updates,
                    ;; unless the values are equal (or if equality
                    ;; testing has been explicitly disabled) in which
                    ;; case there is no need to enqueue DIV tree
                    ;; update.
                    (push-to-var-list var)
                    )
                  (values)
                  ))
               (else
                (error
                 "update-var thunk not called within a `DIV-EVENT-HANDLER` context"
                 var update-proc
                 )))))))))

    (define (update-var var update-proc)
      ;; When it comes time to modify a `STATE-VAR` in response to an
      ;; event, you must use this procedure to specify which
      ;; `STATE-VAR` you want to update, and you must specify an
      ;; `UPDATE-PROC` procedure which will be the current value of
      ;; the `STATE-VAR` and is expected to return a new value to be
      ;; stored into the `STATE-VAR`. The equality test will compare
      ;; the new and old value of `STATE-VAR` and if different an
      ;; update to any `USE-VARS` monads will be enqueued to update
      ;; the view.
      ;;------------------------------------------------------------------
      (%update-var #t var update-proc)
      )

    (define (signal-var var update-proc)
      ;; This procedure is similar to `update-var`, except that the
      ;; `state-var-type?` `VAR` that is applied must contain a `DIV`
      ;; node value, and the `UPDATE-PROC` is expected to perform a
      ;; stateful update on the `DIV` node and trigger a redraw in the
      ;; GUI. Updates are always dispatched, no equality testing is
      ;; performed on the variable value.
      ;;------------------------------------------------------------------
      (%update-var #f var update-proc)
      )

    (define (div-run-update old new)
      (let ((update (or (div-on-update old) (div-on-update new))))
        (when update (update old new))
        new
        ))

    (define (%div-event-handler log-output proc)
      ;; This procedure is used to implemnent the Schemacs UI
      ;; semantics in lower-level GUI toolkits. Whenever an event
      ;; handler is installed, for example into a push button, that
      ;; event handler should evaluate it's code in a thunk passed to
      ;; this procedure. This proceudre will begin doing the book
      ;; keeping for whichever `STATE-VAR` are updated by `USE-VARS`
      ;; during the course of evaluating the event handling code
      ;; assigned to the widgets.
      ;;
      ;; The `DIV-EVENT-HANDLER` procedure takes a thunk `PROC` which
      ;; takes zero arguments. When the `DIV-EVENT-HANDLER` procedure
      ;; itself returns, the result of `PROC` is returned. The `PROC`
      ;; must return exacly 1 value of any type at all.
      ;;
      ;; The list of updated variables is used to call the procedure
      ;; stored in the `*DIV-UPDATER*` parameter on every instance of
      ;; `USE-VARS` which needs to update it's content. The procedure
      ;; stored in the `*DIV-UPDATER*` parameter simply takes two
      ;; arguments: the old `DIV` from before `USE-VARS` procedure was
      ;; applied, and the new `DIV` produced by the latest call to
      ;; `USE-VARS`.
      ;;--------------------------------------------------------------
      (let ((box '()))
        (parameterize
            ((*push-to-var-list*
              (lambda (val) (set! box (cons val box)))
              ;; A function to put updated state variables into the above box.
              ))
          ;; Run event handler `PROC`, may call `UPDATE-VAR` with many vars.
          (let ((result (proc)))
            ;; `BOX` now contains a list of all state vars that were
            ;; updated, so for each state var, dispatch updates on all
            ;; `USE-VARS` instances that are subscribed each state var.
            (let loop ((box (reverse box)))
              (cond
               ((null? box) result)
               (else
                (let ((stvar (car box)))
                  (div-dispatch-updates log-output div-run-update stvar)
                  (loop (cdr box))
                  ))))))))

    (define div-event-handler
      (case-lambda
        ((proc) (%div-event-handler #f proc))
        ((log-output proc) (%div-event-handler log-output proc))
        ))

    (define (div-dispatch-updates log-output div-run-update state-var)
      ;; After an update to state variables, every `USES-VAR` instance
      ;; in the DIV tree that is subscribed to those variables needs
      ;; to be evaluated again with the new values of the state variables.
      ;; This procedure is usually called from the 
      (let loop ((var-users (state-var-subscribers state-var)))
        (cond
         ((null? var-users) (values))
         (else
          (when log-output                     ;;LOG
            (pretty                            ;;LOG
             log-output                        ;;LOG
             (print                            ;;LOG
              "EVENT "                         ;;LOG
              (print-use-vars (car var-users)) ;;LOG
              (line-break)                     ;;LOG
              )))                              ;;LOG
          (let*((usevar (car var-users))
                (old-value (use-vars-value usevar))
                (new-value (apply-use-vars log-output usevar))
                )
            (div-run-update old-value new-value)
            (loop (cdr var-users))
            )))))

    (define state-var
      ;; Construct a new state variable. Takes 0, 1, 2, or 3
      ;; arguments, all optional:
      ;;
      ;; - () :: variable is initalized to #f which uses `equal?` to
      ;;   test if the variable's value has changed.
      ;;
      ;; - (init-value) :: initialized to the given `init-value`.
      ;;
      ;; - (equal init-value) :: initialized to the given `init-value`
      ;;   and using `equal` as the equality test, which triggers a
      ;;   DIV tree update when the state variable's value is updated
      ;;   to a new value that is not equal to the previous value.
      ;;
      ;; - (label init-value) :: initialized to the given `init-value`
      ;;   and has a `label` used to identify this state variable in a
      ;;   group of variables.
      ;;
      ;; - (label equal init-value) :: variable has an identifying
      ;;   `label`, an equality test `equal`, and is initialized to
      ;;   the given `init-value`.
      ;;--------------------------------------------------------------
      (case-lambda
        (() (state-var #f equal? #f))
        ((init-value) (state-var #f equal? init-value))
        ((lbl/eq init-value)
         (cond
          ((or (symbol? lbl/eq) (string? lbl/eq) (integer? lbl/eq))
           (state-var equal? lbl/eq init-value)
           )
          ((procedure? lbl/eq)
           (state-var lbl/eq equal? init-value)
           )
          (else (error "not an identifier or equality predicate" lbl/eq))
          ))
        ((label equality-test init-value)
         (make<state-var> label (or equality-test equal?) init-value '())
         )))

    ;;----------------------------------------------------------------

    (define-record-type <use-vars-type>
      (make<use-vars> vars proc val)
      use-vars-type?
      (vars    use-vars-state-vars)
      (proc    use-vars-procedure)
      (val     use-vars-value  set!use-vars-value)
      )

    (define (print-use-vars vars)
      (form
       1 "use-vars"
       (apply join-lines (map print-state-var (use-vars-state-vars vars)))
       (line-break)
       (qstr (use-vars-procedure vars))
       ))

    (define (use-vars state-vars proc)
      ;; This is a DIV element constructor that reads the content of a
      ;; state variable, or a list of state variables, and applies
      ;; these values to a procedure which uses that content to
      ;; construct and return a child DIV. It is very important to use
      ;; `USE-DIV`s to construct content because this will register
      ;; the `DIV` as a subscriber to the state variable and the DIV
      ;; will be notified of changes to the state variable, triggering
      ;; updates when the variable changes.
      ;;--------------------------------------------------------------
      (define (check-state-vars n vars)
        (cond
         ((null? vars)
          (if (< 0 n) #t
              (error "requires at least one <state-var-type>")
              ))
         ((pair? vars)
          (let ((var (car state-vars)))
            (cond
             ((state-var-type? var)
              (check-state-vars (+ 1 n) (cdr state-vars))
              )
             (else (error "not a state variable" var))
             )))
         (else (error "requires a list of state variables" vars))
         ))
      (cond
       ((not
         (or (state-var-type? state-vars)
             (check-state-vars 0 state-vars)
             ))
        (error "not a <state-var-type>" state-vars)
        )
       ((not (procedure? proc))
        (error "not a procedure" state-vars)
        )
       (else
        (let ((usevar (make<use-vars> state-vars proc #f))) ;;TODO
          (let loop ((state-vars state-vars))
            (cond
             ((null? state-vars) usevar)
             (else
              (let ((stvar (car state-vars)))
                (var-subscribe stvar usevar)
                (loop (cdr state-vars))
                ))))))))

    (define (get-var state-var)
      ;; Similar to `USE-VARS`, except it takes only one state var and
      ;; returns it's value as-is without applying it to any
      ;; procedure.  So the value of the state variable must be either
      ;; a `DIV`, a call to a div monad combinator such as `DIV`,
      ;; `DIV-PACK`, `DIV-SPACE`, or `DIV-GRID`, or some value that
      ;; can be translated to a `DIV`.
      ;;------------------------------------------------------------------
      (use-vars (list state-var) state-var-value)
      )

    (define (apply-use-vars log-output uses)
      (let loop ((vars (use-vars-state-vars uses)) (args '()))
        (cond
         ((pair? vars)
          (loop (cdr vars) (cons (state-var-value (car vars)) args))
          )
         ((null? vars)
          (when log-output                               ;;LOG
            (display "VAR " log-output)                  ;;LOG
            (write (use-vars-procedure uses) log-output) ;;LOG
            (newline log-output)                         ;;LOG
            )                                            ;;LOG
          (let*((return (apply (use-vars-procedure uses) (reverse args)))
                (return
                 (cond
                  ((div-monad-type? return)
                   (%run-div-monad log-output #f return)
                   )
                  (else return)
                  )))
            (set!use-vars-value uses return)
            (cond
             ((div-record-type? return) (set!div-from-var return #t))
             ((floater-type? return) (set!floater-from-var return #t))
             (else (values))
             )
            return
            ))
         (else (error "not a proper list of variables" vars))
         )))

    ;;================================================================

    (define-record-type <div-type>
      ;; It is a tree of this data structure that constructs the
      ;; virtual widget tree. It is constructed by the `div`
      ;; constructor which is actually a monadic function that
      ;; constructs this tree.
      ;;--------------------------------------------------------------
      (make<div> parent view-type props on-update equal widget delete content rect from-var)
      div-record-type?
      (parent      div-parent         set!div-parent)
      (view-type   div-view-type      set!div-view-type)
      (props       div-properties     set!div-properties)
      (on-update   div-on-update      set!div-on-update)
      (equal       div-equality-test  set!div-equality-test)
      (widget      div-widget         set!div-widget)
      (delete      div-on-delete      set!div-on-delete)
      (content     div-content        set!div-content)
      (rect        div-rect           set!div-rect)
      (from-var    div-from-var?      set!div-from-var)
      )

    (define (div-type-check check-cont-type)
      (lambda (o)
        (or (and (div-record-type? o) (check-cont-type (div-content o)))
            (check-cont-type o)
            )))
 
    (define (%content->div o) (make<div> #f #f #f #f equal? #f #f o #f #f))

    (define (%copy-div copy-widget-ref)
      (lambda (o)
        (cond
         ((not o) o)
         ((div-record-type? o)
          (make<div>
           (div-parent o)
           (div-view-type o)
           (copy-2D (vbal-copy (div-properties o)))
           (div-on-update o)
           (div-equality-test o)
           (div-widget o)
           (div-on-delete o)
           (div-content o)
           (copy-2D (div-rect o))
           (div-from-var? o)
           ))
         ((div-grid-type?  o) (%content->div (copy-div-grid  copy-widget-ref o)))
         ((div-pack-type?  o) (%content->div (copy-div-pack  copy-widget-ref o)))
         ((div-space-type? o) (%content->div (copy-div-space copy-widget-ref o)))
         (else (error "cannot copy value" o))
         )))

    (define copy-div
      ;; This procedure creates a copy of it's first argument which
      ;; must be a `<DIV-TYPE>`. The first argument may also be a
      ;; content type such as `<DIV-PACK-TYPE>`, `<DIV-GRID-TYPE>`, or
      ;; `<DIV-SPACE-TYPE>` but in this case a blank `<DIV-TYPE>` is
      ;; constructed to contain the content type and the `<DIV-TYPE>`
      ;; value is returned.
      ;;
      ;; Provide an optional second argument to copy the
      ;; `WIDGET-REF` field of `<WIDGET-TYPE>` values, if this second
      ;; argument is not applied, the `WIDGET-REF` is copied in such a
      ;; way that the reference may be aliased across multiple `DIV`
      ;; trees, which may not desirable. It is also possible to
      ;; override widget copying with the `*DEFAULT-COPY-WIDGET-REF*
      ;; parameter, although the copy procedure applied as the second
      ;; argument to this procedure takes precedent.
      ;;
      ;; NOTE: this procedure is not a monad, so you **do not** need
      ;; to run it with `RUN-DIV-MONAD`.
      ;;--------------------------------------------------------------
      (case-lambda
        ((o) ((%copy-div (*default-copy-widget-ref*)) o))
        ((o copy-widget-ref) ((%copy-div copy-widget-ref) o))
        ))

    (define (write-div-content depth cont port)
      (indent-by depth port)
      (write-string "(content " port)
      (write cont port)
      (write-char #\) port)
      )

    (define (%print-div depth o)
      (cond
       ((floater-type? o) (print-floater o))
       ((div-record-type? o)
        (let*-values
            (((cont) (div-content o))
             ((constr is-shallow print-content)
              (cond
               ((div-pack-type?  cont) (values "div-pack"  #f print-div-pack))
               ((div-grid-type?  cont) (values "div-grid"  #f print-div-grid))
               ((div-space-type? cont) (values "div-space" #f print-div-space))
               (else (values "div" #t (lambda (_ o) (and o (qstr o)))))
               ))
             ((props) (div-properties o))
             ((vtype) (div-view-type o))
             ((len) (and props (vbal-length props)))
             )
          (form
           1 constr
           (cond
            ((div-pack-type? cont)
             ;; Special rule here to write the div-pack flags on the same
             ;; line as the "div-pack" constructor.
             (apply
              join-by #\space (div-pack-orientation cont)
              (let ((flags (div-pack-flags cont)))
                (let loop
                    ((flags
                      (cons
                       (div-pack-from cont)
                       (or (and (not flags) '()) flags)
                       )))
                  (cond
                   ((pair? flags)
                    (cons (car flags) (loop (cdr flags)))
                    )
                   (else (list (line-break)))
                   )))))
            (else #f)
            )
           (and vtype (print (print-div-view-type vtype) (line-break)))
           ;; now write the properties list
           (and props
                (print
                 (form
                  1 "properties" (line-break)
                  (print-vbal-with
                   (lambda (key val)
                     (print
                      (if (symbol? key) (print #\' key) (qstr key))
                      "  " (qstr val) (line-break)
                      ))
                   props
                   ))
                 (line-break)
                 ))
           ;; now write the content
           (if (or is-shallow (not depth) (> depth 0))
               (print-content (and (not is-shallow) depth (- depth 1)) cont)
               "..."
               ))))
       (else (print 0))
       ))

    (define print-div
      (case-lambda
        ((o) (%print-div #f o))
        ((depth o) (%print-div depth o))
        ))

    (define expand
      ;; keyword: `EXPAND` used as argument to `SIZE2D` procedure,
      ;; indicates that this div should expand to fill the space
      ;; available to it.
      ;;--------------------------------------------------------------
      'expand)

    (define enclose
      ;; Keyword: `ENCLOSE` used as an argument to the `SIZE2D` procedure,
      ;; indicates that this div should be calculated by the size of
      ;; all elements contained within it.
      ;;--------------------------------------------------------------
      'enclose)

    (define (variable-size? a)
      (or (eq? a expand) (eq? a enclose))
      )

    (define (equality-test eq-proc)
      ;; Use this to initialize a `DIV` with a comparison procedure
      ;; such as `EQUAL?`, which compares the current version of the
      ;; `DIV`'s `content` with a new, updated `DIV`'s content to
      ;; decide whether to trigger the `on-update` procedure. If a
      ;; `DIV` is not initialized with an `EQUALITY-TEST`, the default
      ;; equality test is `EQUAL?`.
      ;;--------------------------------------------------------------
      (record-updater
       div-record-type?
       (lambda (o) (set!div-equality-test o eq-proc) o)
       ))

    (define (view-type a)
      (record-updater
       div-record-type?
       (lambda (o)
         (cond
          ((not (div-view-type o)) (set!div-view-type o a) o)
          (else (error "view-type defined more than once" (div-view-type o) a))
          ))))

    (define (on-update proc)
      ;; When using this to initialize a `DIV`, you can override the
      ;; default behavior defined by `div-run-update`. If a new `DIV`
      ;; is determined to have content that is different from an old
      ;; `DIV` in the same position of the `DIV` tree, usually the
      ;; `div-run-update` procedure is called, but if `DIV` has it's
      ;; own `on-update` procedure, the `PROC` applied to this
      ;; constructor, then theold `DIV` and the new `DIV` are applied
      ;; (in that order) as arguments to the `PROC` given here. IT is
      ;; expected that `PROC` should return a `DIV`, usually the new
      ;; `DIV` with some modification made to it.
      ;;--------------------------------------------------------------
      (cond
       ((procedure? proc)
        (record-updater
         div-record-type?
         (lambda (o) (set!div-on-update o proc) o)
         ))
       (else (error "div-monad procedure type is requred" proc))
       ))

    (define =>div-widget*!
      (record-unit-lens div-widget set!div-widget '=>div-widget*!)
      )

    (define =>div-on-delete*!
      (record-unit-lens div-on-delete set!div-on-delete '=>div-on-delete*!)
      )

    (define =>div-on-update*!
      (record-unit-lens div-on-update set!div-on-update '=>div-on-update*!)
      )

    (define =>div-properties*!
      (record-unit-lens div-properties set!div-properties '=>div-properties*!)
      )

    (define =>div-rect*!
      ;; For a computed rectangle, this is area computed most
      ;; recently.
      ;;--------------------------------------------------------------
      (record-unit-lens div-rect set!div-rect '=>div-rect*!)
      )

    (define (content init)
      ;; Initializes the content of a `DIV` to contain an arbitrary
      ;; value `INIT`. Ordinarily you initialize the content of a
      ;; `DIV` using one of the combinators such as `DIV-GRID`,
      ;; `DIV-PACK`, `DIV-SPACE`, or `DIV-CLIP`. But the `CONTENT`
      ;; constructor allows the `DIV` to contain any value at all.
      ;;--------------------------------------------------------------
      (record-updater
       div-record-type?
       (lambda (o)
         (let ((old (div-content o)))
           (cond
            ((not old) (set!div-content o init))
            (else (error "initialized content more than once" old o))
            )
           o
           ))))

    (define (widget wref)
      ;; This initializes the widget reference from the
      ;; platform-specific GUI implementation for this particular
      ;; `DIV`. This is typically not used in the `div` constructor
      ;; itself, rather it is used during the drawing pass after the
      ;; tree has been fully constructed. See also: the `draw-div-tree`
      ;; API.
      ;;--------------------------------------------------------------
      (record-updater
       div-record-type?
       (lambda (o)
         (let ((old-wref (div-widget o)))
           (cond
            ((not old-wref) (set!div-widget o wref) o)
            (else (error "widget reference specified more than once" old-wref wref))
            )))))

    (define *default-copy-widget-ref*
      ;; This parameter contains a procedure which copies the
      ;; `WIDGET-REF` field of a widget when the `COPY-DIV` procedure
      ;; is applied. If not parameterized, the reference is simply
      ;; aliased, i.e. the reference itself is shallow-copied and may
      ;; set the widget in more than one place in the `DIV` tree,
      ;; which may not always be desirable.
      ;;--------------------------------------------------------------
      (make-parameter (lambda (ref) ref))
      )

    (define (prop-lookup sym props)
      (and props
           (let ((pair (vbal-assq sym props)))
             (and pair (cdr pair))
             )))

    (define (div-prop-lookup sym o)
      (prop-lookup sym (div-properties o))
      )

    (define (properties . elems)
      ;; An declarative way to define properties, the arguments
      ;; applied to this procedure should be given in pairs, with as
      ;; many pairs as necessary. The first of the pair must always
      ;; be a `SYMBOL?` value, and the second can be any arbitrary
      ;; value depending on the `DIV` that is being constructed. 
      ;;
      ;; The properties of a `DIV` are usually platform specific,
      ;; although there are a few "built-in" properties that every GUI
      ;; implementation is expected to provide, such as `forecolor:`,
      ;; `backcolor:`, `type:` and `on-button-pressed:`,
      ;; `on-input-char:`, `on-cursor-move:`, `on-enter-pressed:`, and
      ;; so on. Notice that by convention these property symbols
      ;; always end with a colon.
      ;;
      ;; GUI implementations should ignore properties from the
      ;; property list if they are not recognized, because these
      ;; properties may be defined for other GUI implementations --
      ;; implementations which might even be running in the same
      ;; process, for example a Gtk+ based GUI and an ANSI
      ;; video-terminal-based GUI.
      ;;------------------------------------------------------------------
      (alist->vbal
       (let loop ((elems elems))
         (cond
          ((null? elems) '())
          (else
           (let*((key (car elems))
                 (elems (cdr elems))
                 (val (and (not (null? elems)) (car elems)))
                 (elems (if (null? elems) '() (cdr elems)))
                 )
             (cons (cons key val) (loop elems))
             ))))))

    (define (put-content o cont)
      (cond
       ((div-content o)
        (error "content defined more than once" (div-content o) cont)
        )
       (else (set!div-content o cont) o)
       ))

    (define (div-common-constr o init)
      ;; Return `o` with appropriate field `set!` to `INIT` if `INIT`
      ;; is a type that can be used to set the content of a `DIV`,
      ;; otherwise return `#f`, indicating that `init` may be useful
      ;; to some other constructor.
      (cond
       ((vbal-type? init)
        ;; TODO: merge properties if they have already been defined
        (set!div-properties o init)
        o)
       (else #f)
       ))

    (define div
      (new-constructor
       (lambda (tag) (eq? tag div-record-type?))
       (lambda () (make<div> #f #f #f #f equal? #f #f #f #f #f))
       (lambda (o init)
         (or (div-common-constr o init)
             (cond
              ((string? init) (put-content o init) o)
              ((number? init) (put-content o (number->string init)) o)
              ((div-monad-type? init) (put-content o init) o)
              ((use-vars-type? init) (put-content o init))
              ((div-pack-type? init)
               (put-content o (copy-div-pack (*default-copy-widget-ref*) init)) o
               )
              ((div-grid-type? init)
               (put-content o (copy-div-grid (*default-copy-widget-ref*) init)) o
               )
              ((div-space-type? init)
               (put-content o (copy-div-space (*default-copy-widget-ref*) init)) o
               )
              (else (error "cannot initialze div with value of type" init))
              )))
       (lambda (o) ;; <- finalize
         (make<div-monad>
          (lambda (log-output parent)
            (set!div-parent o parent)
            (let ((cont (div-content o)))
              (cond
               ((div-monad-type? cont)
                (set!div-content o (%run-div-monad log-output parent cont))
                o)
               (else o)
               )))))))

    (define (div-type? a)
      (or (not a) (div-record-type? a)
          (use-vars-type? a)
          (grid-record-type? a)
          (pack-record-type? a)
          (space-record-type? a)
          ))

    (define (vector-fill-list! vec i elems)
      (cond
       ((and (< i (vector-length vec)) (pair? elems))
        (vector-set! vec i (car elems))
        (vector-fill-list! vec (+ 1 i) (cdr elems))
        )
       (else vec)
       ))

    (define (construct-content-list log-output parent cast elems)
      ;; Step through a list of div-monad procedures, evaluate each
      ;; one, and gather it's content into a list. Return two values:
      ;; the length of the list, and the list of `DIV` elements.
      ;;--------------------------------------------------------------
      (define count 0)
      (define (resolve-elem elem)
        (cond
         ((not elem) #f)
         (else
          (let*((return
                 (cond
                  ((or (div-monad-type? elem) (use-vars-type? elem))
                   (%run-div-monad log-output parent elem)
                   )
                  ((or (div-record-type? elem) (floater-type? elem)) elem)
                  (else (error "not a valid div constructor" elem))
                  ))
                (return (cast return))
                )
            (set! count (+ 1 count))
            return
            ))))
      (define (gather-vec resume next vec)
        (let ((len (vector-length vec)))
          (let loop ((i 0))
            (cond
             ((< i len)
              (let ((result (resolve-elem (vector-ref vec i)))
                    (i (+ 1 i))
                    )
                (if result (cons result (loop i)) (loop i))
                ))
             (else (resume next))
             ))))
      (define (gather-list resume next elems)
        (let loop ((elems elems))
          (cond
           ((null? elems) (resume next))
           ((pair? elems)
            (let ((result (resolve-elem (car elems)))
                  (elems (cdr elems))
                  )
              (if (cons result (loop elems)) (loop elems))
              ))
           (else (error "not a list" elems))
           )))
      (define (loop elems)
        (cond
         ((null? elems) '())
         ((pair? elems)
          (let*((elem (car elems))
                (next (cdr elems))
                )
            (cond
             ((pair? elem) (gather-list loop next elem))
             ((vector? elem) (gather-vec loop next elem))
             (else (cons (resolve-elem elem) (loop next)))
             )))
         ((vector? elems) (gather-vec loop '() elems))
         (else (error "not a list" elems))
         ))
      (let*((divs (loop elems)))
        (values count divs)
        ))

    (define (construct-content log-output parent cast elems)
      (let*-values
          (((count divs) (construct-content-list log-output parent cast elems))
           ((vec) (make-vector count))
           )
        (vector-fill-list! vec 0 divs)
        vec
        ))

    (define (for-each-div proc)
      ;; A curried procedure which takes a `PROC` and a `<DIV-TYPE>`,
      ;; applies `PROC` to every child `<DIV-TYPE>` in the
      ;; `DIV-CONTENT` of the given `<DIV-TYPE>`.
      ;;--------------------------------------------------------------
      (lambda (div)
        (let ((content (div-content div)))
          (cond
           ((div-grid-type?  content)
            ((for-each-div-grid  (lambda (div x y w h) (proc div))) content)
            )
           ((div-pack-type?  content)
            ((for-each-div-pack  (lambda (div i w) (proc div))) content)
            )
           ((div-space-type? content)
            ((for-each-div-space (lambda (flo) (proc (floater-div flo)))) content)
            )
           (else (values))
           )
          (proc div)
          )))

    (define (diff-div diff-action old new)
      (let*((test-equal (or (div-equality-test old) equal?))
            (old-cont (div-content old))
            (new-cont (div-content new))
            (updater  (div-on-update old))
            (both (lambda (type?) (and (type? old-cont) (type? new-cont))))
            (step-in (lambda (diff) (diff diff-action old-cont new-cont)))
            )
        (cond
         ((not (eq? (div-view-type old) (div-view-type new))) (diff-action old new))
         ((both div-grid-type?)  (step-in diff-div-grid))
         ((both div-pack-type?)  (step-in diff-div-pack))
         ((both div-space-type?) (step-in diff-div-space))
         ((and (both div-record-type?) (test-equal old-cont new-cont)) old)
         (updater (updater old new))
         (else (diff-action old new))
         )))

    (define diff-div-vector 
      (case-lambda
        ((diff-action old new)
         (diff-div-vector diff-action (lambda (id) id) old new)
         )
        ((diff-action get-div old new)
         (let ((old-len (and old (vector-length old))))
           (cond
            ((and (vector? old) (vector? new)
                  (= old-len (vector-length new))
                  )
             (let loop ((i old-len))
               (cond
                ((< i old-len)
                 (diff-div
                  diff-action
                  (get-div (vector-ref old i))
                  (get-div (vector-ref new i))
                  ))
                (else (values))
                ))
             new
             ))))))

    (define (div-delete o)
      (let*((on-delete (div-on-delete o))
            (cont (div-content o))
            )
        (cond
         ((div-grid-type?  cont) (div-grid-delete  o cont))
         ((div-pack-type?  cont) (div-pack-delete  o cont))
         ((div-space-type? cont) (div-space-delete o cont))
         (else (values))
         )
        (when on-delete (on-delete o))
        o))

    (define (draw-div-tree o . constrs)
      ;; This procedure is used by the platform-specific GUI
      ;; implementation to set the `div-widget` and `div-rect` field
      ;; of a `DIV` after the implementation-specific widget reference
      ;; has been constructed. Pass a `DIV` as the first argument,
      ;; apply the `widget` record-updater with the widget reference,
      ;; and a `rect2D` value indicating the actual geometric size of
      ;; the rectangle allocated for the widget.
      ;;--------------------------------------------------------------
      (apply
       (new-constructor
        (lambda (tag) (eq? tag div-record-type?))
        (lambda () o)
        (lambda (o val)
          (cond
           ((rect2D-type? val) (set!div-rect o val) o)
           (else (error "div has no record field for value of this type" val))
           ))
        (lambda (o) o))
       constrs
       ))

    ;;================================================================

    (define-record-type <div-grid-type>
      (make<div-grid> x-sizes y-sizes subdivs)
      grid-record-type?
      (x-sizes   div-grid-x-sizes   set!div-grid-x-sizes)
      (y-sizes   div-grid-y-sizes   set!div-grid-y-sizes)
      (subdivs   div-grid-subdivs   set!div-grid-subdivs)
      )

    (define div-grid-type? (div-type-check grid-record-type?))

    (define (copy-div-grid copy-ref o)
      (make<div-grid>
       (vector-map copy-2D (div-grid-x-sizes o))
       (vector-map copy-2D (div-grid-y-sizes o))
       (vector-map (%copy-div copy-ref) (div-grid-subdivs o))
       ))

    (define (make-grid-rec-init setter)
      (let ((not-enough "must have at least 1 grid element size specified"))
        (lambda (sizes)
          (record-updater
           div-grid-type?
           (lambda (o)
             (let ((grid (div-content o)))
               (cond
                ((vector? sizes)
                 (cond
                  ((< 0 (vector-length sizes)) (setter grid sizes) o)
                  (else (error not-enough sizes))
                  ))
                ((pair? sizes) (setter grid (list->vector sizes)) o)
                ((null? sizes) (error not-enough sizes))
                (else
                 (error "value of this type cannot initialize grid size list" sizes)
                 ))))))))

    (define x-sizes
      ;; This is a record initializer for `DIV-GRID`, it takes a list
      ;; or vector argument, each element containing a number or else
      ;; `'ENCLOSE` or `'EXPAND`. These values indicate the size (or
      ;; how to compute the size) of columns, i.e. divisions
      ;; perpendicular to the X-axis.
      ;;--------------------------------------------------------------
      (make-grid-rec-init set!div-grid-x-sizes)
      )

    (define y-sizes
      ;; This is a record initializer for `DIV-GRID`, it takes a list
      ;; or vector argument, each element containing a number or else
      ;; `'ENCLOSE` or `'EXPAND`. These values indicate the size (or
      ;; how to compute the size) of rows, i.e. divisions
      ;; perpendicular to the Y-axis.
      ;;--------------------------------------------------------------
      (make-grid-rec-init set!div-grid-y-sizes)
      )

    (define (increasing imax)
      ;; This is an argument you can pass to `ROW-MAJOR` or
      ;; `COLUMN-MAJOR`, to indicate that the list elements should be
      ;; filled into the grid in increasing order.
      ;;--------------------------------------------------------------
      (lambda (i) i)
      )

    (define (decreasing imax)
      ;; This is an argument you can pass to `ROW-MAJOR` or
      ;; `COLUMN-MAJOR`, to indicate that the list elements should be
      ;; filled into the grid in decreasing order.
      ;;--------------------------------------------------------------
      (lambda (i) (- imax i 1))
      )

    (define (creasing? f)
      (or (not f) (eq? f increasing) (eq? f decreasing))
      )

    (define (%grid-fill log-output parent ylbl xdir ydir xmax ymax subdiv-vec ranks)
      (let ((adjust-x (xdir xmax))
            (adjust-y (ydir ymax))
            )
        (let y-loop ((y-index 0) (ranks ranks))
          (cond
           ((null? ranks) subdiv-vec)
           (else
            (let*-values
                (((elems) (car ranks))
                 ((y-index elems)
                  (cond
                   ((null? elems) (values 0 elems))
                   (else
                    (let ((elem0 (car elems)))
                      (cond
                       ((integer? elem0) (values elem0 (cdr elems)))
                       (else (values y-index elems))
                       ))))))
              (cond
               ((< y-index ymax)
                (let-values
                    (((len elems)
                      (construct-content-list log-output parent cast-to-div-record elems)
                      ))
                  (let x-loop ((x-index 0) (elems elems))
                    (cond
                     ((null? elems) (values))
                     ((<= len x-index) (values))
                     (else
                      (let*((x (adjust-x x-index))
                            (y (adjust-y y-index))
                            (i (+ x (* xmax y)))
                            (elem (car elems))
                            )
                        (vector-set! subdiv-vec i elem)
                        (x-loop (+ 1 x-index) (cdr elems))
                        )))))
                (y-loop (+ 1 y-index) (cdr ranks))
                )
               (else
                (error "index out of bounds" ylbl y-index '>= ymax)
                ))))))))

    (define (%grid-elems row-major major-dir minor-dir ranks)
      (cond
       ((not (creasing? major-dir))
        (error "first argument must be 'increasing or 'decreasing" major-dir)
        )
       ((not (creasing? minor-dir))
        (error "second argument must be 'increasing or 'decreasing" minor-dir)
        )
       (else
        (record-updater
         div-grid-type?
         (lambda (o)
           (let ((grid (div-content o)))
             (set!div-grid-subdivs
              grid
              (make<div-monad>
               (lambda (log-output parent)
                 (let*((major-dir (or major-dir increasing))
                       (minor-dir (or minor-dir increasing))
                       (xsizes (div-grid-x-sizes grid))
                       (ysizes (div-grid-y-sizes grid))
                       (xmax (if xsizes (vector-length xsizes) #f))
                       (ymax (if ysizes (vector-length ysizes) #f))
                       (undefd "grid size list not specified")
                       (subdiv-vec
                        (cond
                         ((not xsizes) (error undefd 'x-axis))
                         ((not ysizes) (error undefd 'y-axis))
                         (else (make-vector (* xmax ymax) #f))
                         )))
                   (if row-major
                       (%grid-fill
                        log-output parent 'row    major-dir minor-dir
                        xmax ymax subdiv-vec ranks
                        )
                       (%grid-fill
                        log-output parent 'column minor-dir major-dir
                        ymax xmax subdiv-vec ranks
                        ))))))
             o
             ))))))

    (define (%xy-major row-major)
      (lambda ranks
        (let*((first (if (null? ranks) #f (car ranks)))
              (ranks (if first (cdr ranks) ranks))
              (second (if (null? ranks) #f (car ranks)))
              (ranks (if second (cdr ranks) ranks))
              (listish? (lambda (a) (or (pair? a) (null? a))))
              )
          (cond
           ((and (creasing? first) (creasing? second) (pair? ranks))
            (%grid-elems row-major first second ranks)
            )
           ((and (creasing? first) (listish? second))
            (%grid-elems row-major first #f (cons second ranks))
            )
           ((listish? first)
            (%grid-elems row-major #f #f (cons first (cons second ranks)))
            )
           (else
            (error "must provide at least one rank of grid elements")
            )))))

    (define row-major
      ;; This is a record initializer for `DIV-GRID`. This specifies
      ;; the content of the grid. You must pass at least one argument:
      ;; a list of ranks, where each rank is a list of elements (so a
      ;; list of lists). Each rank fills in a row. A rank may begin
      ;; with an integer indicating the row index into which elements
      ;; are filled. Each element must be a `DIV` constructor.
      ;;
      ;; If you wish, you may apply either `DECREASING` or
      ;; `INCREASING` as the first argument, or you may apply them as
      ;; both the first and second arguments. If the first argument is
      ;; `DECREASING`, rows are filled in from the opposite direction
      ;; from how they are specified in the ranks (usually
      ;; right-to-left). If `DECRASING` is also applied as the second
      ;; argument, ranks fill in from bottom to top.
      ;;
      ;; Each element in the ranks must be `#f`, or a `DIV`
      ;; constructor, or a procedure taking a column number and row
      ;; number and returning `#f` or a `DIV` constructor.
      ;;--------------------------------------------------------------
      (%xy-major #t)
      )

    (define column-major
      ;; Like `ROW-MAJOR` except each rank fills in columns of the
      ;; grid, rather than rows. Like with `ROW-MAJOR` you may apply
      ;; either `INCREASING` or `DECREASING` as the first, or as both
      ;; the first and second arguments.
      ;;--------------------------------------------------------------
      (%xy-major #f)
      )

    (define grid-elems
      ;; This is a synonym for `ROW-MAJOR`.
      ;;-----------------------------------
      row-major
      )

    (define div-grid
      (new-constructor
       (lambda (tag) (or (eq? tag div-grid-type?) (eq? tag div-record-type?)))
       (lambda () (%content->div (make<div-grid> #f #f #f)))
       (lambda (o init)
         (or (div-common-constr o init)
             (error "cannot initialize div-grid with value of this type" init)
             ))
       (lambda (o) ;; <- finalize
         (make<div-monad>
          (lambda (log-output parent)
            (let*((grid  (div-content o))
                  (monad (div-grid-subdivs grid))
                  (monad
                   (if (div-monad-type? monad)
                       (%%run-div-monad log-output parent monad)
                       monad
                       ))
                  (vec
                   (if (or (div-monad-type? monad) (use-vars-type? monad))
                       (%run-div-monad log-output parent monad)
                       monad
                       )))
              (set!div-grid-subdivs grid vec) o
              ))))))

    (define (for-each-div-grid proc)
      (lambda (grid)
        (let*((x-sizes (div-grid-x-sizes grid))
              (len-x-sizes (vector-length x-sizes))
              (y-sizes (div-grid-y-sizes grid))
              (elems (div-grid-subdivs grid))
              (len (vector-length elems))
              )
          (cond
           ((vector? elems)
            (let loop ((i 0))
              (cond
               ((< i len)
                (let ((x (remainder i len-x-sizes))
                      (y (quotient  i len-x-sizes))
                      )
                  (proc
                   (vector-ref elems i) x y
                   (vector-ref x-sizes x)
                   (vector-ref y-sizes y)
                   )
                  (loop (+ 1 i))
                  ))
               (else (values))
               )))
           (else (values))
           ))))

    (define (diff-div-grid diff-action old new)
      (diff-div-vector
       diff-action
       (div-grid-subdivs old)
       (div-grid-subdivs new)
       ))

    (define (div-grid-delete o cont)
      ((for-each-div-grid
        (lambda (subdiv i j w h)
          (cond
           ((div-record-type? subdiv) (div-delete subdiv))
           (else
            (let ((port (current-error-port)))
              (display "(schemacs ui) Warning: div-grid contains non-div element " port)
              (write subdiv port)
              (newline port)
              )))))
       cont
       ))

    (define (print-div-grid depth cont)
      (define xs (div-grid-x-sizes cont))
      (define xs-len (vector-length xs))
      (define vec (div-grid-subdivs cont))
      (define len (vector-length vec))
      (define (print-sizes c get)
        (let*((vec (get cont))
              (len (vector-length vec))
              )
          (apply form 1 (print c "-sizes") (vector->list vec))
          ))
      (define (x-loop i x)
        (cond
         ((>= i len) '())
         ((< x xs-len)
          (cons
           (%print-div depth (vector-ref vec i))
           (x-loop (+ 1 i) (+ 1 x))
           ))
         (else '())
         ))
      (define (y-loop i y)
        (cond
         ((< i len)
          (cons
           (form
            1 "list" y (line-break)
            (apply join-lines (x-loop i 0))
            (line-break)
            )
           (y-loop (+ xs-len i) (+ 1 y))
           ))
         (else '())
         ))
      (print
       (line-break)
       (print-sizes #\x div-grid-x-sizes) (line-break)
       (print-sizes #\y div-grid-y-sizes) (line-break)
       (form
        1 "row-major" (line-break)
        (apply join-lines (y-loop 0 0))
        )))

    ;;----------------------------------------------------------------

    (define-record-type <div-pack-type>
      (make<div-pack> oriented from flags sub-sizes subdivs)
      pack-record-type?
      (oriented    div-pack-orientation    set!div-pack-orientation) 
      (from        div-pack-from           set!div-pack-from)
      (flags       div-pack-flags          set!div-pack-flags)
      (sub-sizes   div-pack-subdiv-sizes   set!div-pack-subdiv-sizes)
      (subdivs     div-pack-subdivs        set!div-pack-subdivs)
      )

    (define div-pack-type? (div-type-check pack-record-type?))

    (define (copy-div-pack copy-ref o)
      (make<div-pack>
       (div-pack-orientation o)
       (div-pack-from o)
       (div-pack-flags o)
       (vector-map copy-2D (div-pack-subdiv-sizes o))
       (vector-map (%copy-div copy-ref) (div-pack-subdivs o))
       ))

    (define (cut-orientation? a)
      (or (eq? a cut-horizontal) (eq? a cut-vertical)
          (eq? a align-vertical) (eq? a align-horizontal)
          ))

    (define cut-horizontal
      ;; A keyword that indicates the area should be split by
      ;; horizontal lines.  This is a synonym for `align-vertical`
      ;;--------------------------------------------------------------
      'cut-horizontal
      )

    (define align-vertical
      ;; A keyword that indicates the elements should be stacked or
      ;; aligned vertically. This is a synonym for `cut-horizontal`
      ;;--------------------------------------------------------------
      'align-vertical
      )

    (define cut-vertical
      ;; A keyword that indicates the area should be split by vertical
      ;; lines.
      ;;--------------------------------------------------------------
      'cut-vertical
      )

    (define align-horizontal
      ;; A keyword that indicates the elements should be stacked or
      ;; aligned horizontally. This is a synonym for `cut-vertical`
      ;;--------------------------------------------------------------
      'align-horizontal
      )

    (define (pack-direction? a)
      (or (eq? a from-start) (eq? a from-end))
      )

    (define from-start
      ;; A keyword that indicates the splitting should start from the
      ;; left if the cut orientation is `'cut-vertical`, or from the
      ;; top if the cut orientation is `'cut-horizontal'.
      ;;--------------------------------------------------------------
      'from-start
      )

    (define from-end
      ;; A keyword that indicates the splitting should start from the
      ;; right if the cut orientation is `'cut-vertical`, or from the
      ;; bottom if the cut orientation is `'cut-horizontal'.
      ;;--------------------------------------------------------------
      'from-end
      )

    (define wrapping
      ;; This keyword indicates that the packed `DIV`s should be split
      ;; into rows or columns (whichever direction is orthogonal to
      ;; the direction of alignment) and elements that don't fit in
      ;; the constrained width of the `DIV` should be moved into
      ;; successive rows/columns. An example of this is how words in a
      ;; paragraph are wrapped so that the width of the paragraph
      ;; never exceeds a certain limit.
      ;;
      ;; This flag is mutually exclusive with `adjustable`, you cannot
      ;; specify both.
      ;;--------------------------------------------------------------
      'wrapping
      )

    (define (div-pack-flag? val)
      ;; Flags for the `DIV-PACK` constructor include `WRAPPING` and
      ;; `DIV-PACK`. These flags are mutually exclusive, only one or
      ;; the other must be specified.
      ;;--------------------------------------------------------------
      (eq? wrapping val)
      )

    (define (%pack-init size elem)
      (record-updater
       div-pack-type?
       (lambda (o)
         (let*((pack (div-content o)))
           (set!div-pack-subdiv-sizes
            pack (cons size (div-pack-subdiv-sizes pack))
            )
           (set!div-pack-subdivs
            pack (cons elem (div-pack-subdivs pack))
            )
           o
           ))))

    (define (%pack o size elem)
      ((run-record-updater (%pack-init size elem)) o)
      )

    (define pack-elem
      ;; Place an element into the `DIV-PACK`. Apply a size as the
      ;; first argument, the size may also be `'expand` or `'enclose`.
      ;; If no size is given, the size defaults to `'expand'.
      ;;--------------------------------------------------------------
      (case-lambda
        ((elem) (%pack-init (size2D enclose) elem))
        ((size elem)
         (%pack-init
          (cond
           ((size2D-type? size) size)
           ((variable-size? size) (size2D size))
           (else size)
           )
          elem
          ))))

    (define div-pack
      ;; Constructs a `DIV` in which elements are packed next to each
      ;; other, each given an amount of space which you specify when
      ;; packing. Packing elements creates "cuts" in the parent `DIV`,
      ;; these cuts can be oriented horizontally or vertically. One of
      ;; the arguemnts to this constructor must be exactly one of the
      ;; keyword `'cut-horizontal` or `'cut-vertical`. Another one of
      ;; the keywords decides the direction from which packing starts,
      ;; this is `'from-start` or `'from-end`, the default is
      ;; `'from-start`
      ;;
      ;; This constructor can then take an arbitrary number of
      ;; arguments, they will be packed accordingly. Use `pack-elem`
      ;; to specify a size for an element, an element with no size
      ;; defaults to `'expand`. Elements must be `DIV` constructors,
      ;; or values that can be converted to a `DIV` constructor using
      ;; the procedure parameterizing the `*div-content-converter*`
      ;; parameter.
      ;;--------------------------------------------------------------
      (new-constructor
       (lambda (tag) (or (eq? tag div-pack-type?) (eq? tag div-record-type?)))
       (lambda () (%content->div (make<div-pack> #f #f #f '() '())))
       (lambda (o val)
         (or (div-common-constr o val)
             (let ((pack (div-content o)))
               (cond
                ((or (div-monad-type? val) (use-vars-type? val))
                 (%pack o enclose val)
                 o)
                ((cut-orientation? val)
                 (cond
                  ((not (div-pack-orientation pack))
                   (set!div-pack-orientation pack val)
                   o)
                  (else
                   (error "orientation flag specified more than once" val)
                   )))
                ((pack-direction? val)
                 (cond
                  ((not (div-pack-from pack))
                   (set!div-pack-from pack val)
                   o)
                  (else
                   (error "pack-from flag specified more than once" val)
                   )))
                ((div-pack-flag? val)
                 (set!div-pack-flags pack val)
                 o)
                ((size2D-type? val) (%pack o val #f) o)
                ((rect2D-type? val) (%pack o (rect2D-size val) #f) o)
                (else (%pack o enclose val) o)
                ))))
       (lambda (o) ;; <- finalize
         (let ((pack (div-content o)))
           (unless (div-pack-orientation pack)
             (set!div-pack-orientation pack 'cut-vertical)
             )
           (when (not (div-pack-from pack))
             (set!div-pack-from pack 'from-start)
             )
           (make<div-monad>
            (lambda (log-output parent)
              (let*((subdivs-vec
                     (construct-content
                      log-output parent cast-to-div-record
                      (reverse (div-pack-subdivs pack))
                      ))
                    (sizes-vec
                     (list->vector
                      (reverse (div-pack-subdiv-sizes pack))
                      )))
                (set!div-pack-subdivs pack subdivs-vec)
                (set!div-pack-subdiv-sizes pack sizes-vec)
                o)))))))

    (define (for-each-div-pack proc)
      (lambda (pack)
        (let ((elems (div-pack-subdivs pack))
              (sizes (div-pack-subdiv-sizes pack))
              )
          (cond
           ((vector? elems)
            (let loop ((i 0))
              (proc (vector-ref elems i) i (vector-ref sizes i))
              (loop (+ 1 i))
              ))
           (else (values))
           ))))

    (define (diff-div-pack diff-action old new)
      (diff-div-vector
       diff-action
       (div-pack-subdivs old)
       (div-pack-subdivs new)
       ))

    (define (div-pack-delete o cont)
      ((for-each-div-pack
        (lambda (subdiv i size)
          (cond
           ((div-record-type? subdiv) (div-delete subdiv))
           (else
            (let ((port (current-error-port)))
              (display "(schemacs ui) Warning: div-pack contains non-div element " port)
              (write subdiv port)
              (newline port)
              )))))
       cont
       ))

    (define (print-div-pack depth cont)
      ;; NOTE: flags and orientation are written by a special rule in
      ;; the `%write-div` procedure.
      (let*((sizes (div-pack-subdiv-sizes cont))
            (subdivs (div-pack-subdivs cont))
            (len (min (vector-length sizes) (vector-length subdivs)))
            )
        (apply
         join-lines
         (let loop ((i 0))
           (cond
            ((< i len)
             (cons
              (form
               1 "pack-elem" (vector-ref sizes i) (line-break)
               (%print-div depth (vector-ref subdivs i))
               )
              (loop (+ 1 i))
              ))
            (else '())
            )))))

    ;;----------------------------------------------------------------

    (define-record-type <floater-type>
      (make<floater> rect z-index from-var div)
      floater-type?
      (rect        floater-rect        set!floater-rect)
      (z-index     floater-z-index     set!floater-z-index)
      (from-var    floater-from-var?   set!floater-from-var)
      (div         floater-div         set!floater-div)
      )

    (define (copy-floater copy-ref)
      (lambda (o)
        (make<floater>
         (copy-rect2D (floater-rect o))
         (floater-z-index o)
         (floater-from-var? o)
         ((%copy-div copy-ref) (floater-div o))
         )))

    (define-record-type <div-space-type>
      (make<div-space> outer-align inner-align elements)
      space-record-type?
      (outer-align  div-space-outer-align  set!div-space-outer-align)
      (inner-align  div-space-inner-align  set!div-space-inner-align)
      (elements     div-space-elements     set!div-space-elements)
      )

    (define div-space-type? (div-type-check space-record-type?))

    (define (cast-to-div-record o)
      (cond
       ((not o) #f)
       ((div-record-type? o) o)
       ((floater-type? o) (floater-div o))
       (else (error "cannot cast to `DIV` node type" o))
       ))

    (define (cast-to-floater o)
      (cond
       ((not o) #f)
       ((div-record-type? o)
        (make<floater> (rect2D (point2D 0 0) (size2D enclose)) 0 #f o)
        )
       ((floater-type? o) o)
       (else (error "cannot cast to floater node type" o))
       ))

    (define (copy-div-space copy-ref o)
      (make<div-space>
       (div-space-outer-align o)
       (div-space-inner-align o)
       (vector-map (copy-floater copy-ref) (div-space-elements o))
       ))

    (define =>div-space-elements*! ;; not for export
      (record-unit-lens
       div-space-elements  set!div-space-elements
       '=>div-space-elements*!
       ))

    (define (%align lbl setter)
      (lambda (point)
        (record-updater
         div-record-type?
         (lambda (o)
           (let ((space (div-content o)))
             (cond
              ((point2D-type? point) (setter space point) o)
              (else (error "point2D type value required to initialize" lbl point))
              ))))))

    (define inner-align
      ;; A record constructor for `DIV-SPACE` which must take a
      ;; `<POINT2D-TYPE>`. This value describes a point inside of the
      ;; space that is to be aligned with the `OUTER-ALIGN` point. If
      ;; this argument is not specified it defaults to `(point2D 0 0)`
      ;; which is usually the upper-left corner of the space. To align
      ;; to the middle of the space pass a value of:
      ;;     `(point2D from-middle from-middle)`
      ;;--------------------------------------------------------------
      (%align 'inner-align set!div-space-inner-align)
      )

    (define outer-align
      ;; A record constructor for `DIV-SPACE` which must take a
      ;; `<POINT2D-TYPE>`. This value describes a point in the frame
      ;; of the containing `DIV-SPACE` widget that is to be aligned
      ;; with the `INNER-ALIGN` point. If this argument is not
      ;; specified it defaults to `(point2D 0 0)` which is usually the
      ;; upper-left corner of the frame. To align to the middle of the
      ;; space pass a value of:
      ;;     `(point2D from-middle from-middle)`
      ;;--------------------------------------------------------------
      (%align 'outer-align set!div-space-outer-align)
      )

    (define from-middle
      ;; A keyword that can be used to construct a `point2D-type` that
      ;; is computed auomaticaly. This `'from-middle' value indicates
      ;; point alignment should be at the midpoint of the area along
      ;; the X or Y axis, whichever field of the point2D it specifies.
      ;;--------------------------------------------------------------
      'from-middle
      )

    (define div-space
      ;; Construct a space with many `FLOATER` elements. All arguments
      ;; to this constructor must be `FLOATER`s.
      ;;--------------------------------------------------------------
      (new-constructor
       (lambda (tag) (or (eq? tag div-space-type?) (eq? tag div-record-type?)))
       (lambda () (%content->div (make<div-space> #f #f '())))
       (lambda (o val)
         (cond
          ((not val) o)
          ((div-common-constr o val) o)
          ((or (div-monad-type? val)
               (use-vars-type? val)
               (floater-type? val)
               )
           (let ((space (div-content o)))
             (update
              (lambda (elems) (cons val elems))
              space =>div-space-elements*!
              )
             o
             ))
          (else (error "cannot construct floater from type" val))
          ))
       (lambda (o)
         (make<div-monad>
          (lambda (log-output parent)
            (let*((space (div-content o))
                  (elems
                   (construct-content
                    log-output parent cast-to-floater
                    (reverse (div-space-elements space))
                    )))
              ;; <TODO> sort the elements by their z-index.
              (set!div-space-elements space elems) o
              ))))))

    (define floater
      ;; Construct an element in a `div-space` which can be placed at
      ;; any arbitrarily point in the space. Takes 3 arguments:
      ;;
      ;;  1. a `div` element
      ;;
      ;;  2. an optional `point2D` indicating where the div should be
      ;;     placed, defaulting to (point2D 0 0), the size of the
      ;;     `div` element will be `(size2D enclose)` which will
      ;;     contain the `div` in the smallest possible rectangle that
      ;;     encloses all elements of the `div`. Instead of specifying
      ;;     a `point2D` you may also may also specify a `rect2D`
      ;;     which specifies a fixed size rather than `(size2D enclose)`.
      ;;
      ;;  3. an integer Z-index (could also be called a layer index),
      ;;     indicating the drawing order in which this element is
      ;;     drawn into the `div-space`. If not specified, it is
      ;;     automatically assigned an integer value (starting from 0)
      ;;     one more than the `floater` element that comes before it.
      ;;--------------------------------------------------------------
      (new-constructor
       (lambda (tag) (eq? tag floater-type?))
       (lambda () (make<floater> #f #f #f '()))
       (lambda (flo val)
         (cond
          ((point2D-type? val)
           (set!floater-rect flo (rect2D val (size2D enclose))) flo
           )
          ((rect2D-type? val)  (set!floater-rect    flo val) flo)
          ((integer? val)      (set!floater-z-index flo val) flo)
          ((or (div-record-type? val)
               (div-monad-type? val)
               (use-vars-type? val)
               )
           (set!floater-div flo val) flo
           )
          (else (error "value cannot be used to construct floating div" val))
          ))
       (lambda (flo)
         (make<div-monad>
          (lambda (log-output parent)
            (let ((div (%run-div-monad log-output parent (floater-div flo))))
              (cond
               ((or (not div) (div-record-type? div))
                (set!floater-div flo div)
                flo
                )
               (else (error "content of floater is not a `DIV`" div flo))
               )))))))

    (define (for-each-div-space proc)
      ;; A curried procedure which takes a `PROC` and a
      ;; `<DIV-SPACE-TYPE>`, applies `PROC` to every `<DIV-TYPE>`
      ;; within a `<DIV-SPACE-TYPE>`.
      ;;--------------------------------------------------------------
      (lambda (space)
        ((for-each-floater
          (lambda (flo)
            ((for-each-div proc) (floater-div flo))
            ))
         space
         )))

    (define (for-each-floater proc)
      ;; A curried procedure which takes a `PROC` and a
      ;; `<DIV-SPACE-TYPE>`. Applies every `<FLOATER-TYPE>` in a
      ;; `<DIV-SPACE-TYPE>` to the given `PROC`.
      ;;--------------------------------------------------------------
      (lambda (space)
        (let*((space
               (cond
                ((div-type? space) (div-content space))
                ((div-space-type? space) space)
                (else (error "not a div-space container" space))
                ))
              (elems (div-space-elements space))
              )
          (cond
           ((vector? elems) (vector-for-each proc elems))
           (else (values))
           ))))

    (define (diff-div-space diff-action old new)
      (diff-div-vector
       diff-action floater-div
       (div-space-elements old)
       (div-space-elements new)
       ))

    (define (div-space-delete o cont)
      ((for-each-floater
        (lambda (flo)
          (let ((subdiv (floater-div flo)))
            (cond
             ((div-record-type? subdiv) (div-delete subdiv))
             (else
              (let ((port (current-error-port)))
                (display "(schemacs ui) Warning: floater contains non-div element " port)
                (write subdiv port)
                (newline port)
                ))))))
       cont
       ))

    (define (print-div-space depth cont)
      (let*((outer (div-space-outer-align cont))
            (inner (div-space-inner-align cont))
            )
        (apply
         join-by (line-break)
         (and outer
              (form 1 "outer-align" outer (line-break))
              )
         (and inner
              (form 1 "inner-align" inner (line-break))
              )
         (map
          (print-floater depth)
          (vector->list (div-space-elements cont))
          ))))

    (define (print-floater depth)
      (lambda (flo)
        (let ((z (floater-z-index flo))
              (r (floater-rect flo))
              (cont (floater-div flo))
              )
          (form
           1 "floater" z (line-break)
           (print-rect2D r) (line-break)
           (%print-div depth (floater-div flo))
           ))))

    ;;----------------------------------------------------------------
    ;; Pre-defined view types
    ;;
    ;; The following functions have two purposes. The first is that
    ;; each function is a shorthand for `div`, `div-pack`, `div-grid`,
    ;; or `div-space` with `view-type` applied automatically, this way
    ;; end users can simply write `(push-button ...)` rather than
    ;; `(div (view-type push-button) ...)`. Each of these functions
    ;; simply apply it's arguments to a setting the `view-type` to the
    ;; function itself. The second purpose is that each of these
    ;; functions serve as a unique symbol to denote view type of the
    ;; `div` node being constructed. A back-end implementation can
    ;; check the whether type of a `div` node is a `push-button` like
    ;; so: `(eq? push-button (div-view-type o))`.
    ;;----------------------------------------------------------------

    (define (push-button . args)
      ;; Creates a push button with a string as the label.
      ;;
      ;; Properties:
      ;;
      ;;  - `'ON-BUTTON-PUSH:` :: a procedure that takes zero
      ;;  arguments which updates some state variable when the end
      ;;  user pushes the button.
      ;;--------------------------------------------------------------
      (apply div (view-type push-button) args)
      )

    (define (check-box . args)
      ;; Create a check box with a string as the label.
      ;;
      ;; Properties:
      ;;
      ;;  - `'ON-BUTTON-PUSH:` :: a procedure that can takes a boolean
      ;;    argument which updates some state when the
      ;;--------------------------------------------------------------
      (apply div (view-type check-box) args)
      )

    (define (text-input . args)
      ;; Create a text input field with a string as the initial input.
      ;;
      ;; Properties:
      ;;
      ;;  - `'ON-INPUT:` :: called with a character whenever the state
      ;;    of the text field changes due to some key press, including
      ;;    arrow keys.
      ;;
      ;;  - `'ON-ENTER:` :: called only when the enter key is pressed
      ;;    while this div element is highlighted.
      ;;
      ;;  - `'COMPLETION:` :: called whenever text is added to the
      ;;    input text field, should return a list or vector of string
      ;;    which can suggest to the user what to type.
      ;;--------------------------------------------------------------
      (apply div (view-type text-input) args)
      )

    (define (radio-group . args)
      ;; Create `div-pack` node intended to hold a group of radio
      ;; buttons. Each radio button must be a `check-box`, but is
      ;; rendered with a visual indicator that each check box is a
      ;; mutually-exclusive choice.
      ;;
      ;;  - `'ON-INPUT:` :: called when any one of the buttons within
      ;;    the group is pressed.
      ;;--------------------------------------------------------------
      (apply div-pack (view-type radio-group) args)
      )

    (define (labeled-group . args)
      ;; Create a `div-pack` node with a rectangle drawn around the
      ;; elements within it, and a label along the top of the
      ;; rectangle. This is used to indicate to end users a group of
      ;; related input controls.
      ;;
      ;; Properties:
      ;;
      ;;  - `'label:` :: a string label for this group.
      ;;--------------------------------------------------------------
      (apply div-pack (view-type labeled-group) args)
      )

    (define (text-editor . args)
      ;; Create a `div` node which can display a text buffer.
      ;;
      ;; Properties:
      ;;
      ;;  - `'buffer:` :: a platform-specific text buffer object or
      ;;    some kind of human-readable handle for the text buffer.
      ;;
      ;;  - `'url:` or `'path:` :: are synonymous and can be used
      ;;    interchangably. If no URL schema is given a local file
      ;;    path is assumed. This property indicate where the source
      ;;    of the text is from, and where it should be saved if a
      ;;    save event occurs.
      ;;
      ;;  - `'cache:` :: a filesystem path that saves the most recent
      ;;    local changes to the buffer without updating the resource
      ;;    at the URL. NOTE: the auto-save time interval should be
      ;;    specified in a global configuration variable, not on a
      ;;    per-widget basis.
      ;;
      ;;  - `'read-only:` :: disables editing commands. Only search
      ;;    and selection is possible.
      ;;
      ;;  - `'scrolled:` :: defaults to `#t`, but if set to `#f`
      ;;    indicates that the scroll bars should never be
      ;;    visible. This is useful when creating "notebook" like user
      ;;    interfaces where you want many text editor nodes
      ;;    interspersed among many other elements all stored in a
      ;;    `div-pack` node, and you want the `div-pack` node to
      ;;    handle the scrolling. Setting this property to `#f`
      ;;    prevents scroll bars from being nested within other scroll
      ;;    bars.
      ;;
      ;;  - `'markup:` :: set to `#t` or the symbol `'markdown`
      ;;    indicates markdown rendering (where available). Set to the
      ;;    symbol `'html' indicates HTML rendering (where ;;
      ;;    available).  Other options could be `'org`, `'rst`,
      ;;    `'roff`, `'info`. Set to `#f` indicates UTF-only.
      ;;--------------------------------------------------------------
      (apply div (view-type text-editor) args)
      )

    (define (canvas . args)
      ;; Create a `div` node which can display arbitrary 2D garphics.
      ;;
      ;; Properties:
      ;;
      ;;  - `'buffer:` :: a platform-specific pixel buffer object, or
      ;;    some kind of human-readable handle for the pixel buffer.
      ;;
      ;;  - `'url:` or `'path:` :: are synonymous and can be used
      ;;    interchangably. If no URL schema is given a local file
      ;;    path is assumed. This property indicate where the source
      ;;    of the text is from, and where it should be saved if a
      ;;    save event occurs.
      ;;
      ;;  - `'cache:` :: a filesystem path that saves the most recent
      ;;    local changes to the buffer without updating the resource
      ;;    at the URL. NOTE: the auto-save time interval should be
      ;;    specified in a global configuration variable, not on a
      ;;    per-widget basis.
      ;;
      ;;  - `'scrolled:` :: defaults to `#t`, but if set to `#f`
      ;;    indicates that the scroll bars should never be
      ;;    visible. This is useful when creating "notebook" like user
      ;;    interfaces where you want many text editor nodes
      ;;    interspersed among many other elements all stored in a
      ;;    `div-pack` node, and you want the `div-pack` node to
      ;;    handle the scrolling. Setting this property to `#f`
      ;;    prevents scroll bars from being nested within other scroll
      ;;    bars.
      ;;
      ;;  - `'on-move-pointer:` :: a procedure to be called when a
      ;;    pointer device (e.g. a mouse) cursor is moved while the
      ;;    pointer is within the bounds of the canvas. This callback
      ;;    should receive an `point-2D` value.
      ;;
      ;;  - `'on-drag-pointer:` :: a procedure to be called when a
      ;;    pointer device (e.g. a mouse) button is pressed while the
      ;;    mouse cursor is within the bounds of the canvas, and the
      ;;    device is moved while the button is pressed. This callback
      ;;    should receive a `point-2D` value and a list of symbols
      ;;    indicating which device buttons are pressed and held.
      ;;
      ;;  - `'on-click:` :: a procedure to be called when a pointer
      ;;    device (e.g. a mouse) button is pressed and releasd
      ;;    quickly. What precise action constitutes a "click"
      ;;    (e.g. the amount of time between button press and release)
      ;;    should be determined by a global configuration
      ;;    variable. This callback should receive a `point-2D`
      ;;    indicating the position within the canvas boundaries the
      ;;    event occurred, and a list of symbols indicating which
      ;;    device buttons have been pressed.
      ;;
      ;;  - `'on-2click:` :: like the `'on-click` callback but for a
      ;;    "double click" event, the precise action of which is also
      ;;    determined by a global configuration variable.
      ;;--------------------------------------------------------------
      (apply div (view-type canvas) args)
      )

    (define (composite . args)
      ;; Create a `div-space` node which can compose arbitrary
      ;; `canvas` objects into a composite raster image.
      ;;
      ;; Properties:
      ;;
      ;;  - `'on-move-pointer:` :: a procedure to be called when a
      ;;    pointer device (e.g. a mouse) cursor is moved while the
      ;;    pointer is within the bounds of the canvas. This callback
      ;;    should receive an `point-2D` value.
      ;;
      ;;  - `'on-drag-pointer:` :: a procedure to be called when a
      ;;    pointer device (e.g. a mouse) button is pressed while the
      ;;    mouse cursor is within the bounds of the canvas, and the
      ;;    device is moved while the button is pressed. This callback
      ;;    should receive a `point-2D` value and a list of symbols
      ;;    indicating which device buttons are pressed and held.
      ;;
      ;;  - `'on-click:` :: a procedure to be called when a pointer
      ;;    device (e.g. a mouse) button is pressed and releasd
      ;;    quickly. What precise action constitutes a "click"
      ;;    (e.g. the amount of time between button press and release)
      ;;    should be determined by a global configuration
      ;;    variable. This callback should receive a `point-2D`
      ;;    indicating the position within the canvas boundaries the
      ;;    event occurred, and a list of symbols indicating which
      ;;    device buttons have been pressed.
      ;;
      ;;  - `'on-2click:` :: like the `'on-click` callback but for a
      ;;    "double click" event, the precise action of which is also
      ;;    determined by a global configuration variable.
      ;;--------------------------------------------------------------
      (apply div-space (view-type composite) args)
      )

    (define (tiled-windows . args)
      ;; Create a `div-pack` where each element is in a tiled windows
      ;; with an adjustment widget in between tiles that lets end
      ;; users resize the tiles.
      ;;
      ;; Properties: (none)
      ;;--------------------------------------------------------------
      (apply div-pack (view-type tiled-windows) args)
      )

    (define (print-div-view-type vtype)
      (and vtype
           (let ((sym
                  (cond
                   ((eq? vtype push-button) "push-button")
                   ((eq? vtype check-box) "check-box")
                   ((eq? vtype text-input) "text-input")
                   ((eq? vtype radio-group) "radio-group")
                   ((eq? vtype labeled-group) "labeled-group")
                   ((eq? vtype text-editor) "text-editor")
                   ((eq? vtype canvas) "canvas")
                   ((eq? vtype composite) "composite")
                   ((eq? vtype tiled-windows) "tilded-windows")
                   (else vtype)
                   )))
             (form 1 "view-type" sym)
             )))

    ;;----------------------------------------------------------------
    ))

;; ** What the `DIV` procedure does
;; The `DIV` procedure (along with `DIV-PACK`, `DIV-SPACE`, and
;; `FLOATER`) all constructors which construct a `div-monad-type?`
;; closure. The closure contains quite a lot of information, including
;; arguments were passed to the constructor, state variables, event
;; handlers, and other `div-monad-type?` monads. The event handlers
;; themselves are closures which also contain references to all of
;; this information. When a monad that was constructed by a `DIV`
;; constructor is evaluated, all of that information is used to
;; construct a `DIV` node tree. Event handlers will be frozen into the
;; tree but can perform updates on state variables in the closure,
;; which in turn trigger the construction of new `DIV` node trees that
;; can be merged into a `DIV` node tree that was constructed and
;; stored into the closure previously.
;;
;; So a time step in a reactive program performs the following steps:
;;
;;  1. construct a monadic closure, referred to simply as a "monad",
;;
;;  2. evaluate the monad to construct a `DIV` node tree, embedding
;;     event handler closures into the tree.
;;
;;  3. Render the tree in the back end.
;;
;; Then, when an event occurs:
;;
;;  1. State variables are updated,
;;
;;  2. state updates trigger evaluation of monads in the closure,
;;
;;  3. the monad evaluation constructs a new `DIV` node tree,
;;
;;  4. the new `DIV` node tree is compared to the old `DIV` node tree
;;     stored in the closure of the event handler,
;;
;;  5. the changed portions of the `DIV` node tree trigger a new
;;     rendering call in the back-end where the old `DIV` node tree is
;;     deleted, and the new `DIV` node tree is made visible on screen.
;;     The new `DIV` node tree also contains new event handler
;;     closures.
;;
;; ** Should `USE-VARS` nodes exist in the `DIV` tree?
;; Though `use-vars` nodes are used in the construction of a the `DIV`
;; tree, should they always remain there even after the `DIV` tree has
;; been constructed? The answer is "no".  The `DIV` nodes contain a
;; boolean tag indicating whether it was constructed from a
;; variable. If that node is tagged as variable, the rendering
;; back-end can do the accounting necessary to make that part of the
;; rendered tree more easily changed. But when a change occurs, and
;; `run-div-monad` is applied to the monadic procedures that generate
;; new `DIV` trees, the variables used during construction need to be
;; resolved immediately.
;;
;; The monads that are applied when an event occurs will contain
;; references to the `USE-VARS` variables within the closure itself,
;; and these closures are themselves contained in the event handler
;; code that is stored in the back-end. So it is not necessary to keep
;; these `USE-VARS` nodes in the final tree computed by
;; `run-div-monad`. Even if the old `USE-VARS` node is kept, the old
;; `USE-VARS` node is replaced by a new one just like it as soon as
;; `run-div-monad` is evaluated. So it is better to remove the
;; `USE-VARS` nodes returned by `run-div-monad` so as not to
;; complicate the rendering process with constant checks of the type
;; of node.

(define-library (schemacs ui platform guile-gi-gtk3)
  (import
    (scheme base)
    (scheme write)
    (scheme file)
    (only (scheme process-context) command-line)
    (only (schemacs lens) lens  view  lens-set  update)
    (only (schemacs vbal)
          vbal-assq  vbal-for-each
          )
    (only (schemacs ui rectangle)
          point2D  point2D-x  point2D-y
          rect2D-type?  rect2D  rect2D-point  rect2D-size
          size2D-type?  size2D  size2D-width  size2D-height
          )
    (only (schemacs ui)
          div-monad-type?  run-div-monad  div
          div-type?  div-record-type?  div-from-var?
          div-view-type  div-parent  div-content  div-on-update
          =>div-rect*!  =>div-widget*!  =>div-properties*!
          =>div-on-delete*!  =>div-on-update*!
          div-prop-lookup  prop-lookup
          div-grid-type?  for-each-div-grid
          div-pack-type?  for-each-div-pack
          div-space-type?  div-space  div-space-elements
          div-space-inner-align  div-space-outer-align
          for-each-floater  floater floater-div  floater-rect
          div-pack-subdivs  div-pack-subdiv-sizes  div-pack-from
          div-pack-orientation  div-pack-flags  wrapping
          cut-vertical  cut-horizontal
          from-start  from-end
          use-vars-type?   use-vars-value
          div-event-handler  div-run-update
          div-delete  =>div-on-delete*!
          push-button  check-box  text-input  radio-group
          labeled-group  text-editor  canvas  composite
          tiled-windows
          )
    (prefix (schemacs ui text-buffer-impl) *impl/)
    (only (schemacs ui text-buffer) *text-load-buffer-size*)
    (only (scheme case-lambda) case-lambda)
    (only (scheme char) char-upcase)
    (prefix (gi) gi:)
    (only (gi util) push-duplicate-handler!)
    (only (gi) <signal>)
    (prefix (gi repository) gi-repo:)
    (only (gi types) flags->list)
    (prefix (ice-9 eval-string) guile:)
    )
  (export
   gtk-draw-div
   *gtk-application-object*
   *main-app-window*
   )

  (begin
    ;;----------------------------------------------------------------

    (define *main-app-window*
      (let ((win #f))
        (case-lambda
          (() win)
          ((val) (set! win val) val)
          )))

    (define (on-main-loop-start window-name window-size top)
      ;; Construct an event handler that is handles the application
      ;; startup event. This procedure returns an event handler
      ;; procedure that renders the content of a `div` constructed by
      ;; the `TOP` monad, and also sets the application window's title
      ;; to `WINDOW-NAME` and initializes it's size to `WINDOW-SIZE`.
      (lambda (app)
        (let*((top (gtk-draw-content top #f))
              (app-win
               (gi:make
                <GtkApplicationWindow>
                #:application *gtk-application-object*
                #:default-width  (size2D-width window-size)
                #:default-height (size2D-height window-size)
                #:hexpand #f
                #:vexpand #f
                #:title window-name
                #:child (gtk-get-outer-widget (view top =>div-widget*!))
                )))
          (gobject-ref app-win)
          (*main-app-window* app-win)
          (widget:show-all app-win)
          )))

    (define (gtk-draw-div top)
      ;; This is the entrypoint into the program. Apply a `div`
      ;; constructing monad as the `TOP` argument to this
      ;; procedure. When you do the Gtk event loop begins and renders
      ;; the result of the `div`. This procedure will not return until
      ;; the Application window is closed and the Gtk event loop halts.
      (cond
       ((application:register? *gtk-application-object* #f)
        (let*((top (if (div-monad-type? top) (run-div-monad #f top) top))
              (top (if (div-type? top) top
                       (error "not a graphical `DIV` element" top)
                       ))
              (win-name (div-prop-lookup 'window-title: top))
              (win-name
               (cond
                ((symbol? win-name) (string->symbol win-name))
                ((string? win-name) win-name)
                ((not win-name) "Schemacs Gtk Window")
                (else
                 (error
                  "window-title property must be a string or symbol"
                  top
                  ))))
              (win-size (div-prop-lookup 'window-size: top))
              (win-size
               (cond
                ((not win-size) (size2D 640 360))
                ((rect2D-type? win-size) (rect2D-size win-size))
                ((size2D-type? win-size) win-size)
                ((number? win-size) (let ((s (round win-size))) (size2D s s)))
                (else
                 (error
                  "window size property must be a rect2D or size2D value"
                  win-size
                  )))))
          (gi:connect
           *gtk-application-object* gio:activate
           (on-main-loop-start win-name win-size top)
           )
          (application:run *gtk-application-object* (command-line))
          ))
       (else (error "failed to register Gtk application" *gtk-application-object*))
       ))

    ;;================================================================

    (define-record-type <gtk-div-boxed-type>
      ;; Most `div`s are drawing using Gtk widgets which are
      ;; structured as an outer `GtkBox` with other Gtk widgets the
      ;; visible content within the `GtkBox`. This record stucture
      ;; pairs the content with it's containing box.
      (make<gtk-div-boxed> bin wref)
      gtk-div-boxed-type?
      (bin  gtk-div-boxed-container)
      (wref gtk-div-boxed-content)
      )

    (define (gtk-delete-boxed-content o)
      (cond
       ((gtk-div-boxed-type? o)
        (let ((wref (gtk-div-boxed-content o)))
          (when wref (gtk-unref-destroy wref))
          ))
       (else (gtk-unref-destroy o))
       ))

    (define (gtk-delete-container-content o)
      ;; When a state update occurs, sometimes only the content of a
      ;; Gtk widget needs to be deleted and not it's containing GtkBox
      ;; or GtkLayout. This procedure deletes only the content
      ;; widgets, leaving the outer widgets alone.
      (cond
       ((gtk-div-boxed-type? o) (gtk-delete-boxed-content o))
       ((gtk-div-contain-type? o) (gtk-div-contain-delete-content o))
       ((div-record-type? o) (gtk-delete-container-content (view o =>div-widget*!)))
       (else (gtk-unref-destroy o))
       ))

    (define (gtk-div-update-string old new)
      ;; If the `div` being updated is a label, only change the text
      ;; in the label, not the label widget itself.
      (let ((old-cont (div-content old))
            (new-cont (div-content new))
            (wref (view old =>div-widget*!))
            )
        (lens-set wref new =>div-widget*!)
        (lens-set #f old =>div-widget*!)
        (lens-set gtk-div-update-string new =>div-on-update*!)
        (lens-set gtk-div-delete new =>div-on-delete*!)
        (label:set-text wref (div-content new))
        new
        ))

    (define (gtk-unref-destroy wref)
      ;; Marks the widget for deletion. GObject unref is necessary
      ;; because an additional reference count must be maintained
      ;; while a widget reference exists in the `div` tree.
      ;;
      ;; NOTE: that if a `div` tree is deleted and a reference to the
      ;; tree exists in a continuation created by `call/cc`, there is
      ;; a very good chance that resuming the continuation will result
      ;; in a crash. I am not sure how `guile-gi` tracks C references,
      ;; but it does not seem to tie reference counting into the Guile
      ;; memory manager correctly.
      (gobject-unref wref)
      (widget:destroy wref)
      )

    (define (gtk-div-delete o)
      ;; Delete the entire widget.
      (let ((wref
             (cond
              ((div-record-type? o)
               (let ((wref (view o =>div-widget*!)))
                 (lens-set #f o =>div-widget*!)
                 wref
                 ))
              (else o)
              )))
        (cond
         ((not wref) (values))
         ((vector? wref)
          (vector-for-each (lambda (wref) (gtk-unref-destroy wref)) wref)
          )
         ((gtk-div-boxed-type? wref)
          (gtk-unref-destroy (gtk-div-boxed-container wref))
          (gtk-unref-destroy (gtk-div-boxed-content wref))
          )
         (else (gtk-unref-destroy wref))
         )))

    ;;----------------------------------------------------------------

    (define-record-type <gtk-div-contain-type>
      ;; Similar to `<gtk-div-boxed-type>`, this record type is for
      ;; Gtk container widgets and their contents, containers such as
      ;; `GtkBox`, `GtkGrid`, and `GtkLayout`. It has additional
      ;; fields for scroller widgets and a `viewport`, which the
      ;; scroller widget requires for `GtkBox` and `GtkGrid`
      ;; content. `GtkLayout` already instantiates the GtkScrollable
      ;; interface so `div-space` nodes do not fill the `viewport` or
      ;; `scroller` fields.
      (make<gtk-div-contain> outer scroll viewport inner)
      gtk-div-contain-type?
      (outer    gtk-div-contain-outer    set!gtk-div-contain-outer)
      (scroll   gtk-div-contain-scroll   set!gtk-div-contain-scroll)
      (viewport gtk-div-contain-viewport set!gtk-div-contain-viewpoet)
      (inner    gtk-div-contain-inner    set!gtk-div-contain-inner)
      )

    (define (gtk-div-contain-delete-content wref)
      (let*((scroll   (gtk-div-contain-scroll wref))
            (viewport (gtk-div-contain-viewport wref))
            (inner    (gtk-div-contain-inner wref))
            )
        (cond
         ((not inner) (values))
         ((vector? inner)
          (vector-for-each
           (lambda (wref) (gtk-unref-destroy wref))
           inner
           ))
         (else (gtk-unref-destroy inner))
         )
        (when scroll   (gtk-unref-destroy scroll))
        (when viewport (gtk-unref-destroy viewport))
        ))

    (define (gtk-div-contain-delete-inner o)
      ;; Deletes the content of the `<gtk-div-contain-type>` this
      ;; procedure is called by `gtk-delete-container-content` and by
      ;; `gtk-div-contain-delete-all`.
      (gtk-div-contain-delete-content (view o =>div-widget*!))
      )

    (define (gtk-div-contain-delete-all o)
      ;; Delete all widgets in a `<gtk-div-contain-type>`. This
      ;; procedure is used as the `=>div-on-delete*!` callback.
      (let*((wref (view o =>div-widget*!))
            (outer (gtk-div-contain-outer wref))
            )
        (gtk-div-contain-delete-content wref)
        (when outer (gtk-unref-destroy outer))
        ))

    ;;----------------------------------------------------------------

    (define (gtk-get-containing-box o)
      ;; Used for updating after state changes, returns the outer-most
      ;; Gtk box (if it exists) so that it's content can be replaced.
      ;;
      ;; `div` widgets may be contained within their own GtkBox or
      ;; GtkLayout widget. Most `div` widgets also contain other
      ;; widgets for rendering content. All of these widgets are
      ;; grouped together into a record structure. This procedure
      ;; returns the outer-most `GtkBox` or `GtkLayout` so that it can
      ;; be placed into a Gtk container widget.
      ;;
      ;; WARNING: if the outer-most box does not exist, this procedure
      ;; returns `#f`, usually so that an error can be reported. Use
      ;; `gtk-get-outer-widget` if you just want to store a Gtk widget
      ;; into a Gtk widget container regardless of whether it is
      ;; contained in a Gtk box widget.
      (cond
       ((gtk-div-boxed-type? o) (gtk-div-boxed-container o))
       ((gtk-div-contain-type? o) (gtk-div-contain-outer o))
       ((div-record-type? o) (gtk-get-containing-box (view o =>div-widget*!)))
       (else (error "not a widget container" o))
       ))

    (define (gtk-get-outer-widget o)
      ;; Used for placing widgets into containers, used to obtain a
      ;; Gtk widget that can be placed into some other Gtk container
      ;; widget.
      ;;
      ;; `div` widgets may be contained within their own GtkBox or
      ;; GtkLayout widget. Most `div` widgets also contain other
      ;; widgets for rendering content. All of these widgets are
      ;; grouped together into a record structure. This procedure
      ;; returns the outer-most `GtkBox` or `GtkLayout` so that it can
      ;; be placed into a Gtk container widget.
      ;;
      ;; WARNING: if the outer-most box does not exist, it returns the
      ;; inner widget. If you want only to replace the Gtk widget
      ;; contained within an outer Gtk box widget, and you want `#f`
      ;; to be returned when there is no outer box, you should use
      ;; `get-gtk-box` instead.
      (cond
       ((gtk-div-boxed-type? o)
        (or (gtk-div-boxed-container o)
            (gtk-div-boxed-content o)
            ))
       ((gtk-div-contain-type? o)        
        (or (gtk-div-contain-outer o)
            (gtk-div-contain-scroll o)
            (gtk-div-contain-viewport o)
            (gtk-div-contain-inner o)
            ))
       ((div-record-type? o)
        (gtk-get-outer-widget (view o =>div-widget*!))
        )
       (else o)
       ))

    ;;----------------------------------------------------------------

    (define (gtk-prepare-outer-box o outer align)
      ;; When a widget is being drawn, it may be drawing after a state
      ;; variable update, or it might be drawing anew. If drawing is
      ;; happening during an update, there is usually already an outer
      ;; `GtkBox` into which the widget content should be drawn. If
      ;; that outer widget does not exist, it must be created, which
      ;; is what this procedure does.
      (and
       (div-from-var? o)
       (or outer
           (let ((outer
                  (cond
                   ((eq? align 'cut-vertical)
                    (gi:make <GtkHBox> #:spacing 0 #:visible #t)
                    )
                   (else (gi:make <GtkVBox> #:spacing 0 #:visible #t))
                   )))
             (gobject-ref outer)
             outer
             ))))

    (define (gtk-div-updater old new)
      ;; This procedure takes the outer `GtkBox` of the `OLD` `div`
      ;; and delete's it's content, and asks the `NEW` `div` to draw
      ;; within the old outer `GtkBox`. This operation doesn't check
      ;; if an update needs to happen, it always redraws, so checking
      ;; for whether content needs to be updated should happen before
      ;; this procedure is called.
      (let*((old-wref (view old =>div-widget*!))
            (old-box  (gtk-get-outer-widget old-wref))
            )
        (gtk-delete-boxed-content old-wref)
        (gtk-draw-content new old-box)
        (widget:show old-box)
        ))

    (define (gtk-div-container-update old new)
      (let*((old-wref (view old =>div-widget*!))
            (old-box  (gtk-get-outer-widget old-wref))
            )
        (gtk-delete-container-content old-wref)
        (gtk-draw-content new old-box)
        (widget:show old-box)
        ))

    ;;================================================================

    (define (gtk-draw-content o outer)
      ;; Draw a `div` type `O`. In Gtk, every drawn `div` is placed in
      ;; it's own `GtkBox` so that if a state variable changes the
      ;; `div`, it can be modified in the widget tree by replacing
      ;; itself in the `GtkBox`. The `OUTER` argument is the `GtkBox`,
      ;; if `OUTER` is `#f` a new `GtkBox` must be constructed.
      (let*((cont (div-content o)))
        (cond
         ((div-space-type? cont) (gtk-draw-div-space o outer cont))
         ((div-pack-type?  cont) (gtk-draw-div-pack  o outer cont))
         ((div-grid-type?  cont) (gtk-draw-div-grid  o outer cont))
         ((use-vars-type?  cont) (gtk-draw-content (use-vars-value cont) outer))
         (else
          (let*((cont-str
                 (cond
                  ((string? cont) cont)
                  (else
                   (call-with-port (open-output-string)
                     (lambda (port) (write cont port) (get-output-string port))
                     ))))
                (props (view o =>div-properties*!))
                (ptype (div-view-type o))
                (wref
                 (cond
                  ((eq? ptype push-button) (gtk-draw-push-button cont props))
                  (else (gtk-draw-string cont))
                  ))
                (outer (gtk-prepare-outer-box o outer #f))
                )
            (cond
             (outer
              (container:add outer wref)
              (lens-set (make<gtk-div-boxed> outer wref) o =>div-widget*!)
              )
             (else
              (lens-set wref o =>div-widget*!)
              ))
            (lens-set gtk-div-delete o =>div-on-delete*!)
            (update
             ;; if the front-end developer has already set an updater
             ;; function, do not replace it. Otherwise use
             ;; `gtk-div-updater` as the updater.
             (lambda (old-updater) (or old-updater gtk-div-updater))
             o =>div-on-update*!
             )
            o)))))

    (define (gtk-draw-string o)
      (let ((label
             (gi:make
              <GtkLabel>
              #:label o
              #:visible #t
              #:halign 'start
              #:valign 'start
              #:selectable #t
              #:has-default #f
              #:focus-on-click #t ;; TODO: set this to #t but select none until click
              #:can-default #f
              #:vexpand #f
              #:hexpand #f
              )))
        (gobject-ref label)
        label
        ))

    (define (gtk-draw-push-button label props)
      (let*((action (prop-lookup 'on-button-push: props))
            (wref
             (gi:make
              <GtkButton>
              #:label label
              #:visible #t
              #:sensitive (procedure? action)
              #:vexpand #f
              #:hexpand #f
              )))
        (gobject-ref wref)
        (when (procedure? action)
          (gi:connect
           wref button:clicked
           (lambda _ (div-event-handler
                 (lambda _
                   (action)
                   #t)))))
        wref
        ))

    (define (%gtk-set-widget-width-height wid w h)
      (cond
       ((and (number? w) (number? h))
        (widget:set-size-request wid (floor w) (floor h))
        )
       ((and (number? w) (eq? h 'expand))
        (widget:set-size-request wid (floor w) -1)
        (widget:set-vexpand wid #t)
        )
       ((and (eq? w 'expand) (number? h))
        (widget:set-size-request wid -1 (floor h))
        (widget:set-hexpand wid #t)
        )
       (else
        (when (eq? w 'expand)
          (widget:set-hexpand wid #t)
          )
        (when (eq? h 'expand)
          (widget:set-vexpand wid #t)
          )
        (when (or (not w) (eq? w 'enclose))
          (widget:set-hexpand wid #f)
          )
        (when (or (not h) (eq? h 'enclose))
          (widget:set-vexpand wid #f)
          ))))

    (define gtk-set-widget-width-height
      (case-lambda
        ((wid size)
         (let*-values
             (((size)
               (cond
                ((rect2D-type? size) (rect2D-size size))
                ((size2D-type? size) size)
                ((not size) size)
                (else (error "not a size value" size))
                ))
              ((w h)
               (or (and size (values (size2D-width size) (size2D-height size)))
                   (values 'enclose 'enclose)
                   )))
           (%gtk-set-widget-width-height wid w h)
           ))
        ((wid w h) (%gtk-set-widget-width-height wid w h))
        ))
 
    ;;----------------------------------------------------------------

    (define (gtk-empty-div orient)
      (let ((wref
             (if (eq? orient cut-vertical)
                 (gi:make <GtkHBox> #:spacing 0 #:visible #t)
                 (gi:make <GtkVBox> #:spacing 0 #:visible #t)
                 )))
        (gobject-ref wref)
        wref
        ))

    (define (gtk-draw-div-pack o outer cont)
      ;; Draw a `div-pack` node, branches off to several different
      ;; drawing procedures depending on the node's properties.
      (let*((cont    (div-content o))
            (rect    (view o =>div-rect*!))
            (from    (div-pack-from cont))
            (orient  (div-pack-orientation cont))
            (sizes   (div-pack-subdiv-sizes cont))
            (subdivs (div-pack-subdivs cont))
            (nelems
             (min (or (and (vector? sizes)   (vector-length sizes))   0)
                  (or (and (vector? subdivs) (vector-length subdivs)) 0)
                  )))
        (cond
         ((= 0 nelems)
          (let ((wref (gtk-empty-div orient)))
            (lens-set wref o =>div-widget*!)
            ))
         ((= 1 nelems)
          (let*((wref (gtk-empty-div orient))
                (subdiv (gtk-draw-content (vector-ref subdivs 0) #f))
                )
            (container:add wref (gtk-get-outer-widget subdiv))
            (lens-set wref o =>div-widget*!)
            wref
            ))
         (else
          (let ((outer (gtk-prepare-outer-box o outer orient)))
            (cond
             ((eq? flags wrapping)
              (gtk-draw-pack-flow o outer from orient rect sizes subdivs)
              )
             ((eq? tiled-windows (div-view-type o))
              (gtk-draw-pack-tiled-windows o outer nelems from orient rect sizes subdivs)
              )
             (else
              (gtk-draw-pack-nowrap o outer from orient rect sizes subdivs)
              ))
            )))
        (lens-set gtk-div-contain-delete-all o =>div-on-delete*!)
        o))

    (define (gtk-container-nullify-focus container _widget)
      ;; A signal which disables the default focus behavior when a
      ;; container widget is created.
      ;;--------------------------------------------------------------
      ;;(display "; caught set-focus-child signal\n");;DEBUG
      (container:set-focus-child container #f)
      #f
      )

    (define (gtk-draw-pack-tiled-windows o outer nelems from orient rect sizes subdivs)
      (let*-values
          (((make-paned)
            (cond
             ((eq? orient cut-horizontal) (lambda () (gi:make <GtkVPaned> #:visible #t)))
             ((eq? orient cut-vertical)   (lambda () (gi:make <GtkHPaned> #:visible #t)))
             (else (error "unknown box orientation value" orient))
             ))
           ((add1 add2)
            (cond
             ((eq? from from-start) (values paned:add1 paned:add2))
             ((eq? from from-end)   (values paned:add2 paned:add1))
             (else (error "unknown box direction value" from))
             ))
           ((first-size)   (vector-ref sizes 0))
           ((first-subdiv) (vector-ref subdivs 0))
           ((first-subdiv) (gtk-draw-content first-subdiv #f))
           ((first-wref)   (view first-subdiv =>div-widget*!))
           ((splitpane)    (make-paned))
           ((nsplits)      (- nelems 1))
           ((split-wrefs)  (make-vector nsplits))
           ((wref)
            (make<gtk-div-contain>
             (or outer splitpane) #f #f split-wrefs)
            ))
        (lens-set split-wrefs o =>div-widget*!)
        (gobject-ref splitpane)
        (gtk-set-widget-width-height (gtk-get-outer-widget first-wref) rect)
        (add1 splitpane (gtk-get-outer-widget first-wref))
        (vector-set! split-wrefs 0 splitpane)
        (let loop ((i 1) (splitpane splitpane))
          (let*((size        (vector-ref sizes i))
                (subdiv      (gtk-draw-content (vector-ref subdivs i) #f))
                (subdiv-wref (view subdiv =>div-widget*!))
                )
            (cond
             ((< i nsplits)
              (let*((next (make-paned)))
                (gobject-ref next)
                (vector-set! split-wrefs i next)
                (add2 splitpane next)
                (add1 next (gtk-get-outer-widget subdiv-wref))
                (loop (+ 1 i) next)
                ))
             (else (add2 splitpane (gtk-get-outer-widget subdiv-wref)))
             )))
        (cond
         (outer
          (container:add outer splitpane)
          (widget:show outer)
          )
         (else
          (widget:show splitpane)
          ))
        (lens-set gtk-div-container-update o =>div-on-update*!)
        (lens-set wref o =>div-widget*!)
        o))

    (define (gtk-make-scroller o orient)
      (let*((scrollprop (div-prop-lookup 'scrollbar: o))
            (scrollprop
             (cond
              ((eq? scrollprop #t)
               (cond
                ((eq? orient 'cut-horizontal) 'horizontal)
                ((eq? orient 'cut-vertical)   'vertical)
                (else scrollprop)
                ))
              (else scrollprop)
              )))
        (cond
         ((not scrollprop) #f)
         ((eq? scrollprop 'vertical)
          (gi:make
           <GtkScrolledWindow>
           #:vscrollbar-policy 'policy-automatic
           #:hscrollbar-policy 'policy-never
           ))
         ((eq? scrollprop 'horizontal)
          (gi:make
           <GtkScrolledWindow>
           #:vscrollbar-policy 'policy-never
           #:hscrollbar-policy 'policy-automatic
           ))
         ((eq? scrollprop 'both)
          (gi:make
           <GtkScrolledWindow>
           #:vscrollbar-policy 'policy-automatic
           #:hscrollbar-policy 'policy-automatic
           ))
         (else (error "unknown value for 'scrollbar: property" scrollprop))
         )))

    (define (gtk-draw-pack-flow o outer from orient rect sizes subdivs)
      (let*((scroll (gtk-make-scroller o orient))
            (viewport (gi:make <GtkViewport>))
            (flowbox
             (gi:make
              <GtkFlowBox>
              (cond
               ((eq? orient cut-horizontal) 'vertical)
               ((eq? orient cut-vertical) 'horizontal)
               ((not orient) 'horizontal)
               (else (error "unknown box orientation value" orient))
               )
              #:selection-mode 'selection-multiple
              ))
            (counter (vector-length (div-pack-subdiv-sizes o)))
            (pack-start
             (lambda (child)
               (flow-box:insert flowbox child counter)
               (set! counter (+ 1 counter))
               ))
            (pack-end
             (lambda (child)
               (set! counter (- counter 1))
               (flow-box:insert flowbox child counter)
               ))
            (pack
             (cond
              ((eq? from from-end) pack-end)
              ((eq? from from-start) (set! counter 0) pack-start)
              ((not from) (set! counter 0) pack-start)
              (else (error "unknown box direction value" from))
              ))
            (wref (make<gtk-div-contain> outer scroll viewport flowbox))
            )
        (gobject-ref flowbox)
        (gobject-ref viewport)
        (cond
         (scroll
          (gobject-ref scroll)
          (when outer (container:add outer scroll))
          (container:add scroll viewport)
          )
         (else
          (when outer (container:add outer viewport))
          ))
        (container:add viewport flowbox)
        (gtk-set-widget-width-height wref rect)
        (vector-for-each
         (lambda (size child)
           (let*((child (gtk-draw-content child #f))
                 (wref (view child =>div-widget*!))
                 )
             (when wref (pack (gtk-get-outer-widget wref)))
             (values)
             ))
         sizes subdivs
         )
        (lens-set wref o =>div-widget*!)
        (lens-set gtk-div-container-update o =>div-on-update*!)
        o))

    (define (gtk-draw-pack-nowrap o outer from orient rect sizes subdivs)
      (let*((scroll (gtk-make-scroller o orient))
            (viewport (and scroll (gi:make <GtkViewport> #:visible #t)))
            (box-wref
             (cond
              ((eq? orient cut-horizontal) (gi:make <GtkVBox> #:spacing 0 #:visible #t))
              ((eq? orient cut-vertical)   (gi:make <GtkHBox> #:spacing 0 #:visible #t))
              ((not orient) (gi:make <GtkVBox> #:spacing 0 #:visible #t))
              (else (error "unknown box orientation value" orient))
              ))
            (pack-start (lambda (widget) (box:pack-start box-wref widget #f #f 0)))
            (pack-end   (lambda (widget) (box:pack-end   box-wref widget #f #f 0)))
            (pack
             (cond
              ((eq? from from-end) pack-end)
              ((eq? from from-start) pack-start)
              ((not from) pack-start)
              (else (error "unknown box direction value" from))
              ))
            (wref (make<gtk-div-contain> outer scroll viewport box-wref))
            )
        (gobject-ref box-wref)
        (gtk-set-widget-width-height box-wref rect)
        (cond
         (scroll
          (gobject-ref scroll)
          (gobject-ref viewport)
          (when outer (container:add outer scroll))
          (container:add scroll viewport)
          (container:add viewport box-wref)
          )
         (else
          (when outer (container:add outer box-wref))
          ))
        (vector-for-each
         (lambda (size child)
           (let*((child (gtk-draw-content child #f))
                 (wref (view child =>div-widget*!))
                 )
             (when wref (pack (gtk-get-outer-widget wref)))
             (values)
             ))
         sizes subdivs
         )
        (lens-set wref o =>div-widget*!)
        (lens-set gtk-div-container-update o =>div-on-update*!)
        o))

    (define (gtk-draw-div-space o outer cont)
      (let ((elems (div-space-elements cont)))
        (cond
         ((not elems) (gtk-empty-div cut-horizontal))
         (else
          (let*((scroll (gtk-make-scroller o #t))
                (outer  (gtk-prepare-outer-box o outer #f))
                (layout (gi:make <GtkLayout>))
                (wref   (make<gtk-div-contain> outer scroll #f layout))
                )
            (gobject-ref layout)
            (vector-for-each
             (lambda (flo)
               (let*((rect   (floater-rect flo))
                     (pt     (rect2D-point rect))
                     (pt     (if (or (not pt) (symbol? pt)) (point2D 0 0) pt))
                     (size   (rect2D-size rect))
                     (x      (point2D-x pt))
                     (y      (point2D-y pt))
                     (w      (size2D-width size))
                     (h      (size2D-height size))
                     (subdiv (gtk-draw-content (floater-div flo) #f))
                     (wid    (gtk-get-outer-widget subdiv))
                     )
                 (%gtk-set-widget-width-height wid w h)
                 (layout:put layout wid x y)
                 ))
             elems
             )
            (gobject-ref layout)
            (cond
             (scroll
              (gobject-ref scroll)
              (when outer (container:add outer scroll))
              (container:add scroll layout)
              )
             (else
              (when outer (container:add outer layout))
              ))
            (lens-set wref o =>div-widget*!)
            (lens-set gtk-div-container-update o =>div-on-update*!)
            o)))))

    (define (gtk-draw-div-grid o outer cont)
      (let*((outer (gtk-prepare-outer-box o outer #f))
            (scroll (gtk-make-scroller o (div-prop-lookup 'scrolled: o)))
            (viewport (and scroll (gi:make <GtkViewport> #:has-focus #f #:can-default #f)))
            (grid (gi:make <GtkGrid> #:has-focus #f #:can-default #f))
            (wref (make<gtk-div-contain> outer scroll viewport grid))
            )
        (gobject-ref grid)
        (gi:connect grid container:add gtk-container-nullify-focus)
        ;; ^ TODO: this doesn't prevent the first element in the grid from
        ;;         being foused auomatically. Need to find out how to make
        ;;         sure nothing is selected when the widget is realized.
        (when viewport (gobject-ref viewport))
        (lens-set gtk-div-delete o =>div-on-delete*!)
        ((for-each-div-grid
          (lambda (subdiv x y w h)
            (let*((subdiv (gtk-draw-content subdiv #f))
                  (subdiv-wref (view subdiv =>div-widget*!))
                  )
              (%gtk-set-widget-width-height subdiv-wref w h)
              (cond
               ((not subdiv) (values))
               (else
                (grid:attach grid subdiv-wref x y 1 1)
                (values)
                )))))
         cont
         )
        (cond
         (scroll
          (gobject-ref scroll)
          (gobject-ref viewport)
          (when outer (container:add outer scroll))
          (container:add scroll viewport)
          (container:add viewport grid)
          )
         (else
          (when outer (container:add outer grid))
          ))
        (lens-set wref o =>div-widget*!)
        (lens-set gtk-div-container-update o =>div-on-update*!)
        o))

    ;;----------------------------------------------------------------
    ;; Text Buffer API wrapper

    (define (gtk-buffer-type? o)
      (gi:is-a? o <GtkTextBuffer>)
      )

    (define (gtk-new-buffer)
      (gi:make <GtkTextBuffer>)
      )

    (define (gtk-style-type? o)
      (gi:is-a? o <GtkTextTag>)
      )

    (define (gtk-new-style props)
      (call-with-port (open-output-string)
        (lambda (port)
          (vbal-for-each
           (lambda (sym val)
             (write-string (symbol->string sym) port)
             (write val port)
             (write-char #\; port)
             )
           props
           )
          (let*((props (get-output-string port))
                (style (text-tag:new props))
                )
            (display "; new GtkTextTag object with properties: ");;DEBUG
            (write result);;DEBUG
            (newline);;DEBUG
            (gobject-ref props)
            style
            ))))

    (define (gtk-buffer-length buffer)
      (text-buffer:get-char-count buffer)
      )

    (define (gtk-text-load-port buffer port flags)
      ;; TODO: handle `FLAGS` to allow for markdown and HTML parsing.
      (cond
       ((not (gtk-buffer-type? buffer))
        (error "not a GtkTextBuffer" buffer)
        )
       ((not (input-port-open? port))
        (error "not an open input port" port)
        )
       (else
        (let ((bufsize (gtk-buffer-length buffer)))
          (let loop ((count 0))
            (let*((next-chunk (read-string bufsize))
                  (chunk-size (string-length next-chunk)))
              (cond
               ((eof-object? next-chunk) count)
               (else
                (text-buffer:insert-at-cursor buffer next-chunk chunk-size)
                (display "; inserted ") (write chunk-size) (display " characters") (newline) ;;DEBUG
                (loop (+ count chunk-size))
                ))))))))

    (define (gtk-text-dump-port buffer port flags)
      (cond
       ((not (gtk-buffer-type? buffer))
        (error "not a GtkTextBuffer" buffer)
        )
       ((not (output-port-open? port))
        (error "not an open output port" port)
        )
       (else
        (let*((chunk-size (*text-load-buffer-size*))
              (bufsize (gtk-buffer-length buffer))
              )
          (let loop ((lo 0) (hi (min bufsize chunk-size)))
            (cond
             ((< lo bufsize)
              (let*((lo-iter
                     (text-buffer:get-iter-at-offset!
                      buffer (gi:make <GtkTextIter>)
                      lo))
                    (hi-iter
                     (text-buffer:get-iter-at-offset!
                      buffer (gi:make <GtkTextIter>)
                      hi))
                    (chunk (text-buffer:get-slice lo-iter hi-iter #t))
                    )
                (string-for-each (lambda (c) (write-char c port)) chunk)
                (loop (+ lo chunk-size) (min bufsize (+ hi chunk-size)))
                ))
             (else (values))
             ))))))

    (define (parameterized-gtk-api thunk)
      (parameterize
          ((*impl/buffer-type?*     gtk-buffer-type?)
           (*impl/new-buffer*       gtk-new-buffer)
           (*impl/style-type?*      gtk-style-type?)
           (*impl/new-style*        gtk-new-style)
           (*impl/buffer-length*    gtk-buffer-length)
           (*impl/text-load-port*   gtk-text-load-port)
           (*impl/text-dump-port*   gtk-text-dump-port)
           )
        (thunk)
        ))

    ;;----------------------------------------------------------------

    (define *gtk-application-object*
      (begin
        (gi:use-typelibs
         (("GLib" "2.0")
          #:prefix glib:))

        (gi:use-typelibs
         (("Gio" "2.0")
          #:select
          (activate)
          #:prefix gio:
          ))

        (gi:use-typelibs
         (("GObject" "2.0")
          #:prefix gobject-
          #:select
          (ref unref)
          ))

        ;; (gi:use-typelibs
        ;;  (("Gdk" "3.0")
        ;;   #:prefix gdk:)
        ;;  )

        (gi:use-typelibs
         (("Gtk" "3.0")
          #:prefix gtk:)
         )

        (push-duplicate-handler! 'merge-generics)
        (push-duplicate-handler! 'shrug-equals)

        (gi-repo:load-by-name "Gdk" "keyval_name")
        (gi-repo:load-by-name "Gdk" "keyval_to_unicode")
        (gi-repo:load-by-name "Gdk" "Event")
        (gi-repo:load-by-name "Gdk" "EventMask")
        (gi-repo:load-by-name "Gio" "Application")
        (gi-repo:load-by-name "Gdk" "ModifierType")
        (gi-repo:load-by-name "Gtk" "Align")
        (gi-repo:load-by-name "Gtk" "Application")
        (gi-repo:load-by-name "Gtk" "ApplicationWindow")
        (gi-repo:load-by-name "Gtk" "Button")
        (gi-repo:load-by-name "Gtk" "Container")
        (gi-repo:load-by-name "Gtk" "EventController")
        (gi-repo:load-by-name "Gtk" "EventControllerKey")
        (gi-repo:load-by-name "Gtk" "FlowBox")
        (gi-repo:load-by-name "Gtk" "Grid")
        (gi-repo:load-by-name "Gtk" "IMContext")
        (gi-repo:load-by-name "Gtk" "Label")
        (gi-repo:load-by-name "Gtk" "Layout")
        (gi-repo:load-by-name "Gtk" "Paned")
        (gi-repo:load-by-name "Gtk" "HPaned")
        (gi-repo:load-by-name "Gtk" "VPaned")
        (gi-repo:load-by-name "Gtk" "Orientation")
        (gi-repo:load-by-name "Gtk" "Separator")
        (gi-repo:load-by-name "Gtk" "ScrolledWindow")
        (gi-repo:load-by-name "Gtk" "TextBuffer")
        (gi-repo:load-by-name "Gtk" "TextTag")
        (gi-repo:load-by-name "Gtk" "TextIter")
        (gi-repo:load-by-name "Gtk" "TextView")
        (gi-repo:load-by-name "Gtk" "DeleteType")
        (gi-repo:load-by-name "Gtk" "Viewport")
        (gi-repo:load-by-name "Gtk" "Widget") ;; show-all
        (gi-repo:load-by-name "Gtk" "Window")
        (gi-repo:load-by-name "Gtk" "Box")
        (gi-repo:load-by-name "Gtk" "HBox")
        (gi-repo:load-by-name "Gtk" "VBox")

        (gi:make <GtkApplication> #:application-id "page.codeberg.schemacs")
        ))

    ;;----------------------------------------------------------------
    ))

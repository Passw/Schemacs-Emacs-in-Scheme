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
    (rename (schemacs ui)
            (div-set-focus*  *impl/div-set-focus*)
            (is-graphical-display?* *impl/is-graphical-display?*)
            )
    (prefix (schemacs ui text-buffer-impl) *impl/)
    (prefix (schemacs editor) ed:)
    (prefix (schemacs keymap) km:)
    (only (schemacs ui text-buffer) *text-load-buffer-size*)
    (only (schemacs pretty) display-lines pretty print line-break qstr)
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
   main  gtk-draw-div
   *gtk-application-object*
   *gtk-selected-window*
   )

  (begin
    ;;----------------------------------------------------------------

    (define (gtk-is-graphical-display?) #t)

    (define-record-type <gtk-global-state-type>
      (make<gtk-global-state> msgbuf)
      gtk-global-state-type?
      (msgbuf  gtk-global-messages-buffer  set!gtk-global-messages-buffer)
      )

    (define gtk-global-state
      (make<gtk-global-state> #f)
      )

    (define (gtk-global-get-messages-buffer)
      (gtk-global-messages-buffer gtk-global-state)
      )

    (define *default-window-title* "Schemacs")

    (define *gtk-selected-window* (make-parameter #f))

    (define (on-main-loop-start window-name window-size top)
      ;; Construct an event handler that is handles the application
      ;; startup event. This procedure returns an event handler
      ;; procedure that renders the content of a `div` constructed by
      ;; the `TOP` monad, and also sets the application window's title
      ;; to `WINDOW-NAME` and initializes it's size to `WINDOW-SIZE`.
      (lambda (app)
        (let*((settings (settings:get-default))
              (div (gtk-draw-app-window top))
              (wref
               (cond
                ((floater-type? div) (view (floater-div div) =>div-widget*!))
                ((div-record-type? div) (view div =>div-widget*!))
                ((not div) (display "WARNING: no `DIV` content to display\n") #f)
                (else (error "not a `DIV` type" div))
                )))
          (when settings
            (set! (settings:gtk-label-select-on-focus settings) #f)
            )
          (when wref
            (widget:show (gtk-get-outer-widget wref))
            ))))

    (define (gtk-draw-div top)
      ;; This is the entrypoint into the program. Apply a `div`
      ;; constructing monad as the `TOP` argument to this
      ;; procedure. When you do the Gtk event loop begins and renders
      ;; the result of the `div`. This procedure will not return until
      ;; the Application window is closed and the Gtk event loop halts.
      (cond
       ((application:register? *gtk-application-object* #f)
        (let*((top (if (or (div-monad-type? top) (use-vars-type? top))
                       (run-div-monad  top)
                       top
                       ))
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

    (define (main . scheme-args)
      ;; TODO: handle scheme-args
      (parameterized-gtk-api
       (lambda () (gtk-draw-div (apply ed:main scheme-args)))
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
      (let ((vis (not (div-prop-lookup 'hidden: o))))
        (and
         (div-from-var? o)
         (or outer
             (let ((outer
                    (cond
                     ((eq? align 'cut-vertical)
                      (gi:make
                       <GtkHBox>
                       #:spacing 0
                       #:visible vis
                       #:expand #t
                       #:valign 'fill
                       #:halign 'fill
                       ))
                     (else
                      (gi:make
                       <GtkVBox>
                       #:spacing 0
                       #:visible vis
                       #:expand #t
                       #:valign 'fill
                       #:halign 'fill
                       )))))
               (gobject-ref outer)
               outer
               )))))

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
    ;; Gtk event handler infrastructure

    (define (gtk-set-event-handler wref gtk-signal action)
      (gi:connect
       wref gtk-signal
       (lambda (wref event)
         (gtk3-event-handler (lambda () (action event)))
         )))

    (define (gtk-widget-set-event-handlers wref props)
      (let*((on-key-event   (prop-lookup 'on-key-event:   props))
            (on-focus-event (prop-lookup 'on-focus-event: props))
            (on-focus-in    (prop-lookup 'on-focus-in:    props))
            (on-focus-out   (prop-lookup 'on-focus-out:   props))
            (lazy           (lambda (proc . args) (lambda _ (apply proc args))))
            (set-handler
             (lambda (signal action) (gtk-set-event-handler wref signal action))
             ))
        (cond
         ((procedure? on-focus-event)
          (set-handler widget:focus-in-event  (lazy on-focus-event #t))
          (set-handler widget:focus-out-event (lazy on-focus-event #f))
          )
         (else
          (when (procedure? on-focus-in)
            (set-handler widget:focus-in-event  (lazy on-focus-in))
            )
          (when (procedure? on-focus-out)
            (set-handler widget:focus-out-event (lazy on-focus-out))
            )))
        (when (procedure? on-key-event)
          (set-handler
           (gi:make <signal> #:name "key-press-event")
           (lambda (event)
             (gtk-key-press-event-handler on-key-event event)
             )))))

    (define (gtk-key-press-event-handler on-key-event event)
      (let*-values
          (((val-ok   keyval)    (event:get-keyval event))
           ((code-ok  keycode)   (event:get-keycode event))
           ((state-ok modifiers) (event:get-state event))
           ((mod-bits)           (modifier-type->number modifiers))
           ((unicode)            (gtk-keyval-normalize keyval))
           )
        ;; (pretty ;;DEBUG
        ;;  (print ;;DEBUG
        ;;   ";;key-event (keyval: " keyval ;;DEBUG
        ;;   " keycode: " keycode           ;;DEBUG
        ;;   " modifiers: " modifiers       ;;DEBUG
        ;;   " mod-bits: " mod-bits         ;;DEBUG
        ;;   " unicode: " unicode           ;;DEBUG
        ;;   ")" (line-break)               ;;DEBUG
        ;;   ))                             ;;DEBUG
        ;;;; Example output of the above format statement after pressing space bar:
        ;;;;     (key-event #x20 #x41 ())
        (cond
         ((and (= 0 mod-bits) (char=? unicode #\null))
          ;; Ignored because this is a key press of a modifier without an
          ;; accompanying printing character.
          #t)
         (else
          (let*((mod-list (modifier-type->list modifiers))
                (km-index (km:keymap-index (gtk-keymod->keymap-mod mod-list unicode)))
                )
            (gtk3-event-handler (lambda () (on-key-event km-index)))
            ;; Must return #t to prevent this event from propagating up
            ;; the widget tree to the parent TextView object and
            ;; triggering the default event handler.
            #t)))))

    (define (gtk-keymod->keymap-mod mod-list keyval)
      ;; This function is responsible for constructing keyboard modifier
      ;; symbols from a modifier mask taken from a key event. Modifier
      ;; masks can also be returned with mouse events. See also
      ;; "GTK-KEYVAL-NORMALIZE".
      (let loop ((mod-list mod-list) (keyval keyval))
        (cond
         ((null? mod-list) (list keyval))
         (else
          (let ((mod (car mod-list)))
            (cond
             ((eq? mod 'shift-mask)   (loop (cdr mod-list) (char-upcase keyval)))
             ((eq? mod 'alt-mask)     (cons 'meta (loop (cdr mod-list) keyval)))
             ((eq? mod 'control-mask) (cons 'control (loop (cdr mod-list) keyval)))
             ((eq? 'mod1-mask)
              ;; Ignore 'mod1-mask as it is typically only used for
              ;; alternative input methods. Also note: though `eq?` is
              ;; used to compare a symbol `mod1-mask` to the symbol
              ;; returned by `modifier-type->number` (which returns a
              ;; list of symbols) and if evaluate `(eq? 'mod1-mask m)`
              ;; when `m` is a symbol `modifier-reserved-25-mask`
              ;; returned by `modifier-type->number`, `eq?` evaluates
              ;; to `#t`. This ;; is definitely a bug, probably in
              ;; Guile GOOPS. Oopsie.
              (loop (cdr mod-list) keyval)
              )
             (else
              (display "; unknown Gtk key modifier symbol ") (write mod) (newline) ;;DEBUG
              (loop (cdr mod-list) keyval)
              )))))))

    (define (gtk-keyval-normalize keyval)
      ;; This function is responsible for decoding the "keyval" portion of
      ;; a key event to a character or string, which are the keys that are
      ;; NOT modifiers like "Control" or "Alt." If a character is
      ;; returned, there is an ASCII character associated with the key
      ;; pressed, such as a letter or number keys, enter, delete, or
      ;; escape.  If a string is returned, the string is a symbol of the
      ;; key on the keyboard that was pressed, which might be (for
      ;; example) an arrow key, backspace, or a function key. See also
      ;; "GTK-KEYMOD->KEYMAP-MOD"
      (let ((norm-keyval (keyval-to-unicode keyval)))
        (cond
         ((and (<= keyval #x8000) (> norm-keyval 0))
          (integer->char norm-keyval)
          )
         (else
          (integer->char (keyval-to-unicode keyval))
          ))))

    ;;----------------------------------------------------------------
    ;; Gtk widgets accounting for size

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

    ;;================================================================
    ;; Gtk widget rendering

    (define (gtk-draw-content o outer)
      ;; Draw a `div` type `O`. In Gtk, every drawn `div` is placed in
      ;; it's own `GtkBox` so that if a state variable changes the
      ;; `div`, it can be modified in the widget tree by replacing
      ;; itself in the `GtkBox`. The `OUTER` argument is the `GtkBox`,
      ;; if `OUTER` is `#f` a new `GtkBox` must be constructed.
      (let*((cont (div-content o)))
        (cond
         ((not cont) o)
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
                  ((or (eq? ptype text-editor) (gtk-buffer-type? cont))
                   (gtk-draw-text-editor o cont props)
                   )
                  (else (gtk-draw-string cont props))
                  ))
                (outer (gtk-prepare-outer-box o outer #f))
                (inner (gtk-get-outer-widget wref))
                )
            (cond
             (outer
              (container:add outer inner)
              (lens-set (make<gtk-div-boxed> outer wref) o =>div-widget*!)
              )
             (else (lens-set wref o =>div-widget*!))
             )
            (lens-set gtk-div-delete o =>div-on-delete*!)
            (update
             ;; if the front-end developer has already set an updater
             ;; function, do not replace it. Otherwise use
             ;; `gtk-div-updater` as the updater.
             (lambda (old-updater) (or old-updater gtk-div-updater))
             o =>div-on-update*!
             )
            o)))))

    (define (gtk-draw-app-window o/flo)
      ;; This draws the root of the `DIV` tree before calling the
      ;; recurisve procedure `gtk-draw-content` which draws the rest
      ;; of the whole tree. The root requires special treatment
      ;; because a top-level application window needs to be created,
      ;; and the arguments used to creates the app window may differ
      ;; based on whether the argument `O` is a floater type or a
      ;; div-record type.
      (let*-values
          (((o rect)
            (cond
             ((floater-type? o/flo)
              (values (floater-div o/flo) (floater-rect o/flo))
              )
             (else (values o/flo (rect2D 0 0 800 640)))
             )))
        (let*((size (rect2D-size rect))
              (props (and (div-record-type? o) (view o =>div-properties*!)))
              (title (prop-lookup 'title: props))
              (win-wref
               (gi:make
                <GtkApplicationWindow>
                #:application *gtk-application-object*
                #:title (or title *default-window-title*)
                #:valign 'fill
                #:halign 'fill
                #:expand #t
                ))
              (child (gtk-draw-content o #f))
              )
          (gobject-ref win-wref)
          (container:add win-wref (gtk-get-outer-widget child))
          (lens-set win-wref o =>div-widget*!)
          o/flo
          )))

    (define (gtk-draw-string o props)
      (let ((wref
             (gi:make
              <GtkLabel>
              #:label o
              #:visible (not (prop-lookup 'hidden: props))
              #:halign 'start
              #:valign 'start
              #:selectable #t
              #:has-default #f
              #:focus-on-click #t
              #:can-default #f
              #:expand #f
              ;; TODO: set the #:attributes with Pango font properties
              )))
        (gobject-ref wref)
        (gtk-widget-set-event-handlers wref props)
        wref
        ))

    (define (gtk-draw-push-button label props)
      (let*((action (prop-lookup 'on-button-push: props))
            (wref
             (gi:make
              <GtkButton>
              #:label label
              #:visible (not (prop-lookup 'hidden: props))
              #:sensitive (procedure? action)
              #:expand #f
              )))
        (gobject-ref wref)
        (gtk-widget-set-event-handlers wref props)
        (when (procedure? action)
          (gi:connect
           wref button:clicked
           (lambda _
             (gtk3-event-handler
              (lambda () (div-event-handler (lambda _ (action) #t)))
              ))))
        wref
        ))

    (define (gtk-draw-text-editor o buffer props)
      (let*((viewport
             (gi:make
              <GtkTextView>
              #:buffer buffer
              #:expand #t
              #:valign 'fill
              #:halign 'fill
              #:visible (not (prop-lookup 'hidden: props))
              #:monospace (not (prop-lookup 'default-font: props))
              ))
            (scroll
             (gi:make
              <GtkScrolledWindow>
              #:vscrollbar-policy 'automatic
              #:hscrollbar-policy 'automatic
              #:visible (not (prop-lookup 'hidden: props))
              #:expand #t
              #:valign 'fill
              #:halign 'fill
              #:child viewport
              ))
            (wref
             (make<gtk-div-contain>
              #f scroll viewport buffer
              )))
        (gobject-ref viewport)
        (gobject-ref scroll)
        (gtk-widget-set-event-handlers viewport props)
        wref
        ))

    ;;----------------------------------------------------------------

    (define (gtk-empty-div orient)
      (let ((wref
             (if (eq? orient cut-vertical)
                 (gi:make <GtkHBox> #:spacing 0)
                 (gi:make <GtkVBox> #:spacing 0)
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

    (define (gtk-draw-pack-tiled-windows o outer nelems from orient rect sizes subdivs)
      (let*-values
          (((props) (view o =>div-properties*!))
           ((make-paned)
            (cond
             ((eq? orient cut-horizontal)
              (lambda ()
                (gi:make
                 <GtkVPaned>
                 #:visible (not (prop-lookup 'hidden: props))
                 )))
             ((eq? orient cut-vertical)
              (lambda ()
                (gi:make
                 <GtkHPaned>
                 #:visible (not (prop-lookup 'hidden: props))
                 )))
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
             (or outer splitpane) #f #f split-wrefs
             )))
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
           #:vscrollbar-policy 'automatic
           #:hscrollbar-policy 'never
           ))
         ((eq? scrollprop 'horizontal)
          (gi:make
           <GtkScrolledWindow>
           #:vscrollbar-policy 'never
           #:hscrollbar-policy 'automatic
           ))
         ((eq? scrollprop 'both)
          (gi:make
           <GtkScrolledWindow>
           #:vscrollbar-policy 'automatic
           #:hscrollbar-policy 'automatic
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
            (props (view o =>div-properties*!))
            (vis (not (prop-lookup 'hidden: props)))
            (viewport (and scroll (gi:make <GtkViewport> #:visible vis)))
            (box-wref
             (cond
              ((eq? orient cut-horizontal)
               (gi:make
                <GtkVBox>
                #:spacing 0
                #:visible vis
                #:valign 'start
                #:halign 'start
                #:expand #t
                ))
              ((eq? orient cut-vertical)
               (gi:make
                <GtkHBox>
                #:spacing 0
                #:visible vis
                #:valign 'start
                #:halign 'start
                #:expand #t
                ))
              ((not orient)
               (gi:make
                <GtkVBox>
                #:spacing 0
                #:visible vis
                #:valign 'start
                #:halign 'start
                #:expand #t
                ))
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
            (viewport
             (and scroll (gi:make <GtkViewport> #:has-focus #f #:can-default #f))
             )
            (grid (gi:make <GtkGrid> #:has-focus #f #:can-default #f))
            (wref (make<gtk-div-contain> outer scroll viewport grid))
            )
        (gobject-ref grid)
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
            (display "; new GtkTextTag object with properties: ") ;;DEBUG
            (write props) ;;DEBUG
            (newline)     ;;DEBUG
            (gobject-ref props)
            style
            ))))

    (define (gtk-buffer-method proc)
      (lambda (buf . args)
        (apply proc (view buf ed:=>buffer-view) args)
        ))

    (define gtk-buffer-length 
      (gtk-buffer-method
       (lambda (buffer)
         (text-buffer:get-char-count buffer)
         )))

    (define gtk-text-load-port 
      ;; TODO: handle `FLAGS` to allow for markdown and HTML parsing.
      (gtk-buffer-method
       (lambda (buffer port flags)
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
                     (chunk-size (string-length next-chunk))
                     )
                 (cond
                  ((eof-object? next-chunk) count)
                  (else
                   (text-buffer:insert-at-cursor buffer next-chunk chunk-size)
                   ;;(display "; inserted ") (write chunk-size) (display " characters") (newline) ;;DEBUG
                   (loop (+ count chunk-size))
                   ))))))))))

    (define gtk-text-dump-port 
      (gtk-buffer-method
       (lambda (buffer port flags)
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
                         buffer (gi:make <GtkTextIter>) lo
                         ))
                       (hi-iter
                        (text-buffer:get-iter-at-offset!
                         buffer (gi:make <GtkTextIter>) hi
                         ))
                       (chunk (text-buffer:get-slice lo-iter hi-iter #t))
                       )
                   (string-for-each (lambda (c) (write-char c port)) chunk)
                   (loop (+ lo chunk-size) (min bufsize (+ hi chunk-size)))
                   ))
                (else (values))
                ))))))))

    (define gtk-get-cursor-index 
      (gtk-buffer-method
       (lambda (buffer)
         (text-buffer:cursor-position bufrer)
         )))

    (define gtk-set-cursor-index
      (gtk-buffer-method
       (lambda (buffer index)
         (text-buffer:cursor-position buffer index)
         )))

    (define gtk-move-cursor-index
      (gtk-buffer-method
       (lambda (buffer index)
         (let*((cursor (text-buffer:cursor-position buffer)))
           (text-buffer:cursor-position buffer (+ cursor index))
           ))))

    (define gtk-set-cursor-position
      (gtk-buffer-method
       (lambda (buffer line column)
         (let*((iter
                (text-buffer:get-iter-at-line-offset
                 buffer (gi:make <GtkTextIter>) line column
                 )))
           (text-buffer:place-cursor buffer iter)
           ))))

    (define gtk-index->line-column
      (gtk-buffer-method
       (lambda (buffer index)
         (let*((iter
                (text-buffer:get-iter-at-offset
                 buffer (gi:make <GtkTextIter>) index
                 ))
               (line (text-iter:get-line iter))
               (column (text-iter:get-line-offset iter))
               )
           (text-buffer:cursor-position buffer old-index)
           (values line column)
           ))))

    (define gtk-delete-range
      (gtk-buffer-method
       (lambda (buffer from to)
         (let*((from-iter
                (text-buffer:get-iter-at-offset!
                 buffer (gi:make <GtkTextIter>) from
                 ))
               (to-iter
                (text-buffer:get-iter-at-offset!
                 buffer (gi:make <GtkTextIter>) to
                 )))
           (text-buffer:delete buffer from-iter to-iter)
           ))))

    (define gtk-delete-from-cursor
      (gtk-buffer-method
       (lambda (buffer to)
         (let*((cursor (text-buffer:cursor-position buffer))
               (cursor-iter
                (text-buffer:get-iter-at-offset!
                 buffer (gi:make <GtkTextIter>) cursor
                 ))
               (to-iter
                (text-buffer:get-iter-at-offset!
                 buffer (gi:make <GtkTextIter>) (- cursor to)
                 )))
           (text-buffer:delete buffer cursor-iter to-iter)
           ))))

    (define gtk-insert-string
      (gtk-buffer-method
       (lambda (buffer str)
         (cond
          ((char? str)
           (text-buffer:insert-at-cursor buffer (make-string 1 str) 1)
           )
          ((string? str)
           (text-buffer:insert-at-cursor buffer str (string-length str))
           )
          (else (error "not a string or character" str))
          ))))

    (define (parameterized-gtk-api thunk)
      (parameterize
          ((*impl/is-graphical-display?* gtk-is-graphical-display?)
           (*impl/buffer-type?*          gtk-buffer-type?)
           (*impl/new-buffer*            gtk-new-buffer)
           (*impl/style-type?*           gtk-style-type?)
           (*impl/new-style*             gtk-new-style)
           (*impl/buffer-length*         gtk-buffer-length)
           (*impl/text-load-port*        gtk-text-load-port)
           (*impl/text-dump-port*        gtk-text-dump-port)
           (*impl/get-cursor-index*      gtk-get-cursor-index)
           (*impl/set-cursor-index*      gtk-set-cursor-index)
           (*impl/move-cursor-index*     gtk-move-cursor-index)
           (*impl/set-cursor-position*   gtk-set-cursor-position)
           (*impl/index->line-column*    gtk-index->line-column)
           (*impl/delete-from-cursor*    gtk-delete-from-cursor)
           (*impl/insert-string*         gtk-insert-string)
           (*impl/insert-char*           gtk-insert-string)
           (*impl/delete-range*          gtk-delete-range)
           (*impl/delete-from-cursor*    gtk-delete-from-cursor)
           )
        (thunk)
        ))

    (define (gtk3-event-handler proc)
      (parameterized-gtk-api
       (lambda ()
         ;; NOTE: to do print debugging, you can pass
         ;; `(current-output-port)` as the first argument to
         ;; `div-event-handler` like so:
         ;;     (div-event-handler (current-output-port) proc)
         (div-event-handler proc)
         )))

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
        (gi-repo:load-by-name "Gtk" "Settings")
        (gi-repo:load-by-name "Gtk" "ScrolledWindow")
        (gi-repo:load-by-name "Gtk" "TextBuffer")
        (gi-repo:load-by-name "Gtk" "TextTag")
        (gi-repo:load-by-name "Gtk" "TextIter")
        (gi-repo:load-by-name "Gtk" "TextView")
        (gi-repo:load-by-name "Gtk" "DeleteType")
        (gi-repo:load-by-name "Gtk" "Viewport")
        (gi-repo:load-by-name "Gtk" "Widget") ;; show, show-all, key-press-event
        (gi-repo:load-by-name "Gtk" "Window")
        (gi-repo:load-by-name "Gtk" "Box")
        (gi-repo:load-by-name "Gtk" "HBox")
        (gi-repo:load-by-name "Gtk" "VBox")

        (gi:make <GtkApplication> #:application-id "page.codeberg.schemacs")
        ))

    ;;----------------------------------------------------------------
    ))

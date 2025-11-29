(define-library (schemacs editor)
  ;; This is the base package for the Schemacs editor, which is a clone
  ;; of Emacs written in Scheme. The reference implementation is
  ;; written in Guile Scheme which comes with an Emacs Lisp compiler,
  ;; and therefore is not just a clone of the Emacs editor, but of the
  ;; Emacs Lisp programming language.

  ;; TODO: consider possibly doing the following:
  ;;
  ;;  1. changing this library to `(schemacs ui main)`
  ;;
  ;;  2. replacing `(schemacs editor-impl)` with `(schemacs ui
  ;;     text-buffer-impl)`, which means getting rid of all calls to
  ;;     the editor-impl APIs in `(schemacs editor)`.
  ;;

  (import
    (scheme base)
    (scheme lazy)
    (scheme write)
    (only (scheme read) read)
    (only (scheme case-lambda) case-lambda)
    (only (srfi 28) format)
    (only (schemacs hash-table)
          make-hash-table  default-hash
          )
    (only (schemacs lens)
          view record-unit-lens lens-set update =>hash-key!
          )
    (only (schemacs editor command)
          command-type? command-procedure
          new-command show-command apply-command
          )
    (only (schemacs eval) eval-string)
    (prefix (schemacs keymap) km:)
    (prefix (schemacs editor-impl) *impl/)
    (only (schemacs editor-impl)
          make<cell-factory>
          factory-make-cell
          factory-set!cell-value
          )
    (only (schemacs pretty)
          pretty make<pp-state>
          print line-break bracketed qstr
          pp-state-output-port
          pp-state-line-buffer
          pp-state-indent-char
          pp-state-indent
          pp-line-indent-char
          pp-line-indent
          pp-line-string
          display-lines
          )
    (only (schemacs ui rectangle)
          rect2D  point2D  size2D
          )
    (only (schemacs vbal) alist->vbal)
    (only (schemacs ui)
          run-div-monad  enclose  expand
          state-var  use-vars  =>state-var-value*!
          div  view-type  properties  =>div-properties*!
          div-pack  pack-elem  cut-horizontal  cut-vertical
          div-space   floater  print-div
          tiled-windows  text-editor
          use-vars-value ;;DEBUG
          ))

  (export
   ;; A quick note on naming: names delimited with asterisks,
   ;; e.g. *default-keymap* are parameter objects. Asterisk-delimited
   ;; names beginning with "impl/..." for example
   ;; *IMPL/NEW-WINFRAME-VIEW* are parameters that SHOULD be
   ;; parameterized by the user interface backend, but are typically
   ;; parameterized with procedures that do nothing by default.

   main  *the-editor-state*

   ;; ---------------- Buffers ----------------
   buffer-type?  buffer-cell
   =>buffer-handle  =>buffer-view  =>buffer-local-keymap
   *default-buffer-local-keymap*
   current-buffer
   selected-buffer

   ;; -------------- Mode Lines --------------
   ;; This includes the echo area
   line-display-type?
   new-line-display  new-mode-line  new-header-line  new-echo-area
   *mode-line-format*  *header-line-format*

   ;; ---------------- Windows ----------------
   window-type?
   window-cell  window-buffer  window-parent-frame
   =>window-buffer  =>window-view  =>window-local-keymap
   =>window-mode-line  =>window-header-line
   *default-window-local-keymap*
   selected-window  select-window

   ;; -------------- Minibuffers -------------
   new-minibuffer
   *default-keymap*

   ;; ---------------- Frames ----------------
   winframe-type? winframe-cell
   winframe-parent-editor
   =>winframe-layout  =>winframe-selected-window
   =>winframe-local-keymap  =>winframe-view
   =>winframe-echo-area  =>winframe-minibuffer  =>winframe-prompt
   *default-winframe-local-keymap*
   *default-minibuffer-keymap*
   selected-frame
   key-event-handler
   format-message
   display-in-echo-area  clear-echo-area
   simple-read-minibuffer  read-from-minibuffer
   exit-minibuffer
   focus-minibuffer
   clear-minibuffer
   eval-minibuffe
   minibuffer-prompt-resume
   exit-minibuffer-with-return

   ;; ---------------- Editor ----------------
   editor-type?  new-editor
   =>editor-buffer-table
   =>editor-winframe-table
   =>editor-proc-table
   =>editor-base-keymap
   =>editor-minibuffer
   =>editor-view
   editor-messages
   current-editor

   ;; ---------------- Front-end API ----------------
   new-self-insert-keymap  key-event-self-insert
   default-keymap
   print-exception-message
   self-insert-keymap  self-insert-command
   delete-char delete-backward-char
   *unbound-key-index*   unbound-key-index-handler
   *waiting-key-index*   waiting-key-index-handler
   *dispatch-key-event*  dispatch-key-event-handler
   insert  get-buffer  read-from-minibuffer
   eval-expression-string  eval-expression
   print-to-buffer
   )

  ;; =======================
  (begin

    (define (keymap-arg-or-default km-arg *default*)
      ;; This function is basically "or" except it throws an exception if
      ;; the first argument is not #f or a KM:<KEYMAP-TYPE>, and it
      ;; evaluates the second argument (expecting it to be a parameter or
      ;; lambda of zero arguments) if the first argument is #f. The type
      ;; checking and lazy evaluation mechanism on the "*default*"
      ;; argument provided here is pretty useful, especially since this
      ;; function is not used in performance-critical procedures.
      (cond
       ((km:keymap-type? km-arg) km-arg)
       ((not km-arg)
        (cond
         ((km:keymap-type? *default*) *default*)
         ((procedure? *default*) (*default*))
         (else (error "argument not a <keymap-type>" *default*))
         ))
       (else (error "argument not a <keymap-type>" km-arg))))


    ;; -------------------------------------------------------------------------------------------------
    ;; Keymaps defined prior to launching the programming editor GUI.

    (define key-event-self-insert
      (case-lambda
        ((uarg)
         (let*((winframe   (selected-frame))
               (state      (winframe-modal-state winframe))
               (key        (km:modal-lookup-state-key-index state))
               (on-success (lambda (c) c))
               (on-fail    (lambda () #f))
               (c (if (not key) #f
                      (km:keymap-index-to-char key #f on-success on-fail)))
               )
           (key-event-self-insert uarg c)))
        ((uarg c)
         (let*((winframe (selected-frame))
               (window   (winframe-selected-window winframe))
               (cmd      (*impl/self-insert-command*))
               )
           (cond
            ((not cmd) #f)
            ((not c) #f)
            ((procedure?    cmd) (cmd window c) #t)
            ((command-type? cmd) ((command-procedure cmd) window c) #t)
            (else (error "*self-insert-command* does not contain a procedure" cmd))
            )))))

    (define self-insert-command
      ;; This is an actual command (not just a procedure) that calls the
      ;; procedured stored in the *impl/self-insert-command* parameter
      ;; which must be overloaded by the low-level implementation of the
      ;; editor.
      ;;
      ;; TODO: All commands should only take the window as an argument.
      ;; Instead of taking "key" as an argument, it should be stored
      ;; into the window record data and retrieved so that it can be
      ;; handed off to the procedure in the *self-insert-command*
      ;; parameter.
      ;;------------------------------------------------------------------
      (new-command
       "self-insert-command"
       (lambda () (key-event-self-insert #f))
       key-event-self-insert
       "Takes a key event and converts it to a character based on the keyboard
 key that was pressed, then inserts that character into the current
 buffer of the current window."
       ))


    (define self-insert-layer
      ;; This is the `SELF-INSERT-COMMAND` wrapped in a
      ;; `KM:<KEYMAP-INDEX-PREDICATE-TYPE>` layer. When defining keymaps,
      ;; you can use use this as a layer to catch all key events that
      ;; translate to self-inserting characters. Control characters are
      ;; not considered self-inserting by this layer.
      ;;------------------------------------------------------------------
      (km:new-self-insert-keymap-layer #f (lambda (c) self-insert-command) (lambda () #f)))


    (define (minibuffer-prompt-resume)
      (let*((winframe (selected-frame))
            (txt (get-minibuffer-text winframe))
            )
        (display ";;minibuffer-prompt-resume ")(write txt)(newline) ;;DEBUG
        (winframe-prompt-resume winframe txt)))

    ;; -------------------------------------------------------------------------------------------------

    ;; (define init-cell-actor
    ;;   ;; Use this function to parameterize cell-factory* so that objects
    ;;   ;; initialized by this module can construct Goblins cell actors and
    ;;   ;; keep a reference to these cells within their record data types.
    ;;   (make<cell-factory>
    ;;    (case-lambda
    ;;      (() (spawn ^cell))
    ;;      ((init-value) (spawn ^cell init-value)))
    ;;    $))

    (define make<cell>
      ;; This function is not exported, it is used internally to
      ;; initialize cells for the various objects initialized by the
      ;; various "new-___" object constructor functions.
      (case-lambda
        (()
         (let ((factory (*impl/cell-factory*)))
           ((factory-make-cell factory))))
        ((init-value)
         (let ((factory (*impl/cell-factory*)))
           ((factory-make-cell factory) init-value)))))

    (define (set!cell-value cell value)
      ;; This function is not exported, it is used internally to update
      ;; cells for the various "new-___" object constructors functions.
      (let ((factory (*impl/cell-factory*)))
        ((factory-set!cell-value factory) cell value)))

    ;; -------------------------------------------------------------------------------------------------

    (define-record-type <buffer-type>
      (make<buffer> cell keymap handle view)
      buffer-type?
      (cell    buffer-cell)
      (keymap  buffer-local-keymap  set!buffer-local-keymap)
      (handle  buffer-handle        set!buffer-handle)
      (view    buffer-view          set!buffer-view)
      ;; NOTE: the view ^ here, in Gtk, is a GtkTextBuffer, not a
      ;; GtkTextView. A GtkTextView is functionally equivalent to an
      ;; Emacs "window".
      )

    (define =>buffer-handle
      (record-unit-lens buffer-handle set!buffer-handle '=>buffer-handle))

    (define =>buffer-local-keymap
      (record-unit-lens buffer-local-keymap set!buffer-local-keymap '=>buffer-local-keymap))

    (define =>buffer-view
      (record-unit-lens buffer-view set!buffer-view '=>buffer-view))

    (define (insert string-or-char)
      (let ((buffer (window-buffer (selected-window))))
        ((*impl/insert-into-buffer*) buffer string-or-char)
        ))

    (define delete-char
      (new-command
       "delete-char"
       (lambda () (apply-command delete-char 1 #f))
       (lambda args (apply (*impl/delete-char*) args))
       "Takes 2 arguments, an integer number of characters to delete after
    the cursor (negative integers delete before the cursor), followed by a
    boolean indicating whether the deleted characters should be copied to
    the \"kill ring\"."
       ))

    (define delete-backward-char
      (new-command
       "delete-backward-char"
       (lambda () (apply-command delete-char -1 #f))
       (lambda args (apply (*impl/delete-char*) args))
       "Delete the previous N characters (following if N is negative).
    If Transient Mark mode is enabled, the mark is active, and N is 1,
    delete the text in the region and deactivate the mark instead.
    To disable this, set option 'delete-active-region' to nil."
       ))


    (define new-buffer
      ;; Construct a `<BUFFER-TYPE>`, calling `*IMPL/NEW-BUFFER-VIEW*` to
      ;; construct the actual text buffer. In Gtk back-ends, the
      ;; GtkTextBuffer object is constructed and stored in the
      ;; <BUFFER-TYPE> that is created and by this procedure.
      ;; ------------------------------------------------------------------
      (case-lambda
        ((handle) (new-buffer handle #f))
        ((handle keymap)
         (display ";;;----------- new-buffer ------------------;;;\n");;DEBUG
         (let*((keymap (or keymap (*default-buffer-local-keymap*)))
               (this (make<buffer> #f keymap handle #f))
               (view ((*impl/new-buffer-view*) this))
               )
           (set!buffer-view this view)
           this
           ))))

    ;; -------------------------------------------------------------------------------------------------

    (define-record-type <line-display-type>
      ;; This <line-display-type> is used to construct things that are
      ;; displayed in a single line in the user interface. This includes
      ;; things like the echo area, the minibuffer, the mode line, and the
      ;; header line. In the Gtk backend, these are all GtkFlowBox widgets
      ;; containing one or more GtkLabel widgets.
      (make<line-display-type> parent prompt-var input-var view)
      line-display-type?
      (parent      line-display-parent-frame  set!line-display-parent-frame)
      (prompt-var  line-display-prompt-var    set!line-display-prompt-var)
      (input-var   line-display-input-var     set!line-display-input-var)
      (view        line-display-view          set!line-display-view)
      )

    (define (new-line-display parent prompt-str)
      (display ";;;----------- new-line-display ------------------;;;\n");;DEBUG
      (let*((prompt-var (and prompt-str (state-var string=? prompt-str)))
            (input-var (state-var string=? ""))
            (this (make<line-display-type> parent prompt-var input-var #f))
            (widget
             (div-pack
              cut-vertical
              (view-type this)
              (properties 'wrapping: #t)
              (size2D expand enclose)
              (and prompt-var (pack-elem enclose (use-vars (list prompt-var) div)))
              (pack-elem expand (use-vars (list input-var) text-editor))
              )))
        (set!line-display-view this widget)
        this
        ))

    (define (new-echo-area  parent) (new-line-display parent #f))
    (define (new-minibuffer parent) (new-line-display parent ""))

    (define (new-header-line parent-window st)
      (new-mode-line parent-window (or st *header-line-format*))
      )

    (define (new-mode-line parent-window st)
      ;; A mode line is just a `use-vars` node that constructs a `div`
      ;; node containing a variety of indicators for displaying
      ;; informaion about the current window and the buffer it is
      ;; displaying.
      (display ";;;----------- new-mode-line ---------------;;;\n");;DEBUG
      (let ((st (or st (*mode-line-format*))))
        (use-vars
         (list st)
         (lambda (items) 
           (let ((items
                  (and items
                       (map
                        (lambda (item)
                          (cond
                           ((procedure? item) (item parent-window))
                           (else item)
                           ))
                        items
                        ))))
             (cond ;;DEBUG
              (items ;;DEBUG
               (display "; use-vars mode-line") (newline) (display-lines items) (newline) ;;DEBUG
               ) ;;DEBUG
              (else (display "; use-vars mode-line #f\n"));;DEBUG
              ) ;;DEBUG
             ((mode-line-display-items parent-window) items)
             )))))

    (define *header-line-format* (make-parameter (state-var equal? #f)))

    (define *mode-line-format*
      (make-parameter
       (state-var
        equal?
        (list
         (lambda (_) (if (*impl/is-graphical-display?*) " " "-"))
         (lambda (_) "-") ;; buffer encoding
         (lambda (_) ":") ;; end of line style
         (lambda (_) "-") ;; buffer file is writable ("%" in read-only mode)
         (lambda (parent-window) ;; "%" in read-only mode and unchanged
           (if (is-buffer-changed? parent-window) "*" "-")
           )
         (lambda (_) "-") ;; buffer is local or remote ("@" means remote)
         "  "
         (lambda (parent-window)
           (list
            'propertize:
            (view parent-window =>window-buffer =>buffer-handle)
            '(weight: . "bold")
            ))))))

    (define (mode-line-display-single parent-window stack item)
      (define (propertize elems props)
        (let*((elems (mode-line-collect-items parent-window '() elems))
              )
          (if props
              (apply
               div-pack cut-vertical
               (and (pair? props) (apply properties props))
               elems
               )
              #f
              )))
      (cond
       ((or (eq? #t item) (eq? #f item)) stack)
       ((procedure? item)
        (let ((next-item (item parent-window)))
          (mode-line-display-single parent-window stack next-item)
          ))
       ((eq? 'separator item) (cons 'separator stack))
       ((pair? item)
        (let ((head (car item))
              (tail (cdr item))
              )
          (cond
           ((or (eq? 'propertize: head) (eq? ':propertize head))
            (cond
             ((not (null? tail))
              (cons (propertize (car tail) (cdr tail)) stack)
              )
             (else #f)
             ))
           (else (mode-line-collect-items parent-window stack item))
           )))
       ((string? item) (cons (div item) stack))
       (else
        (error "unknown mode-line item" item)
        )))

    (define (mode-line-collect-items parent-window stack items)
      (let loop ((stack stack) (items items))
        (cond
         ((pair? items)
          (loop
           (mode-line-display-single parent-window stack (car items))
           (cdr items)
           ))
         (else
          (reverse stack))
         )))

    (define (mode-line-display-items parent-window)
      ;; Loop over the `ITEMS` (or `*MODE-LINE-FORMAT*` if no `ITEMS` are
      ;; given, or if `ITEMS` is #f) and collect strings, collect all
      ;; strings into a list, call the back-end function
      ;; `*IMPL/MODE-LINE-DISPLAY-ITEMS*`.
      ;;------------------------------------------------------------------
      (lambda (items)
        (and items
             (apply
              div-pack cut-vertical
              (mode-line-collect-items parent-window '() items)
              ))))

    ;; -------------------------------------------------------------------------------------------------

    (define-record-type <window-type>
      ;; This is a wrapper around the Gtk back-end data structures that
      ;; behave like an Emacs text window.
      ;; 
      ;; In Emacs nomencalture a "window" is a view for a
      ;; buffer. Therefore in Gtk nomenclature, a GtkTextView widget is
      ;; the nearest thing to what Emacs considers to be a window. You may
      ;; provide a buffer as an argument, or #f. If you do not
      ;; provide a buffer, an new empty buffer is created.
      (make<window> parent buffer keymap mode-line header-line view)
      window-type?
      (parent       window-parent-frame  set!window-parent-frame)
      (buffer       %window-buffer       set!window-buffer)
      (keymap       window-local-keymap  set!window-local-keymap)
      (mode-line    window-mode-line     set!window-mode-line)
      (header-line  window-header-line   set!window-header-line)
      (view         window-view          set!window-view)
      )

    (define (new-window parent buffer keymap)
      ;; Construct a new ordinary window -- ordinary, that is, as opposed
      ;; to the window constructed specifically to contain the minibuffer.
      ;;------------------------------------------------------------------
      (display ";;;----------- new-window ------------------;;;\n");;DEBUG
      (let*((this (make<window> parent buffer keymap #f #f #f))
            (mode-line (new-mode-line this (*mode-line-format*)))
            (header-line (new-header-line this (*header-line-format*)))
            (widget
             (div-pack
              cut-horizontal (view-type this)
              header-line
              (view buffer =>buffer-view)
              mode-line
              )))
        (set!window-mode-line   this mode-line)
        (set!window-header-line this header-line)
        (set!window-view        this widget)
        (display ";;;----------- window constructed --------------;;;\n");;DEBUG
        this
        ))

    (define window-buffer
      (case-lambda
        (() (window-buffer (selected-window)))
        ((window) (%window-buffer window))
        ))

    (define *default-window-local-keymap*
      ;; This keymap maps self-insert-key to all printable ASCII characters.
      (make-parameter #f)
      )

    (define =>window-local-keymap
      (record-unit-lens window-local-keymap set!window-local-keymap '=>window-local-keymap)
      )

    (define =>window-buffer
      (record-unit-lens %window-buffer set!window-buffer '=>window-buffer)
      )

    (define =>window-mode-line
      (record-unit-lens
       window-mode-line
       set!window-mode-line
       '=>window-mode-line
       ))

    (define =>window-header-line
      (record-unit-lens
       window-header-line
       set!window-header-line
       '=>window-header-line
       ))

    (define =>window-view
      (record-unit-lens window-view set!window-view '=>window-view)
      )

    (define (debug-print-keymaps winframe) ;;DEBUG
      (let*((window   (winframe-selected-window  winframe))
            (buffer   (window-buffer             window))
            (winkmap  (window-local-keymap       window))
            (bufkmap  (if (buffer-type?  buffer) (buffer-local-keymap  buffer) #f))
            (frmkmap  (if (winframe-type? winframe) (winframe-local-keymap winframe) #f))
            (awaiting (winframe-awaiting-prompt? winframe))
            (print-keymap-name
             (lambda (field keymap)
               (print "#:" field #\space
                      (or (and (not keymap) "#f")
                          (view keymap km:=>keymap-label!)
                          "<no-label>"
                          )))))
        (pretty
         (print
          (bracketed 2 #\( #\)
                     "#:awaiting " awaiting #\space
                     (print-keymap-name "bufkmap" bufkmap) #\space
                     (print-keymap-name "winkmap" winkmap) #\space
                     (print-keymap-name "frmkmap" frmkmap)
                     )
          (line-break)
          ))
        #t))

    (define (window-get-or-reset-modal-state! winframe)
      ;; Prepare for a new key event. Get the current modal-lookup-state
      ;; for the window, if it does not exist, try to create a new
      ;; modal-lookup-state by gathering all of the keymaps from the
      ;; current buffer, the window, and the parent frame. If there are no
      ;; keymaps at all, return false. If a new modal-lookup-state was
      ;; created, the windows current modal state is reset to this new
      ;; state value. The window's state value or #f is returned.
      (cond
       ((winframe-type? winframe)
        (let ((state (winframe-modal-state winframe)))
          (cond
           ((km:modal-lookup-state-type? state) state)
           (else
            (let*((window   (winframe-selected-window  winframe))
                  (buffer   (window-buffer             window))
                  (frmkmap  (winframe-local-keymap     winframe))
                  (winkmap  (window-local-keymap       window))
                  (bufkmap  (buffer-local-keymap       buffer))
                  (init-km  (km:keymap bufkmap winkmap frmkmap))
                  (state (and (not (null? init-km)) (km:new-modal-lookup-state init-km)))
                  )
              (unless (not state)
                (set!winframe-modal-state winframe state)
                )
              state
              )))))
       (else
        (error "not <winframe-type> data" winframe)
        )))

    (define (unbound-key-index-handler key-index)
      (format-message
       "~s is undefined\n"
       (km:keymap-index->list key-index)
       ))

    (define *unbound-key-index*
      ;; This event handlers is parameterized should it ever become
      ;; necessary to override it, but usually the default procedure
      ;; UNBOUND-KEY-INDEX-HANDLER does the job well enough.
      (make-parameter unbound-key-index-handler))

    (define (waiting-key-index-handler key-path keymap)
      (format-message "~s-"
                      (km:keymap-index->list (force key-path))))

    (define *waiting-key-index*
      ;; This event handlers is parameterized should it ever become
      ;; necessary to override it, but usually the default procedure
      ;; WAITING-KEY-INDEX-HANDLER does the job well enough.
      (make-parameter waiting-key-index-handler))

    (define (dispatch-key-event-handler key-path action)
      ;; This is the default action for the `*DISPATCH-KEY-EVENT*`
      ;; parameter which is called to actually execute a command once it
      ;; that command has been looked up in a keymap in response to a key
      ;; event. It receives the `<KEYMAP-INDEX-TYPE>`, and a
      ;; `<COMMAND-TYPE>` or procedure that was associated with the keymap
      ;; index.
      ;;
      ;; This procedure is also responsible for setting up the
      ;; continuation that is called by `READ-FROM-MINIBUFFER` to return
      ;; control to the input event loop after prompting the end user by
      ;; showing the minibuffer.
      ;;------------------------------------------------------------------
      (let ((apply-action
             (cond
              ((command-type? action) (lambda () ((command-procedure action)) #f))
              ((procedure?    action) (lambda () (action) #f))
              (else
               (error "cannot dispatch key event to non command type"
                      (force key-path) action))))
            (winframe (selected-frame))
            )
        (cond
         (winframe
          (call/cc
           (lambda (cont)
             (parameterize
                 (((winframe-event-dispatch winframe) cont))
               (apply-action)))))
         (else
          (apply-action)))))

    (define *dispatch-key-event*
      ;; This event handlers is parameterized should it ever become
      ;; necessary to override it, but usually the default procedure
      ;; DISPATCH-KEY-EVENT-HANDLER does the job well enough.
      (make-parameter dispatch-key-event-handler))

    ;; -------------------------------------------------------------------------------------------------

    (define-record-type <winframe-type>
      (make<winframe>
       editor window layout modal keymap
       echo minibuf dispatch prompt view
       )
      winframe-type?
      (editor   winframe-parent-editor)
      (window   winframe-selected-window set!winframe-selected-window)
      (layout   winframe-layout          set!winframe-layout)
      (modal    winframe-modal-state     set!winframe-modal-state)
      (keymap   winframe-local-keymap    set!winframe-local-keymap)
      (echo     winframe-echo-area       set!winframe-echo-area)
      (minibuf  winframe-minibuffer      set!winframe-minibuffer)
      (dispatch winframe-event-dispatch  set!winframe-event-dispatch)  
      (prompt   winframe-prompt-stack    set!winframe-prompt-stack)
      (view     winframe-view            set!winframe-view)
      ;; Interesting note: it was here where I discovered that Guile does
      ;; NOT allow record types or field "<frame-type>" or have predicates
      ;; called "frame-type?". When I did try the "frame-type?" predicate,
      ;; I got some completely weird error:
      ;;
      ;;    Wrong type to apply: #<syntax-transformer frame-type?>
      ;;
      ;; This is definitely a bug and should be reported.
      )

    (define (new-frame editor init-buffer init-keymap)
      ;; "keymap" is the keymap for this local frame, which is a fallback
      ;; keymap used when no key matches the buffer local keymap.
      (display ";;;------------ new-frame ------------------;;;\n");;DEBUG
      (let*((editor      (or editor (*the-editor-state*)))
            (init-buffer (or init-buffer (editor-messages editor)))
            (init-keymap
             (keymap-arg-or-default
              init-keymap
              *default-winframe-local-keymap*
              ))
            (init-window     #f)
            (layout          #f)
            (modal-key-state #f)
            (echo-area       #f)
            (minibuffer      #f)
            (dispatch        (make-parameter #f))
            (prompt          '())
            (widget          #f)
            (this
             (make<winframe>
              editor  init-window  layout  modal-key-state
              init-keymap  echo-area  minibuffer  dispatch
              prompt  widget
              ))
            (init-window (new-window this init-buffer init-keymap))
            (echo-area   (new-echo-area this))
            (minibuffer  (new-minibuffer this))
            (layout      (state-var init-window))
            (lo-size     (size2D expand enclose))
            (widget
             (floater
              (rect2D 0 0 expand expand)
              (div-pack
               cut-horizontal
               (view-type this)
               (pack-elem
                (size2D expand expand)
                (use-vars (list layout) window-view)
                )
               (pack-elem lo-size (line-display-view echo-area))
               (pack-elem lo-size (line-display-view minibuffer))
               ))))
        (set!winframe-selected-window this init-window)
        (set!winframe-layout          this layout)
        (set!winframe-echo-area       this echo-area)
        (set!winframe-minibuffer      this minibuffer)
        (set!winframe-view            this widget)
        (display ";;;---------- frame constructed --------------;;;\n");;DEBUG
        this
        ))

    (define *default-winframe-local-keymap*
      ;; This keymap maps self-insert-key to all printable ASCII characters.
      (make-parameter #f)
      )

    (define =>winframe-layout
      (record-unit-lens
       winframe-layout
       set!winframe-layout
       '=>winframe-layout
       ))

    (define =>winframe-keymap
      (record-unit-lens
       winframe-local-keymap
       set!winframe-local-keymap
       '=>winframe-local-keymap
       ))

    (define =>winframe-selected-window
      ;; This field should contain a `DIV` from the `(schemacs ui)`
      ;; library, in particular a `tiled-window` type of `DIV`.
      ;;------------------------------------------------------------------
      (record-unit-lens
       winframe-selected-window
       set!winframe-selected-window
       '=>winframe-selected-window
       ))

    (define =>winframe-modal-state
      (record-unit-lens
       winframe-modal-state
       set!winframe-modal-state
       '=>winframe-modal-state
       ))

    (define =>winframe-view
      (record-unit-lens
       winframe-view
       set!winframe-view
       '=>winframe-view
       ))

    (define =>winframe-minibuffer
      (record-unit-lens
       winframe-minibuffer
       set!winframe-minibuffer
       '=>winframe-minibuffer
       ))

    (define =>winframe-echo-area
      (record-unit-lens
       winframe-echo-area
       set!winframe-echo-area
       '=>winframe-echo-area
       ))

    (define =>winframe-event-dispatch
      ;; Keeps a continuation constructed at the point of event dispatch
      ;; so that when it comes time to prompt the end user (e.g. using
      ;; `SIMPLE-READ-MINIBUFFER`) the GUI can be set into modal prompt
      ;; mode and return immediately to this event dispatch, but not
      ;; before saving the continuation at which the prompt was requested
      ;; into the `WINFRAME-PROMPT-STACK` so that the continuation can be
      ;; resumed after the prompt input has been collected.
      (record-unit-lens
       winframe-event-dispatch
       set!winframe-event-dispatch
       '=>winframe-event-dispatch
       ))

    (define =>winframe-prompt-stack
      ;; Keeps a continuation at which the end user has been prompted for
      ;; input, as what happens with the `SIMPLE-READ-MINIBUFFER`
      ;; API. When the end user presses the return key, or cancels, the
      ;; continuation at the top of this stack is resumed with the result
      ;; received from the end user.
      (record-unit-lens
       winframe-prompt-stack
       set!winframe-prompt-stack
       '=>winframe-prompt-stack
       ))

    (define select-window
      (case-lambda
        ((window) (select-window window #f))
        ((window norecord)
         (display ";;select-window ")(write (view window =>window-buffer =>buffer-handle))(newline) ;;DEBUG
         ((*impl/select-window*) window)
         (lens-set window (selected-frame) =>winframe-selected-window)
         )))

    ;; -------------------------------------------------------------------------------------------------

    (define-record-type <minibuffer-prompt-type>
      (make<minibuffer-prompt> continuation selected-window)
      minibuffer-prompt-type?
      (continuation    minibuffer-prompt-continuation)
      (selected-window minibuffer-prompt-selected-window)
      )

    (define (winframe-awaiting-prompt? winframe)
      ;; This procedure returns #t if `WINFRAME-AWAIT-PROMPT` has been
      ;; called once before. It determines this by checking if there is at
      ;; least one continuation on the `=>WINFRAME-PROMPT-STACK`, which is
      ;; a stack that is pushed every time `WINFRAME-AWAIT-PROMPT` is
      ;; called, and is popped every time `WINFRAME-PROMPT-RESUME` is
      ;; called.
      ;;------------------------------------------------------------------
      (pair? (view winframe =>winframe-prompt-stack))
      )

    (define (winframe-await-prompt winframe window)
      ;; This procedure sets creates a continuation containing the entire
      ;; procedure call stack of the computation up to the point at which
      ;; this `WINFRAME-AWAIT-PROMPT` procedure was called. This
      ;; continuation is stored into `WINFRAME-CALL-STACK` along with the
      ;; `WINDOW` argument that was applied to this procedure. Then, the
      ;; `WINFRAME-EVENT-DISPATCH` continuation is resumed. To resume the
      ;; computation and have this procedure call return some value, pass
      ;; the return value to the `WINFRAME-PROMPT-RESUME` procedure. When
      ;; the `WINFRAME-PROMPT-RESUME` procedure is applied, the `WINDOW`
      ;; that was passed to this function is focused with `SELECT-WINDOW`
      ;; so that that window is selected to be used by whatever procedure
      ;; invocation might result from the prompt returning a string.
      ;;
      ;; Therefore when this procedure is called, it does not return
      ;; immediately, rather program control is handed off to the
      ;; `WINFRAME-EVENT-DISPATCH` continuation and the GUI event loop
      ;; resumes without this procedure having ever returned, but the
      ;; computation up to this procedure is saved as a continuation. If
      ;; all goes well, this procedure will return when some GUI event
      ;; triggers a call to the `WINFRAME-PROMPT-RESUME` procedure by
      ;; applying to it a `RETURN` argument. That `RETURN` argument
      ;; becomes the return value of this procedure call when the
      ;; continuation constructed by this `WINFRAME-AWAIT-PROMPT`
      ;; procedure is resumed.
      ;;------------------------------------------------------------------
      (cond
       ((window-type? window)
        (let*((dispatch ((view winframe =>winframe-event-dispatch))))
          (display ";;winframe-await-prompt\n")
          (cond
           (dispatch
            (call/cc
             (lambda (cont)
               (update
                (lambda (stack)
                  (cons ;; here is where we push the continuation onto the stack
                   (make<minibuffer-prompt> cont window)
                   stack))
                winframe =>winframe-prompt-stack)
               (dispatch)               ; this does not return
               )))
           (else (error "recursive edit failed, no event dispatch continuation"))
           )))
       (else (error "second argument must be a valid <WINDOW-TYPE>" window))
       ))

    (define (winframe-prompt-resume winframe return)
      ;; If a call to `WINFRAME-AWAIT-PROMPT` has ever been made in
      ;; response to some GUI event, the procedure which called
      ;; `WINFRAME-AWAIT-PROMPT` has been frozen as a continuation and
      ;; stored on the `WINFRAME-PROMPT-STACK`, waiting to resume the
      ;; computation as though `WINFRAME-AWAIT-PROMPT` finally returned a
      ;; value. To resume that computation, call this procedure, and
      ;; provide a return value as an argument to be returned by the call
      ;; to `WINFRAME-AWAIT-PROMPT` when that computation was frozen.
      ;;
      ;; When `WINFRAME-PROMPT-STACK` was called it must have received a
      ;; `WINDOW` argument, which was the window that was in focus before
      ;; the minibuffer was focused, and that same window will be foused
      ;; again after this procedure is applied.
      ;;------------------------------------------------------------------
      (let*((prompt-stack (view winframe =>winframe-prompt-stack))
            (prompt-item  (and (not (null? prompt-stack)) (car prompt-stack)))
            )
        (display ";;winframe-prompt-resume ")(write return)(newline) ;;DEBUG
        (debug-print-keymaps winframe) ;;DEBUG
        (cond
         ((minibuffer-prompt-type? prompt-item)
          (let ((prompt  (minibuffer-prompt-continuation     prompt-item))
                (window  (minibuffer-prompt-selected-window  prompt-item))
                )
            (select-window window)
            (update
             (lambda (stack) (cdr prompt-stack))
             winframe =>winframe-prompt-stack)
            (prompt return)
            ))
         (else (error "no recursive edit prompt on the stack" prompt-stack))
         )))

    ;; -------------------------------------------------------------------------------------------------

    (define (selected-frame) (*impl/selected-frame*))
    (define (selected-window) (view (selected-frame) =>winframe-selected-window))
    (define (current-buffer) (*impl/current-buffer*))
    (define selected-buffer current-buffer)
    ;; These are global variables from the point of view of Emacs
    ;; Lisp. They are parameters in this library, and are parameterized
    ;; by the GUI provider library. These APIs are exposed in the
    ;; (SCHEMACS EDITOR) library, but as procedures which cannot modify
    ;; the parameter.

    ;; -------------------------------------------------------------------------------------------------

    (define-record-type <editor-type>
      ;; The editor state.
      (make<editor>
       cell buftab frametab proctab keymap messages counter view)
      editor-type?
      (cell      editor-cell)
      (buftab    editor-buffer-table    set!editor-buffer-table)
      (frametab  editor-winframe-table  set!editor-winframe-table)
      (proctab   editor-proc-table      set!editor-proc-table)
      (keymap    editor-base-keymap     set!editor-base-keymap)
      (messages  editor-messages        set!editor-messages)
      (counter   editor-obj-counter     set!editor-obj-counter)
      (view      editor-view            set!editor-view))

    (define =>editor-buffer-table
      (record-unit-lens editor-buffer-table set!editor-buffer-table '=>editor-buffer-table)
      )

    (define =>editor-winframe-table
      (record-unit-lens editor-winframe-table set!editor-winframe-table '=>editor-winframe-table)
      )

    (define =>editor-proc-table
      (record-unit-lens editor-proc-table set!editor-proc-table '=>editor-proc-table)
      )

    (define =>editor-base-keymap
      (record-unit-lens editor-base-keymap set!editor-base-keymap '=>editor-base-keymap)
      )

    (define =>editor-obj-counter
      (record-unit-lens editor-obj-counter set!editor-obj-counter '=>editor-obj-counter)
      )

    (define =>editor-view
      (record-unit-lens editor-view set!editor-view '=>editor-view)
      )

    (define (new-table size)
      ;; TODO: make use of the `SIZE` parameter
      (make-hash-table equal? default-hash)
      )

    (define (editor-get-next-obj-id editor)
      (let ((i (editor-obj-counter editor)))
        (set!editor-obj-counter editor (+ 1 i))
        i))

    (define (current-editor) (*impl/current-editor-closure*))

    (define (new-editor)
      ;; Construct a new text editor state.
      ;;
      ;; TODO: consider removing the <editor-type> entirely, and simply
      ;; using the Scheme environment object constructed by this (schemacs
      ;; editor) library as the editor state itself, with all fields of
      ;; <editor-type> redefined as parameters in this environment.
      (display ";;;----------- new-editor ------------------;;;\n");;DEBUG
      (let*((msgs           (new-buffer "*Messages*"))
            (buffer-table   (new-table 63))
            (winframe-table (new-table 15))
            (this
             (make<editor>
              #f                        ; editor-cell
              buffer-table
              winframe-table
              (new-table 15)            ; process table
              (*default-keymap*)        ; base-keymap
              msgs                      ; messages buffer
              0                         ; counter
              #f                        ; view
              ))
            (init-frame     (new-frame this msgs #f))
            (widget
             (div-space
              (view init-frame =>winframe-view)
              )))
        (lens-set msgs       this =>editor-buffer-table   (=>hash-key! (buffer-handle msgs)))
        (lens-set init-frame this =>editor-winframe-table (=>hash-key! "main"))
        (lens-set widget     this =>editor-view)
        (*impl/current-editor-closure* this)
        (display ";;;----------- editor consturcted --------------;;;\n");;DEBUG
        this
        ))

    (define get-buffer
      (case-lambda
        ((name editor-state)
         (cond
          ((string? name) (view editor-state =>editor-buffer-table (=>hash-key! name)))
          ((buffer-type? name) name)
          (else (error "not a string or buffer-type" name))))
        ((name) (get-buffer name (*impl/current-editor-closure*)))))

    (define (key-event-handler winframe key-path)
      ;; Begin a stateful lookup of the key path in all of the keymaps in
      ;; the context of the given frame, including the keymap for the
      ;; currently focused buffer, the window-local keymap, and the
      ;; winframe-local keymap for the parent frame.
      ;;
      ;; This function may call whatever callback has been set in the
      ;; *unbound-key-index*, the *waiting-key-index*, or the
      ;; *dispatch-key-event* function handlers.
      (cond
       ((winframe-type? winframe)
        (debug-print-keymaps winframe) ;;DEBUG
        (let*((state (window-get-or-reset-modal-state! winframe))
              (window (selected-window))
              )
          (cond
           ((not state)
            ((*unbound-key-index*) (selected-window) key-path))
           ((km:modal-lookup-state-type? state)
            (let ((is-waiting
                   (km:modal-lookup-state-step!
                    state key-path
                    (lambda (key-path action) ((*dispatch-key-event*) key-path action))
                    (lambda (key-path keymap) ((*waiting-key-index*)  key-path keymap))
                    (lambda (key-path)        ((*unbound-key-index*)  key-path)))))
              (unless is-waiting (set!winframe-modal-state winframe #f))
              is-waiting))
           (else
            (error
             "unknown object in modal-state-keymap field of window"
             state window)))))
       (else
        (error "key event handler must be provided a window frame" winframe))))

    (define (clear-echo-area winframe)
      ((*impl/clear-echo-area*) winframe))

    (define (display-in-echo-area winframe str)
      (let*((editor (winframe-parent-editor winframe))
            (msgbuf (editor-messages editor))
            )
        (clear-echo-area winframe)
        ((*impl/display-in-echo-area*) winframe str)
        ((*impl/insert-into-buffer*) msgbuf str)
        ))

    (define (format-message msgstr . objlist)
      (let*((winframe (selected-frame))
            (str (apply format (cons msgstr objlist)))
            )
        (display-in-echo-area winframe str)))

    (define (is-buffer-changed? window)
      ;; Takes a window, calls *IMPL/IS-BUFFER-modified?* on the buffer
      ;; that the window is currently displaying
      ;; ------------------------------------------------------------------
      ((*impl/is-buffer-modified?*) (view window =>window-buffer)))

    ;; TODO: when constructing the editor, frames, windows, and buffers,
    ;; make the construction happen in two passes. First, the record data
    ;; types are all constructed. Then a second function is called,
    ;; something like "init-views", which then evaluates the view
    ;; *impl/...* function for the editor and all of its sub fields. The
    ;; idea is that the whole editor data structure should exist in its
    ;; entirity as early as possible, as a kind of "superstructure", and
    ;; then the GUI is constructed on top of this superstructure.

    (define (get-minibuffer-text frame)
      (display ";;get-minibuffer-text\n") ;;DEBUG
      ((*impl/get-minibuffer-text*) frame))

    (define (exit-minibuffer-with-return return)
      ;; This procedure (or the `EXIT-MINIBUFFER` command) should be bound
      ;; to the `<RETURN>` key for a minibuffer. If there has been a call
      ;; to `WINFRAME-AWAIT-PROMPT`, and therfore exists a continuation
      ;; saved which contains the stack of the procedure that called it,
      ;; this function resumes that continuation with the return value to
      ;; that procedure call given as an argument to this procedure.
      (display ";;exit-minibuffer-with-return\n") ;;DEBUG
      (let ((winframe (selected-frame)))
        ((*impl/exit-minibuffer*) winframe)
        (winframe-prompt-resume winframe return)
        ))

    (define exit-minibuffer
      (new-command
       "exit-minibuffer"
       (lambda ()
         (exit-minibuffer-with-return
          (get-minibuffer-text (selected-frame))))
       exit-minibuffer-with-return
       "Terminate this minibuffer argument."))

    (define (focus-minibuffer winframe prompt init-input)
      ;; Move the keyboard focus to the minibuffer.
      (lens-set (winframe-minibuffer winframe) winframe =>winframe-selected-window)
      ((*impl/focus-minibuffer*) winframe prompt init-input))

    (define (clear-minibuffer window)
      ((*impl/clear-minibuffer*) window))


    (define simple-read-minibuffer
      ;; This is a better API to use than the usual Emacs
      ;; `READ-FROM-MINIBUFFER` API which is more procedural than
      ;; functional in style and also loaded with historical baggage.
      (case-lambda
        ((prompt init-input)
         (simple-read-minibuffer prompt init-input #f))
        ((prompt init-input keymap)
         (let*((window   (selected-window))
               (winframe (window-parent-frame window))
               ;;(minibuf  (winframe-minibuffer winframe))
               )
           (focus-minibuffer winframe prompt init-input)
           (winframe-await-prompt winframe window)
           ))
        ))

    (define read-from-minibuffer
      ;; This API mimics the Emacs API of the same name, but otherwise
      ;; simply calls the `SIMPLE-READ-MINIBUFFER` procedure. It sets the
      ;; UI into a mode in which the keyboard focus is on the minibuffer,
      ;; and the enter or return keys are bound to procedures which
      ;; collect that input and perform some action on it.
      ;;
      ;; TODO: for APIs like this, which wrap a simplified Scheme
      ;; procedure such as `SIMPLE-READ-MINIBUFFER` in an API that mimics
      ;; the Emacs API of the same name, I believe thse should be moved
      ;; out of this `(SCHEMACS EDITOR)` library and into their own "compat"
      ;; library. They are not useful to any other APIs here in this
      ;; library and are cluttering-up the code. APIs like this are more
      ;; useful to the Emacs Lisp evaluator, not to the core operation of
      ;; the editor.
      ;;------------------------------------------------------------------
      (case-lambda
        ((a) (read-from-minibuffer a #f #f #f #f #f #f))
        ((a b) (read-from-minibuffer a b #f #f #f #f #f))
        ((a b c) (read-from-minibuffer a b c #f #f #f #f))
        ((a b c d) (read-from-minibuffer a b c d #f #f #f))
        ((a b c d e) (read-from-minibuffer a b c d e #f #f))
        ((a b c d e f) (read-from-minibuffer a b c d e f #f))
        ((prompt init-input keymap read-lisp? history default inherit-im?)
         (let ((received-input (simple-read-minibuffer prompt init-input keymap)))
           (cond
            (read-lisp?
             (cond
              ((string=? received-input "") default)
              (else (read received-input))))
            (else received-input)
            ))
         )))

    (define eval-expression-string
      ;; This is the Scheme built-in implementation for the Emacs Lisp
      ;; `EVAL-EXPRESSION` API. You can pass an optional string to be read
      ;; and evaluated, with no arguments `SIMPLE-READ-MINIBUFFER` is used
      ;; to prompt the end user for an input string to evaluate.
      (case-lambda
        (() (eval-expression-string #f))
        ((input-string)
         (let*((winframe (selected-frame))
               (input-string
                (cond
                 ((string? input-string) input-string)
                 ((not input-string)
                  (simple-read-minibuffer "Eval: " #f (*default-minibuffer-keymap*))
                  )
                 (else (error "must be a string" input-string))))
               (_ (pretty
                   (print ";;eval-expression-string: "
                          (qstr input-string) (line-break))))
               (result
                (cond
                 ((string=? "" input-string) #f)
                 (else
                  (call/cc
                   (lambda (halt)
                     (with-exception-handler (lambda (caught) (halt caught))
                       (lambda ()
                         (call-with-values
                             (lambda () (eval-string input-string))
                           (lambda args args)))))))))
               (output-string
                (cond
                 (result
                  (call-with-port (open-output-string)
                    (lambda (port)
                      (write result port)
                      (flush-output-port port)
                      (get-output-string port))))
                 (else #f)))
               )
           (when output-string
             (display-in-echo-area winframe output-string))
           result))))

    (define eval-expression
      (new-command
       "eval-expression"
       (lambda () (eval-expression-string #f))
       eval-expression-string
       "Evaluate EXP and print resulting value in the echo area. When called
 interactively, read an Emacs Lisp expression in the minibuffer and
 evaluate it.  Value is also consed on to front of the variable
 ‘values’. Optional argument INSERT-VALUE non-nil (interactively, with
 a non ‘-’ prefix argument) means insert the result into the current
 buffer instead of printing it in the echo area."
       ))

    (define (command-error-default-function data context signal)
      ((*impl/command-error-default-function*) data context signal)
      )

    (define command-error-function
      (make-parameter command-error-default-function)
      )

    (define (buffer-line-break pp) ((pp-state-output-port pp) #\newline))

    (define %buffer-write-line
      (case-lambda
        ((pp char indent) (%buffer-write-line pp char indent #f))
        ((pp char indent string)
         (let ((insert (pp-state-output-port pp))
               (buffer (pp-state-line-buffer pp)))
           (when (>= indent 0)
             (insert buffer
                     (call-with-port
                         (lambda (port)
                           (let loop ((i 0))
                             (if (>= i indent) (values)
                                 (begin (write-char char port) (loop (+ 1 i)))))
                           (get-output-string port))
                       (open-output-string))))
           (when string (insert buffer string))
           ))))

    (define (buffer-write-line pp line)
      (%buffer-write-line pp
                          (pp-line-indent-char line)
                          (pp-line-indent line)
                          (pp-line-string line)
                          ))

    (define (buffer-first-indent pp)
      (%buffer-write-line pp
                          (pp-state-indent-char pp)
                          (pp-state-indent pp)
                          ))

    (define (buffer-print-finalize pp) (values))

    (define print-to-buffer
      ;; Construct a pretty-printer state structure, the record data
      ;; returned by this procedure can be used as the first argument to
      ;; `PRETTY` in the `(SCHEMACS PRETTY)` library. The "port" in this
      ;; structure is a lambda that writes characters or strings to the
      ;; buffer.
      ;;
      ;; Takes three arguments, all optional:
      ;;
      ;;    1. the buffer to which to write, defaults to result of
      ;;       `CURRENT-BUFFER` procedure
      ;;
      ;;    2. the number of character used for indentation. Defaults to two.
      ;;
      ;;    3. the character to use for indentation. Defaults to
      ;;       `#\space`.
      ;;
      ;; Any of the above arguments may be omitted, but arguments supplied
      ;; must be passed in the order specified above.
      ;;------------------------------------------------------------------
      (case-lambda
        (() (print-to-buffer (current-buffer) 2 #\space))
        ((a)
         (cond
          ((integer? a) (print-to-buffer (current-buffer) a #\space))
          ((char?    a) (print-to-buffer (current-buffer) 2 a))
          ((buffer-type? a) (print-to-buffer a 2 #\space))
          (else (error "value cannot be used to initialized print-to-buffer state" a))
          ))
        ((a b)
         (cond
          ((and (buffer-type? a) (integer? b)) (print-to-buffer a b #\space))
          ((and (buffer-type? a) (char? b)) (print-to-buffer a 2 b))
          ((and (integer? a) (char? b)) (print-to-buffer (current-buffer) a b))
          (else (error "value cannot be used to initialize print-to-buffer state" a b))
          ))
        ((buffer indent indent-char)
         (make<pp-state>
          buffer-line-break
          buffer-first-indent
          buffer-write-line
          buffer-print-finalize
          buffer                      ;;line-buffer
          (*impl/insert-into-buffer*) ;;output-port
          indent
          indent-char
          #f ;;start-of-line
          ))))

    ;; -------------------------------------------------------------------------------------------------

    (define default-keymap
      (km:keymap '*default-keymap*
                 (km:alist->keymap-layer
                  `(((#\backspace) . ,delete-backward-char)
                    ((#\delete)    . ,delete-char)
                    ((meta #\:)    . ,eval-expression)
                    ))
                 self-insert-layer
                 ))

    (define *default-keymap*
      ;; The keymap for all keys that are not pressed simultaneously with
      ;; modifier keys.
      (make-parameter default-keymap))


    (define *default-buffer-local-keymap*
      ;; This keymap maps self-insert-key to all printable ASCII characters.
      (make-parameter default-keymap)
      )

    (define *default-minibuffer-keymap*
      (make-parameter
       (km:keymap
        '*default-minibuffer-keymap*
        (km:alist->keymap-layer
         `(((#\return) . ,exit-minibuffer)
           ((#\newline) . ,exit-minibuffer)
           ))
        (*default-keymap*)
        )))

    ;;----------------------------------------------------------------
    ;; TODO: fix bug, the call to `EXIT-MINIBUFFER-WITH-RETURN` does
    ;; not restore the window correctly, somehow the minibuffer keymap
    ;; is still used to respond to key events after calling this
    ;; function.
    ;;----------------------------------------------------------------

    (define *the-editor-state* (make-parameter (new-editor)))

    (define (main . args)
      (editor-view (*the-editor-state*))
      )

    ;;----------------------------------------------------------------
    ))

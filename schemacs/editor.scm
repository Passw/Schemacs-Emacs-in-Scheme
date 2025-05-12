;; All of the record types defined in the module can be initialized by
;; a "new-<record-type-name>" function. All record types also have at
;; least two slots which are both initialized to #f by these "new-___"
;; initializers:
;;
;;   - "___-view" slots intended to hold information used for
;;   rendering the record structure in some kind of interactive user
;;   environment, such as a GUI or a CLI It is easier to place the
;;   slots for these view data structures directly into the record
;;   itself so that the view and the record can both be accessed from
;;   within the same thread via the same reference. Trying to wrap a
;;   buffer with a reference to its thread (the Goblins "vat") inside
;;   of a GUI view data structure would become very messy indeed.
;;   These fields can be initialized by parametrizable procedures.
;;
;;  - "___-cell" slots intended to hold a reference to a Goblins
;;    "cell" actor which contains the record itself. The Goblins
;;    library is used throughout this code to all for safe
;;    multi-threaded programming, and it tends to simplify matters if
;;    every object created in the system has a reference to it's own
;;    "cell" actor. These fields are initialized to #f unless the
;;    *do-init-cell-actors* function has been set within a Goblins
;;    "vat".
;;
;; Another architectural choice made here is to define paramaterizable
;; constructors for the views used in each of the record types, that
;; is, each view constructor is a lambda expression wrapped in a
;; parameter variable.  The reason for this is to make it easier use
;; multiple between front-ends at runtime, you can simply paramaterize
;; the "*impl/new-___-view*" function for each of the data types in
;; this module in a way that is specific to your front-end. Each of
;; these view constructors take a reference to the data structure that
;; is being defined.

;; -------------------------------------------------------------------------------------------------

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
           (window   (winframe-window winframe))
           (cmd      (*impl/self-insert-command*))
           )
       (cond
        ((not cmd) #f)
        ((not c) #f)
        ((procedure? cmd)
         (cmd window c)
         #t)
        ((command-type? cmd)
         ((command-procedure cmd) window c)
         #t)
        (else (error "*self-insert-command* does not contain a procedure" cmd)))    
       ))))


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
buffer of the current window."))


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
    (display ";;minibuffer-prompt-resume ")(write txt)(newline);;DEBUG
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
  )

(define =>buffer-handle
  (record-unit-lens buffer-handle set!buffer-handle '=>buffer-handle))

(define =>buffer-local-keymap
  (record-unit-lens buffer-local-keymap set!buffer-local-keymap '=>buffer-local-keymap))

(define =>buffer-view
  (record-unit-lens buffer-view set!buffer-view '=>buffer-view))

(define (insert string-or-char)
  (let*((window (selected-window))
        (buffer (window-buffer window))
        )
    ((*impl/insert-into-buffer*) buffer string-or-char)))

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
     (let*((keymap (if keymap keymap (*default-buffer-local-keymap*)))
           (cell (make<cell>))
           (this (make<buffer> cell keymap handle #f))
           (view ((*impl/new-buffer-view*) this))
           )
       (set!buffer-view this view)
       (set!cell-value cell this)
       this))))

;; -------------------------------------------------------------------------------------------------

(define-record-type <line-display-type>
  ;; This <line-display-type> is used to construct things that are
  ;; displayed in a single line in the user interface. This includes
  ;; things like the echo area, the minibuffer, the mode line, and the
  ;; header line. In the Gtk backend, these are all GtkFlowBox widgets
  ;; containing one or more GtkLabel widgets.
  (make<line-display-type> cell frame view)
  line-display-type?
  (cell   line-display-cell)
  (frame  line-display-parent-frame  set!line-display-parent-frame)
  (view   line-display-view          set!line-display-view))

(define =>line-display-view
  (record-unit-lens
   line-display-view
   set!line-display-view
   '=>line-display-view))

(define (new-line-display-backend backend-param)
  (lambda (parent-window)
    (let ((this (make<line-display-type> (make<cell>) parent-window #f)))
      (set!line-display-view this ((backend-param) this))
      this)))

(define new-echo-area   (new-line-display-backend *impl/new-echo-area-view*))
(define new-header-line (new-line-display-backend *impl/new-header-line-view*))

(define (new-mode-line parent-window items)
  (let ((line-display
         ((new-line-display-backend *impl/new-mode-line-view*) parent-window)))
    (mode-line-display-items parent-window (line-display-view line-display) items)
    line-display))

(define *mode-line-format*
  (make-parameter
   (list
    (lambda (_) (if (*impl/is-graphical-display?*) " " "-"))
    (lambda (_) "-") ;; buffer encoding
    (lambda (_) ":") ;; end of line style
    (lambda (_) "-") ;; buffer file is writable ("%" in read-only mode)
    (lambda (parent-window)  ;; "%" in read-only mode and unchanged
      (if (is-buffer-changed? parent-window) "*" "-"))
    (lambda (_) "-") ;; buffer is local or remote ("@" means remote)
    "  "
    (lambda (parent-window)
      (list
       #:propertize
       (view parent-window =>window-buffer =>buffer-handle)
       '(#:weight . "bold"))
      )
    )))

(define (mode-line-display-single parent-window widget-view stack item)
  (cond
   ((or (eq? #t item) (eq? #f item)) stack)
   ((procedure? item)
    (let ((next-item (item parent-window)))
      (mode-line-display-single
       parent-window widget-view stack next-item)))
   ((eq? #:separator item) (cons #:separator stack))
   ((pair? item)
    (cond
     ((eq? #:propertize (car item))
      (cond
       ((not (null? (cdr item)))
        (cons
         (cons
          #:propertize
          (cons
           (mode-line-display-single parent-window widget-view '() (cadr item))
           (cddr item)))
         stack))
       (else
        (mode-line-collect-items parent-window widget-view stack item)
        )))
     (else stack)))
   ((string? item) (cons item stack))
   (else
    (error "unknown mode-line item" item))
   ))

(define (mode-line-collect-items parent-window widget-view stack items)
  (display ";;mode-line-collect-items ")(write items)(newline)
  (let loop ((stack stack) (items items))
    (cond
     ((pair? items)
      (loop
       (mode-line-display-single
        parent-window widget-view stack (car items))
       (cdr items)))
     (else (reverse stack))
     )))

(define mode-line-display-items
  ;; Loop over the `ITEMS` (or `*MODE-LINE-FORMAT*` if no `ITEMS` are
  ;; given, or if `ITEMS` is #f) and collect strings, collect all
  ;; strings into a list, call the back-end function
  ;; `*IMPL/MODE-LINE-DISPLAY-ITEMS*`.
  ;;------------------------------------------------------------------
  (case-lambda
    ((parent-window widget-view)
     (mode-line-display-items parent-window widget-view #f))
    ((parent-window widget-view items)
     ((*impl/mode-line-display-items*)
      parent-window widget-view
      (mode-line-collect-items
       parent-window widget-view '()
       (if items items (*mode-line-format*)))))))

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
  (make<window> cell parent buffer keymap mode-line header-line view)
  window-type?
  (cell         window-cell)
  (parent       window-parent-frame  set!window-parent-frame)
  (buffer       %window-buffer       set!window-buffer)
  (keymap       window-local-keymap  set!window-local-keymap)
  (mode-line    window-mode-line     set!window-mode-line)
  (header-line  window-header-line   set!window-header-line)
  (view         window-view          set!window-view)
  )

(define window-buffer
  (case-lambda
    (() (window-buffer (selected-window)))
    ((window) (%window-buffer window))))

(define *default-window-local-keymap*
  ;; This keymap maps self-insert-key to all printable ASCII characters.
  (make-parameter #f))

(define =>window-local-keymap
  (record-unit-lens window-local-keymap set!window-local-keymap '=>window-local-keymap))

(define =>window-buffer
  (record-unit-lens %window-buffer set!window-buffer '=>window-buffer))

(define =>window-mode-line
  (record-unit-lens
   window-mode-line
   set!window-mode-line
   '=>window-mode-ilne))

(define =>window-header-line
  (record-unit-lens
   window-header-line
   set!window-header-line
   '=>window-header-line))

(define =>window-view
  (record-unit-lens window-view set!window-view '=>window-view))

(define (new-window-with-view make-view mode-line-items parent-winframe buffer keymap)
  ;; This window constructor lets you specify the view constructor, so
  ;; pick a nice one. This procedure is called by `NEW-WINDOW` to
  ;; construct ordinary buffer windows, and it is called by
  ;; `NEW-MINIBUFFER` to construct a window that contains the
  ;; minibuffer.
  ;;------------------------------------------------------------------
  (let*((cell (make<cell>))
        (buffer
         (cond
          ((buffer-type? buffer) buffer)
          ((not buffer) (new-buffer #f #f))
          (else (error "argument 2 to new-window not a buffer" buffer))))
        (keymap (keymap-arg-or-default keymap *default-window-local-keymap*))
        (this (make<window> cell parent-winframe buffer keymap #f #f #f))
        )
    (when mode-line-items
      (set!window-mode-line this (new-mode-line this mode-line-items)))
       ;; mode-line must be constructed before make-view
    (set!window-view this (make-view this))
    ;; NOTE: the header line is not created initially, it can be
    ;; created when there is a need to display it.
    (set!window-header-line this ((*impl/new-header-line-view*) this))
    (set!cell-value cell this)
    this))

(define (new-window parent buffer keymap)
  ;; Construct a new ordinary window -- ordinary, that is, as opposed
  ;; to the window constructed specifically to contain the minibuffer.
  ;;------------------------------------------------------------------
  (new-window-with-view (*impl/new-window-view*) (*mode-line-format*) parent buffer keymap))


(define (debug-print-keymaps winframe);;DEBUG
  (let*((window   (winframe-window    winframe))
        (buffer   (window-buffer        window))
        (winkmap  (window-local-keymap  window))
        (bufkmap  (if (buffer-type?  buffer) (buffer-local-keymap  buffer) #f))
        (frmkmap  (if (winframe-type? winframe) (winframe-local-keymap winframe) #f))
        (awaiting (winframe-awaiting-prompt? winframe))
        (print-keymap-name
         (lambda (field keymap)
           (print "#:" field #\space
            (or (and (not keymap) "#f")
                (view keymap km:=>keymap-label!)
                "<no-label>")))))
    (pretty
     (print
      (bracketed 2 #\( #\)
       "#:awaiting " awaiting #\space
       (print-keymap-name "bufkmap" bufkmap) #\space
       (print-keymap-name "winkmap" winkmap) #\space
       (print-keymap-name "frmkmap" frmkmap)
       )
      (line-break)))
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
        (let*((window   (winframe-window       winframe))
              (buffer   (window-buffer         window))
              (frmkmap  (winframe-local-keymap winframe))
              (winkmap  (window-local-keymap   window))
              (bufkmap  (buffer-local-keymap   buffer))
              (init-km  (km:keymap bufkmap winkmap frmkmap))
              (state (if (null? init-km) #f (km:new-modal-lookup-state init-km)))
              )
          (unless (not state)
            (set!winframe-modal-state winframe state))
          state)))))
   (else
    (error "not <winframe-type> data" winframe))))

(define (unbound-key-index-handler key-index)
  (format-message
   "~s is undefined\n"
   (km:keymap-index->list key-index)))

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

(define (new-minibuffer parent-winframe keymap)
  (let*((editor (winframe-parent-editor parent-winframe))
        (id (editor-get-next-obj-id editor))
        (handle (string-append " *Minibuf-" (number->string id) "*"))
        (keymap (if keymap keymap (*default-minibuffer-keymap*)))
        (buffer (new-buffer handle keymap))
        )
    (new-window-with-view (*impl/new-minibuffer-view*) #f parent-winframe buffer keymap)
    ))

;; -------------------------------------------------------------------------------------------------

(define-record-type <winframe-type>
  (make<winframe> cell editor window modal keymap echo minibuf dispatch prompt view)
  winframe-type?
  (cell     winframe-cell)
  (editor   winframe-parent-editor)
  (window   winframe-window         set!winframe-window)
  (modal    winframe-modal-state    set!winframe-modal-state)
  (keymap   winframe-local-keymap   set!winframe-local-keymap)
  (echo     winframe-echo-area      set!winframe-echo-area)
  (minibuf  winframe-minibuffer     set!winframe-minibuffer)
  (dispatch winframe-event-dispatch set!winframe-event-dispatch)  
  (prompt   winframe-prompt-stack   set!winframe-prompt-stack)
  (view     winframe-view           set!winframe-view)
  ;; Interesting note: it was here where I discovered that Guile does
  ;; NOT allow record types or field "<frame-type>" or have predicates
  ;; called "frame-type?". When I did try the "frame-type?" predicate,
  ;; I got some completely weird error:
  ;;
  ;;    Wrong type to apply: #<syntax-transformer frame-type?>
  ;;
  ;; This is definitely a bug and should be reported.
  )

(define *default-winframe-local-keymap*
  ;; This keymap maps self-insert-key to all printable ASCII characters.
  (make-parameter #f))

(define =>winframe-keymap
  (record-unit-lens
   winframe-local-keymap
   set!winframe-local-keymap
   '=>winframe-local-keymap))

(define =>winframe-window
  ;; This field should contain a reference to the currently focused
  ;; `<WINDOW-TYPE>`. The front-end GUI must setup event handlers that
  ;; update this field whenever the GUI engine generates an event
  ;; indicating that the user keyboard focus has changed. Provide an
  ;; implementation for *impl/select-window* that can be called both
  ;; programmatically and from the window focus event handler used in
  ;; the GUI front-end.
  ;;------------------------------------------------------------------
  (record-unit-lens
   winframe-window
   set!winframe-window
   '=>winframe-window))

(define =>winframe-modal-state
  (record-unit-lens
   winframe-modal-state
   set!winframe-modal-state
   '=>winframe-modal-state))

(define =>winframe-view
  (record-unit-lens
   winframe-view
   set!winframe-view
   '=>winframe-view))

(define =>winframe-minibuffer
  (record-unit-lens
   winframe-minibuffer
   set!winframe-minibuffer
   '=>winframe-minibuffer))

(define =>winframe-echo-area
  (record-unit-lens
   winframe-echo-area
   set!winframe-echo-area
   '=>winframe-echo-area))

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
   '=>winframe-event-dispatch))

(define =>winframe-prompt-stack
  ;; Keeps a continuation at which the end user has been prompted for
  ;; input, as what happens with the `SIMPLE-READ-MINIBUFFER`
  ;; API. When the end user presses the return key, or cancels, the
  ;; continuation at the top of this stack is resumed with the result
  ;; received from the end user.
  (record-unit-lens
   winframe-prompt-stack
   set!winframe-prompt-stack
   '=>winframe-prompt-stack))

(define (new-frame editor init-window init-keymap)
  ;; "keymap" is the keymap for this local frame, which is a fallback
  ;; keymap used when no key matches the buffer local keymap.
  (cond
   ((window-type? init-window)
    (let*((cell        (make<cell>))
          (init-keymap (keymap-arg-or-default init-keymap *default-winframe-local-keymap*))
          (modal-key-state #f)
          (echo-area   #f)
          (minibuffer  #f)
          (dispatch    (make-parameter #f))
          (prompt      '())
          (view        #f)
          (this
           (make<winframe>
            cell editor init-window modal-key-state init-keymap
            echo-area minibuffer dispatch prompt view))
          )
      (set!winframe-window     this init-window)
      (unless (window-parent-frame init-window)
        (set!window-parent-frame init-window this))
      (set!winframe-echo-area  this (new-echo-area this))
      (set!winframe-minibuffer this (new-minibuffer this #f))
      (set!winframe-view       this ((*impl/new-winframe-view*) this init-window))
      (set!cell-value cell this)
      this))
   ((not init-window) #f)
   (else (error "argument 1 to new-frame not a window" init-window))))

(define select-window
  (case-lambda
    ((window) (select-window window #f))
    ((window norecord)
     ;; TODO: if norecord is #f record this selection action into some kind of history object.
     (display ";;select-window ")(write (view window =>window-buffer =>buffer-handle))(newline);;DEBUG
     ((*impl/select-window*) window))
     (lens-set window (selected-frame) =>winframe-window)
    ))

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
           (dispatch) ; this does not return
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
        (prompt-item  (if (null? prompt-stack) #f (car prompt-stack)))
        )
    (display ";;winframe-prompt-resume ")(write return)(newline);;DEBUG
    (debug-print-keymaps winframe);;DEBUG
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
(define (selected-window) (view (selected-frame) =>winframe-window))
(define (current-buffer) (*impl/current-buffer*))
(define selected-buffer current-buffer)
  ;; These are global variables from the point of view of Emacs
  ;; Lisp. They are parameters in this library, and are parameterized
  ;; by the GUI provider library. These APIs are exposed in the
  ;; (GYPSUM EDITOR) library, but as procedures which cannot modify
  ;; the parameter.

;; -------------------------------------------------------------------------------------------------

(define-record-type <editor-type>
  ;; The editor state.
  (make<editor>
   cell buftab wintab frametab proctab keymap messages counter view)
  editor-type?
  (cell      editor-cell)
  (buftab    editor-buffer-table    set!editor-buffer-table)
  (wintab    editor-window-table    set!editor-window-table)
  (frametab  editor-winframe-table  set!editor-winframe-table)
  (proctab   editor-proc-table      set!editor-proc-table)
  (keymap    editor-base-keymap     set!editor-base-keymap)
  (messages  editor-messages        set!editor-messages)
  (counter   editor-obj-counter     set!editor-obj-counter)
  (view      editor-view            set!editor-view))

(define =>editor-buffer-table
  (record-unit-lens editor-buffer-table set!editor-buffer-table '=>editor-buffer-table))

(define =>editor-window-table
  (record-unit-lens editor-window-table set!editor-window-table '=>editor-window-table))

(define =>editor-winframe-table
  (record-unit-lens editor-winframe-table set!editor-winframe-table '=>editor-winframe-table))

(define =>editor-proc-table
  (record-unit-lens editor-proc-table set!editor-proc-table '=>editor-proc-table))

(define =>editor-base-keymap
  (record-unit-lens editor-base-keymap set!editor-base-keymap '=>editor-base-keymap))

(define =>editor-obj-counter
  (record-unit-lens editor-obj-counter set!editor-obj-counter '=>editor-obj-counter))

(define =>editor-view
  (record-unit-lens editor-view set!editor-view '=>editor-view))

(define (new-table size)
  ;; TODO: make use of the `SIZE` parameter
  (make-hash-table equal? default-hash))

(define (editor-get-next-obj-id editor)
  (let ((i (editor-obj-counter editor)))
    (set!editor-obj-counter editor (+ 1 i))
    i))

(define (current-editor) (*impl/current-editor-closure*))

(define (new-editor)
  ;; Construct a new text editor state.
  ;;
  ;; TODO: consider removing the <editor-type> entirely, and simply
  ;; using the Scheme environment object constructed by this (gypsum
  ;; editor) library as the editor state itself, with all fields of
  ;; <editor-type> redefined as parameters in this environment.
  (let*((cell           (make<cell>))
        (msgs           (new-buffer "*Messages*"))
        (buffer-table   (new-table 63))
        (winframe-table (new-table 15))
        (this
         (make<editor>
          cell ; editor-cell
          buffer-table ; buffer table
          (new-table 15) ; window table
          winframe-table ; frame table
          (new-table 15) ; process table
          (*default-keymap*) ; base-keymap
          msgs  ; messages minibuffer
          0     ; counter
          #f    ; view
          ))
        (view       ((*impl/new-editor-view*) this))
        (_          (set!editor-view this view))
        (window     (new-window #f msgs #f)) ; main window, because frames must have at least one
        (init-frame (new-frame this window #f)) ; initial frame
        )
    (lens-set msgs       this =>editor-buffer-table   (=>hash-key! (buffer-handle msgs)))
    (lens-set init-frame this =>editor-winframe-table (=>hash-key! "GUIgi-Shell"))
    (lens-set window     this =>editor-window-table   (=>hash-key! "main"))
    (set!cell-value cell this)
    (*impl/current-editor-closure* this)
    this))

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
    (debug-print-keymaps winframe);;DEBUG
    (let*((state (window-get-or-reset-modal-state! winframe))
          (window (selected-window))
          )
      (cond
       ((not state)
        ((*unbound-key-index*) window key-path))
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

(define (is-buffer-changed? win)
  ;; Takes a window, calls *IMPL/IS-BUFFER-modified?* on the buffer
  ;; that the window is currently displaying
  ;; ------------------------------------------------------------------
  ((*impl/is-buffer-modified?*) (view win =>window-buffer)))

;; TODO: when constructing the editor, frames, windows, and buffers,
;; make the construction happen in two passes. First, the record data
;; types are all constructed. Then a second function is called,
;; something like "init-views", which then evaluates the view
;; *impl/...* function for the editor and all of its sub fields. The
;; idea is that the whole editor data structure should exist in its
;; entirity as early as possible, as a kind of "superstructure", and
;; then the GUI is constructed on top of this superstructure.

(define (get-minibuffer-text frame)
  (display ";;get-minibuffer-text\n");;DEBUG
  ((*impl/get-minibuffer-text*) frame))

(define (exit-minibuffer-with-return return)
  ;; This procedure (or the `EXIT-MINIBUFFER` command) should be bound
  ;; to the `<RETURN>` key for a minibuffer. If there has been a call
  ;; to `WINFRAME-AWAIT-PROMPT`, and therfore exists a continuation
  ;; saved which contains the stack of the procedure that called it,
  ;; this function resumes that continuation with the return value to
  ;; that procedure call given as an argument to this procedure.
  (display ";;exit-minibuffer-with-return\n");;DEBUG
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
  (lens-set (winframe-minibuffer winframe) winframe =>winframe-window)
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
           (minibuf  (winframe-minibuffer winframe))
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
  ;; out of this `(GYPSUM EDITOR)` library and into their own "compat"
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
                         (lambda () (gypsum:eval-string input-string))
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
  ((*impl/command-error-default-function*) data context signal))

(define command-error-function (make-parameter command-error-default-function))

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
   (pp-state-indent pp)))

(define (buffer-print-finalize pp) (values))

(define print-to-buffer
  ;; Construct a pretty-printer state structure, the record data
  ;; returned by this procedure can be used as the first argument to
  ;; `PRETTY` in the `(GYPSUM PRETTY)` library. The "port" in this
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
      buffer ;;line-buffer
      (*impl/insert-into-buffer*) ;;output-port
      indent
      indent-char
      #f ;;start-of-line
      ))
    ))

;; -------------------------------------------------------------------------------------------------

(define default-keymap
  (km:keymap '*default-keymap*
    (km:alist->keymap-layer
     `(((#\backspace) . ,delete-backward-char)
       ((#\delete)    . ,delete-char)
       ((meta #\:)    . ,eval-expression)
       ))
    self-insert-layer))


(define *default-keymap*
  ;; The keymap for all keys that are not pressed simultaneously with
  ;; modifier keys.
  (make-parameter default-keymap))


(define *default-buffer-local-keymap*
  ;; This keymap maps self-insert-key to all printable ASCII characters.
  (make-parameter default-keymap))


(define *default-minibuffer-keymap*
  (make-parameter
   (km:keymap
    '*default-minibuffer-keymap*
    (km:alist->keymap-layer
     `(((#\return) . ,exit-minibuffer)
       ((#\newline) . ,exit-minibuffer)
       ))
    (*default-keymap*))))


;;--------------------------------------------------------------------
;; TODO: fix bug, the call to `EXIT-MINIBUFFER-WITH-RETURN` does not
;; restore the window correctly, somehow the minibuffer keymap is
;; still used to respond to key events after calling this function.

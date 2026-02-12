(define-library (schemacs apps emacs)
  ;; This is the base package for the Schemacs editor, which is a
  ;; clone of Emacs written in Scheme. It should be thought of as one
  ;; Schemacs apps among many, but the one most people expect to use
  ;; given that the audience for this application is the Emacs
  ;; community.
  ;;
  ;; The reference implementation is written in Guile Scheme which
  ;; comes with an Emacs Lisp compiler, and therefore is not just a
  ;; clone of the Emacs editor, but of the Emacs Lisp programming
  ;; language.

  (import
    (scheme base)
    (scheme lazy)
    (scheme write)
    (only (scheme read) read)
    (only (scheme case-lambda) case-lambda)
    (only (srfi 28) format)
    (only (schemacs hash-table)
          make-hash-table  default-hash
          hash-table-ref/default  hash-table-set!
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
    (prefix (schemacs editor-impl) *old-impl/)
    (prefix (schemacs ui text-buffer) impl/)
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
          run-div-monad  enclose  expand  content  selector
          state-var  use-vars  get-var  =>state-var-value*!
          div  view-type  properties  =>div-properties*!
          div-pack  pack-elem  cut-horizontal  cut-vertical
          div-space   floater  print-div
          div-select  top-div-select  by-div-type
          tiled-windows  text-editor
          use-vars-value  update-var  after-update
          signal-focus!  is-graphical-display?
          =>div-widget*!
          ))

  (export
   ;; A quick note on naming: names delimited with asterisks,
   ;; e.g. *default-keymap* are parameter objects. Asterisk-delimited
   ;; names beginning with "impl/..." for example
   ;; *OLD-IMPL/NEW-WINFRAME-VIEW* are parameters that SHOULD be
   ;; parameterized by the user interface backend, but are typically
   ;; parameterized with procedures that do nothing by default.

   main

   ;; ---------------- Buffers ----------------
   buffer-type?  buffer-cell
   =>buffer-handle  =>buffer-view  =>buffer-local-keymap
   *default-buffer-local-keymap*
   current-buffer  selected-buffer
   messages-buffer  generate-new-buffer-name

   ;; -------------- Mode Lines --------------
   ;; This includes the echo area
   line-display-type?
   new-line-display  new-mode-line  new-header-line  new-echo-area
   *mode-line-format*  *header-line-format*

   ;; ---------------- Windows ----------------
   window-type?
   window-buffer  window-parent-frame
   =>window-buffer  =>window-local-keymap
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
   =>winframe-local-keymap
   =>winframe-echo-area  =>winframe-minibuffer  =>winframe-prompt
   *default-winframe-local-keymap*
   *default-minibuffer-keymap*
   selected-frame
   key-event-handler
   message  display-in-echo-area  clear-echo-area
   simple-read-minibuffer  read-from-minibuffer
   exit-minibuffer
   focus-minibuffer
   clear-minibuffer
   eval-minibuffer
   minibuffer-prompt-resume
   exit-minibuffer-with-return

   ;; ---------------- Front-end API ----------------
   new-self-insert-keymap  key-event-self-insert
   default-keymap
   print-exception-message
   self-insert-keymap  self-insert-command
   delete-char delete-backward-char
   unbound-key-index  waiting-key-index  dispatch-key-event
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
       (else (error "argument not a <keymap-type>" km-arg))
       ))

    ;; -------------------------------------------------------------------------------------------------
    ;; Keymaps defined prior to launching the programming editor GUI.

    (define (uarg->integer dflt uarg)
      (cond
       ((eq? #f uarg) dflt)
       ((eq? #t uarg) 4)
       ((integer? uarg) uarg)
       ((number? uarg) (round uarg))
       (error "U-argument cannot be cast to integer" uarg)
       ))

    (define key-event-self-insert
      (case-lambda
        ((uarg)
         (let*((winframe   (selected-frame))
               (state      (winframe-modal-state winframe))
               (key        (km:modal-lookup-state-key-index state))
               (on-success (lambda (c) c))
               (on-fail    (lambda () #f))
               (c (and key
                       (km:keymap-index-to-char
                        key #f on-success on-fail
                        ))))
           (key-event-self-insert uarg c)
           ))
        ((uarg c)
         (let ((buf (current-buffer)))
           (let loop ((uarg (uarg->integer 1 uarg)))
             (cond
              ((< 0 uarg)
               (impl/insert-char buf c)
               (loop (- uarg 1))
               )
              (else #f)
              ))))))

    (define self-insert-command
      ;; This is an actual command (not just a procedure) that calls the
      ;; procedured stored in the *old-impl/self-insert-command* parameter
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
      (km:new-self-insert-keymap-layer #f (lambda (c) self-insert-command) (lambda () #f))
      )

    (define (minibuffer-prompt-resume)
      (let*((winframe (selected-frame))
            (txt (get-minibuffer-text winframe))
            )
        (winframe-prompt-resume winframe txt)
        ))

    ;; -------------------------------------------------------------------------------------------------

    (define-record-type <buffer-type>
      (make<buffer> keymap handle view)
      buffer-type?
      (keymap  buffer-local-keymap  set!buffer-local-keymap)
      (handle  buffer-handle        set!buffer-handle)
      (view    buffer-view          set!buffer-view)
      )

    (define =>buffer-handle
      (record-unit-lens buffer-handle set!buffer-handle '=>buffer-handle)
      )

    (define =>buffer-local-keymap
      (record-unit-lens buffer-local-keymap set!buffer-local-keymap '=>buffer-local-keymap)
      )

    (define =>buffer-view
      (record-unit-lens buffer-view set!buffer-view '=>buffer-view)
      )

    (define-record-type <buffer-table-type>
      (make<buffer-table> counter messages hash-table)
      buffer-table-type?
      (counter     buffer-table-counter  set!buffer-table-counter)
      (messages    buffer-table-messages  set!buffer-table-messages)
      (hash-table  buffer-table-hash-table)
      )

    (define *buffer-table*
      (make-parameter (make<buffer-table> 0 #f (make-hash-table string=?)))
      )

    (define (buffer-table-next-count! bt)
      (let ((count (+ 1 (buffer-table-counter bt))))
        (set!buffer-table-counter bt count)
        count
        ))

    (define (generate-new-buffer-name name)
      (let*((bt (*buffer-table*))
            (ht (buffer-table-hash-table bt))
            (count (if name 1 (buffer-table-next-count! bt)))
            (name (or name "Buffer"))
            )
        (let loop ((count count) (name name))
          (let ((already-taken (hash-table-ref/default ht name #f)))
            (cond
             (already-taken
              (loop
               (+ 1 count)
               (string-append name "-" (number->string count))
               ))
             (else name)
             )))))

    (define (messages-buffer)
      (let*((bt (*buffer-table*))
            (messages (buffer-table-messages bt))
            )
        (cond
         (messages messages)
         (else
          (let*((messages (new-buffer "*Messages*")))
            (set!buffer-table-messages bt messages)
            messages
            )))))

    (define (insert string-or-char)
      (let ((buffer (current-buffer)))
        (cond
         ((char? string-or-char) (impl/insert-char buffer string-or-char))
         ((string? string-or-char) (impl/insert-string buffer string-or-char))
         (error "argument must be a string or char" string-or-char)
         )))

    (define delete-char
      (new-command
       "delete-char"
       (lambda () (apply-command delete-char 1 #f))
       (lambda args (impl/delete-from-cursor (current-buffer) 1))
       "Takes 2 arguments, an integer number of characters to delete after
    the cursor (negative integers delete before the cursor), followed by a
    boolean indicating whether the deleted characters should be copied to
    the \"kill ring\"."
       ))

    (define delete-backward-char
      (new-command
       "delete-backward-char"
       (lambda () (apply-command delete-char -1 #f))
       (lambda args (impl/delete-from-cursor (current-buffer) -1))
       "Delete the previous N characters (following if N is negative).
    If Transient Mark mode is enabled, the mark is active, and N is 1,
    delete the text in the region and deactivate the mark instead.
    To disable this, set option 'delete-active-region' to nil."
       ))

    (define new-buffer
      ;; Construct a `<BUFFER-TYPE>`, calling `*IMPL/NEW-BUFFER-VIEW*`
      ;; to construct the actual text buffer. In Gtk back-ends, the
      ;; `GtkTextBuffer` object is constructed and stored in the
      ;; `<BUFFER-TYPE>` that is created by this procedure.
      ;;------------------------------------------------------------------
      (case-lambda
        ((handle) (new-buffer handle #f))
        ((handle keymap)
         (let*((handle (generate-new-buffer-name handle))
               (keymap (or keymap (*default-buffer-local-keymap*)))
               (this (make<buffer> keymap handle (impl/new-buffer)))
               (bt (*buffer-table*))
               )
           (hash-table-set! (buffer-table-hash-table bt) handle this)
           this
           ))))

    ;; -------------------------------------------------------------------------------------------------

    (define-record-type <line-display-type>
      ;; This <line-display-type> is used to construct things that are
      ;; displayed in a single line in the user interface. This includes
      ;; things like the echo area, the minibuffer, the mode line, and the
      ;; header line. In the Gtk backend, these are all GtkFlowBox widgets
      ;; containing one or more GtkLabel widgets.
      (make<line-display-type> parent prompt-var buffer)
      line-display-type?
      (parent      line-display-parent-frame  set!line-display-parent-frame)
      (prompt-var  line-display-prompt-var    set!line-display-prompt-var)
      (buffer      line-display-buffer        set!line-display-buffer)
      )

    (define (new-line-display parent buf-name init-keymap prompt-str)
      (make<line-display-type>
       parent
       (and prompt-str (state-var string=? prompt-str))
       (new-buffer buf-name init-keymap)
       ))

    (define (line-display-view o)
      (let*((parent     (line-display-parent-frame o))
            (buffer     (line-display-buffer       o))
            (prompt-var (line-display-prompt-var   o))
            (editline
             (text-editor
              (buffer-view buffer)
              (properties
               'on-key-event:
               (cond
                (prompt-var
                 (lambda (_o key-path)
                   (let*((window (winframe-selected-window parent)))
                     (parameterize
                         ((selected-frame  parent)
                          (selected-window window)
                          (current-buffer  buffer)
                          )
                       (key-event-handler
                        parent (*default-minibuffer-keymap*) key-path
                        )))))
                (else #f)
                )))))
        (cond
         (prompt-var
          (div-pack
           cut-vertical (view-type o)
           (selector 'minibuffer-area)
           (properties 'wrapping: #t)
           (size2D expand enclose)
           (pack-elem
            (size2D enclose)
            (use-vars (list prompt-var) div)
            )
           (pack-elem (size2D expand enclose) editline)
           ))
         (else editline)
         )))

    (define (new-echo-area  parent)
      (new-line-display parent "echo-area" #f #f)
      )

    (define (new-minibuffer parent)
      (new-line-display
       parent "minibuffer"
       (*default-minibuffer-keymap*) ""
       ))

    (define (new-header-line parent-window st)
      (new-mode-line parent-window (or st *header-line-format*))
      )

    (define (new-mode-line parent-window st)
      ;; A mode line is just a `use-vars` node that constructs a `div`
      ;; node containing a variety of indicators for displaying
      ;; informaion about the current window and the buffer it is
      ;; displaying.
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
             ((mode-line-display-items parent-window) items)
             )))))

    (define *header-line-format*
      (make-parameter (state-var equal? #f))
      )

    (define *mode-line-format*
      (make-parameter
       (state-var
        equal?
        (list
         (lambda (_) (if (is-graphical-display?) " " "-"))
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
            (view
             parent-window =>window-buffer =>state-var-value*! =>buffer-handle
             )
            '(weight: . "bold")
            ))))))

    (define (mode-line-display-single parent-window stack item)
      (define (propertize elems props)
        (let*((elems
               (mode-line-collect-items
                parent-window '() elems
                )))
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
      ;; Loop over the `ITEMS` (or `*MODE-LINE-FORMAT*` if no `ITEMS`
      ;; are given, or if `ITEMS` is #f) and collect strings, collect
      ;; all strings into a list.
      ;;--------------------------------------------------------------
      (lambda (items)
        (and items
             (apply
              div-pack cut-vertical
              (mode-line-collect-items parent-window '() items)
              ))))

    ;;----------------------------------------------------------------

    (define-record-type <window-type>
      ;; This is a wrapper around the Gtk back-end data structures that
      ;; behave like an Emacs text window.
      ;; 
      ;; In Emacs nomencalture a "window" is a view for a
      ;; buffer. Therefore in Gtk nomenclature, a GtkTextView widget is
      ;; the nearest thing to what Emacs considers to be a window. You may
      ;; provide a buffer as an argument, or #f. If you do not
      ;; provide a buffer, an new empty buffer is created.
      (make<window> parent buffer keymap mode-line header-line)
      window-type?
      (parent       window-parent-frame  set!window-parent-frame)
      (buffer       %window-buffer       set!window-buffer)
      (keymap       window-local-keymap  set!window-local-keymap)
      (mode-line    window-mode-line     set!window-mode-line)
      (header-line  window-header-line   set!window-header-line)
      )

    (define (new-window parent buffer keymap)
      ;; Construct a new ordinary window -- ordinary, that is, as opposed
      ;; to the window constructed specifically to contain the minibuffer.
      ;;------------------------------------------------------------------
      (let*((buffer (or buffer (messages-buffer)))
            (bufview (state-var buffer))
            (keymap
             (keymap-arg-or-default
              keymap *default-window-local-keymap*
              ))
            (this (make<window> parent bufview keymap #f #f))
            (mode-line (new-mode-line this (*mode-line-format*)))
            (header-line
             (new-header-line this (*header-line-format*))
             ))
        (set!window-mode-line   this mode-line)
        (set!window-header-line this header-line)
        this
        ))

    (define (window-view o)
      (let*((parent      (window-parent-frame o))
            (bufview     (%window-buffer      o))
            (keymap      (window-local-keymap o))
            (mode-line   (window-mode-line    o))
            (header-line (window-header-line  o))
            )
        (div-pack
         cut-horizontal (view-type o)
         header-line
         (use-vars
          (list bufview)
          (lambda (buffer)
            (text-editor
             (content (buffer-view buffer))
             (properties
              'on-key-event:
              (lambda (_o key-path)
                (display "; WV key ") (write (km:keymap-index->list key-path)) (newline);;DEBUG
                (parameterize
                    ((selected-frame parent)
                     (selected-window o)
                     (current-buffer
                      (view (%window-buffer o) =>state-var-value*!)
                      ))
                  (key-event-handler
                   parent (collect-keymaps parent) key-path
                   )))
              'on-get-focus:
              (lambda ignore-args
                (set!winframe-selected-window parent o) #t
                )))))
         mode-line
         )))

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

    (define (debug-print-keymaps winframe) ;;DEBUG
      (let*((window   (winframe-selected-window  winframe))
            (buffer   (window-buffer             window))
            (winkmap  (window-local-keymap       window))
            (bufkmap
             (and (buffer-type? buffer)
                  (buffer-local-keymap  buffer)
                  ))
            (frmkmap
             (and (winframe-type? winframe)
                  (winframe-local-keymap winframe)
                  ))
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
          (bracketed
           2 #\( #\)
           "#:awaiting " awaiting #\space
           (print-keymap-name "bufkmap" bufkmap) #\space
           (print-keymap-name "winkmap" winkmap) #\space
           (print-keymap-name "frmkmap" frmkmap)
           )
          (line-break)
          ))
        #t))

    (define (collect-keymaps winframe)
      (let*((window   (winframe-selected-window  winframe))
            (buffer   (view (window-buffer window) =>state-var-value*!))
            (frmkmap  (winframe-local-keymap     winframe))
            (winkmap  (window-local-keymap       window))
            (bufkmap  (buffer-local-keymap       buffer))
            )
        (km:keymap bufkmap winkmap frmkmap)
        ))

    (define (window-get-or-reset-modal-state! winframe keymap)
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
            (let*((state
                   (and keymap (not (null? keymap))
                        (km:new-modal-lookup-state keymap)
                        )))
              (unless (not state)
                (set!winframe-modal-state winframe state)
                ;; TODO: write the modal state into the echo area
                )
              state
              )))))
       (else
        (error "not <winframe-type> data" winframe)
        )))

    (define (unbound-key-index key-index)
      (message
       "~s is undefined\n"
       (km:keymap-index->list key-index)
       ))

    (define (waiting-key-index key-path keymap)
      (message "~s-" (km:keymap-index->list (force key-path)))
      )

    (define (dispatch-key-event winframe)
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
      (lambda (key-path action)
        (let ((run-action
               (cond
                ((command-type? action)
                 (lambda () ((command-procedure action)) #f)
                 )
                ((procedure?    action)
                 (lambda () (action) #f)
                 )
                (else
                 (error
                  "cannot dispatch key event to non command type"
                  (force key-path) action
                  )))))
          (call/cc
           (lambda (after-read-minibuffer)
             (parameterize
                 (((winframe-event-dispatch winframe) after-read-minibuffer))
               (run-action)
               ))))))

    ;;----------------------------------------------------------------

    (define-record-type <winframe-type>
      (make<winframe>
       window  layout  modal  keymap  minibuf
       echo  echo-st  dispatch  prompt
       )
      winframe-type?
      (window    winframe-selected-window  set!winframe-selected-window)
      (layout    winframe-layout           set!winframe-layout)
      (modal     winframe-modal-state      set!winframe-modal-state)
      (keymap    winframe-local-keymap     set!winframe-local-keymap)
      (minibuf   winframe-minibuffer       set!winframe-minibuffer)
      (echo      winframe-echo-area        set!winframe-echo-area)
      (echo-st   winframe-echo-area-state  set!winframe-echo-area-state)
      (dispatch  winframe-event-dispatch   set!winframe-event-dispatch)  
      (prompt    winframe-prompt-stack     set!winframe-prompt-stack)
      ;; Interesting note: it was here where I discovered that Guile does
      ;; NOT allow record types or field "<frame-type>" or have predicates
      ;; called "frame-type?". When I did try the "frame-type?" predicate,
      ;; I got some completely weird error:
      ;;
      ;;    Wrong type to apply: #<syntax-transformer frame-type?>
      ;;
      ;; This is definitely a bug and should be reported.
      )

    (define (new-frame init-buffer init-keymap)
      ;; "keymap" is the keymap for this local frame, which is a fallback
      ;; keymap used when no key matches the buffer local keymap.
      (let*((init-buffer (or init-buffer (messages-buffer)))
            (init-keymap
             (keymap-arg-or-default
              init-keymap
              *default-winframe-local-keymap*
              ))
            (init-window      #f)
            (layout           #f)
            (modal-key-state  #f)
            (minibuffer       #f)
            (echo-area        #f)
            (echo-st          #f)
            (dispatch         (make-parameter #f))
            (prompt           '())
            (this
             (make<winframe>
              init-window  layout  modal-key-state  init-keymap
              minibuffer  echo-area  echo-st  dispatch  prompt
              ))
            (init-window  (new-window this init-buffer init-keymap))
            (echo-area    (new-echo-area this))
            (minibuffer   (new-minibuffer this))
            (layout       (state-var init-window))
            (echo-st      (state-var echo-area))
            )
        (set!winframe-selected-window this init-window)
        (set!winframe-layout          this layout)
        (set!winframe-minibuffer      this minibuffer)
        (set!winframe-echo-area       this echo-area)
        (set!winframe-echo-area-state this echo-st)
        this
        ))

    (define (winframe-view o)
      (let*((layout  (winframe-layout          o))
            (echo-st (winframe-echo-area-state o))
            )
        (floater
         (rect2D 0 0 expand expand)
         (div-pack
          cut-horizontal (view-type o)
          (pack-elem
           (size2D expand expand)
           (use-vars (list layout) window-view)
           )
          (pack-elem
           (size2D expand enclose)
           (use-vars (list echo-st) line-display-view)
           )))))

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
         (let*((result (signal-focus! (window-view window))))
           (when result
             (set!winframe-selected-window (selected-frame) window)
             )
           result
           ))))

    ;; -------------------------------------------------------------------------------------------------

    (define-record-type <minibuffer-prompt-type>
      ;; Contains information used to emulate the Emacs Lisp
      ;; `read-from-minibuffer`. Holds the target window in which
      ;; commands should update state, and a continuation which is to
      ;; be continued with the string contents of the minibuffer
      ;; argument after the end-user presses "enter" in the
      ;; minibuffer. Note that this is only for Emacs Lisp emulation,
      ;; reading from the minibuffer in Scheme using
      ;; `focus-minibuffer` does not need to remember the continuation
      ;; from which it was called.
      ;;--------------------------------------------------------------
      (make<minibuffer-prompt> current-window continuation)
      minibuffer-prompt-type?
      (current-window  minibuffer-prompt-selected-window)
      (continuation    minibuffer-prompt-continuation)
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
        (let*((dispatch ((winframe-event-dispatch winframe))))
          (display ";;winframe-await-prompt\n") ;;DEBUG
          (cond
           (dispatch
            (call/cc
             (lambda (cont)
               (display "; push recursive edit prompt stack\n") ;;DEBUG
               (update
                (lambda (stack)
                  (cons ;; here is where we push the continuation onto the stack
                   (make<minibuffer-prompt> window cont)
                   stack
                   ))
                winframe =>winframe-prompt-stack
                )
               (dispatch) ;; this does not return
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
        (cond
         ((minibuffer-prompt-type? prompt-item)
          (let ((prompt  (minibuffer-prompt-continuation    prompt-item))
                (window  (minibuffer-prompt-selected-window prompt-item))
                )
            (select-window window #t)
            (display "; pop recursive edit prompt stack\n") ;;DEBUG
            (update
             (lambda (stack) (cdr prompt-stack))
             winframe =>winframe-prompt-stack
             )
            (prompt return)
            ))
         (else (error "no recursive edit prompt on the stack" prompt-stack))
         )))

    ;;----------------------------------------------------------------

    (define selected-frame  (make-parameter #f))
    (define selected-window (make-parameter #f))
    (define current-buffer  (make-parameter #f))
    (define selected-buffer current-buffer)

    ;;----------------------------------------------------------------

    (define (key-event-handler winframe keymaps key-path)
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
        (let*((state (window-get-or-reset-modal-state! winframe keymaps)))
          (cond
           ((not state) (unbound-key-index key-path))
           ((km:modal-lookup-state-type? state)
            (let ((is-waiting
                   (km:modal-lookup-state-step!
                    state key-path
                    (dispatch-key-event winframe)
                    waiting-key-index
                    unbound-key-index
                    )))
              (unless is-waiting (set!winframe-modal-state winframe #f))
              is-waiting
              ))
           (else
            (error
             "unknown object in modal-state-keymap field of window"
             state winframe
             )))))
       (else
        (error "key event handler must be provided a window frame" winframe)
        )))

    (define (clear-echo-area winframe)
      (let*((echo-area (winframe-echo-area winframe))
            (textbuf (line-display-buffer echo-area))
            )
        (impl/delete-range echo-area 0 -1)
        ))

    (define (display-in-echo-area winframe str)
      (let*((msgbuf (messages-buffer))
            (echo-area (winframe-echo-area winframe))
            (textbuf (line-display-buffer echo-area))
            )
        (impl/delete-range textbuf 0 -1)
        (impl/insert-string textbuf str)
        (impl/insert-string msgbuf str)
        ))

    (define (message msgstr . objlist)
      (let*((winframe (selected-frame))
            (str (apply format (cons msgstr objlist)))
            )
        (display-in-echo-area winframe str)
        ))

    (define (is-buffer-changed? window)
      ;; Takes a window, calls *OLD-IMPL/IS-BUFFER-modified?* on the buffer
      ;; that the window is currently displaying
      ;; ------------------------------------------------------------------
      ((*old-impl/is-buffer-modified?*) (view window =>window-buffer))
      )

    (define (get-minibuffer-text winframe)
      (impl/copy-string
       (line-display-buffer (winframe-minibuffer winframe)) 0 -1
       ))

    (define (exit-minibuffer-with-return winframe return)
      ;; This procedure (or the `EXIT-MINIBUFFER` command) should be bound
      ;; to the `<RETURN>` key for a minibuffer. If there has been a call
      ;; to `WINFRAME-AWAIT-PROMPT`, and therfore exists a continuation
      ;; saved which contains the stack of the procedure that called it,
      ;; this function resumes that continuation with the return value to
      ;; that procedure call given as an argument to this procedure.
      (let*((window    (winframe-selected-window winframe))
            (echo-area (winframe-echo-area       winframe))
            (echo-st   (winframe-echo-area-state winframe))
            )
        ((update-var echo-st (lambda _ echo-area)))
        (select-window window #t)
        return
        ))

    (define exit-minibuffer
      (new-command
       "exit-minibuffer"
       (lambda ()
         (let ((winframe (selected-frame)))
           (exit-minibuffer-with-return
            winframe
            (winframe-prompt-resume
             winframe
             (clear-minibuffer winframe #t)
             ))))
       exit-minibuffer-with-return
       "Terminate this minibuffer argument."
       ))

    (define (focus-minibuffer winframe prompt-str init-input keymap)
      ;; Move the keyboard focus to the minibuffer and change it's
      ;; prompt string. For the `PROMPT` argument, you may pass `#f`
      ;; to display no prompt string, or pass `#t` to reuse the
      ;; previous prompt string values. This function does not save
      ;; the current continuation into the prompt stack, it simply
      ;; asks the widget toolkit to show and swtich focus to the
      ;; minibuffer.
      ;;--------------------------------------------------------------
      (let*((echo-st (winframe-echo-area-state winframe))
            (minibuf (winframe-minibuffer winframe))
            )
        ((update-var
          echo-st
          (lambda (echo-st)
            (cond
             ((eq? echo-st minibuf)
              (error "command attempted to use minibuffer while in minibuffer")
              )
             (else
              (display "; focus-minibuffer ") (write prompt-str) (newline) ;;DEBUG
              (let*((prompt-var (line-display-prompt-var minibuf))
                    (textbuf (line-display-buffer minibuf))
                    )
                (cond
                 ((eq? #t prompt-str) (values))
                 (else
                  ((update-var
                    prompt-var
                    (lambda (old-prompt-str)
                      (display "; update prompt string ") (write prompt-str) (newline) ;;DEBUG
                      (cond
                       ((eq? #f prompt-str) "")
                       ((string? prompt-str) prompt-str)
                       (else (error "not a string" prompt-str))
                       ))))))
                (cond
                 ((string? init-input)
                  (impl/delete-range textbuf 0 -1)
                  (impl/insert-string textbuf init-input)
                  )
                 ((eq? #f init-input)
                  (impl/delete-range textbuf 0 -1)
                  )
                 ((eq? #t init-input) (values))
                 (else (error "not an initial input string" init-input))
                 )
                (display "; top-div-select signal-focus! 'minibuffer-area\n");;DEBUG
                (after-update
                 (lambda ()
                   (top-div-select
                    (lambda (o)
                      (display "; signal-focus! ") (write o) (newline) ;;DEBUG
                      (signal-focus! o)
                      )
                    'minibuffer-area
                    (by-div-type text-editor)
                    )))
                minibuf
                ))))))))

    (define (clear-minibuffer winframe get-string?)
      (let*((minibuf (winframe-minibuffer winframe))
            (textbuf (line-display-buffer minibuf))
            (text    (and get-string? (impl/copy-string textbuf 0 -1)))
            )
        (impl/delete-range textbuf 0 -1)
        text
        ))

    (define simple-read-minibuffer
      ;; This is a better API to use than the usual Emacs
      ;; `READ-FROM-MINIBUFFER` API which is more procedural than
      ;; functional in style and also loaded with historical baggage.
      ;; This procedure calls `read-from-minibuffer`.
      (case-lambda
        ((prompt-str init-input)
         (simple-read-minibuffer prompt-str init-input #f)
         )
        ((prompt-str init-input keymap)
         (let*((window   (selected-window))
               (winframe (window-parent-frame window))
               )
           (focus-minibuffer winframe prompt-str init-input keymap)
           (let ((result (winframe-await-prompt winframe window)))
             (display "; simple-read-minibuffer got result: ") (write result) (newline);;DEBUG
             (clear-minibuffer winframe #f)
             (exit-minibuffer-with-return winframe result)
             )))))

    (define read-from-minibuffer
      ;; This API mimics the Emacs API of the same name, but otherwise
      ;; simply calls the `SIMPLE-READ-MINIBUFFER` procedure. It sets
      ;; the UI into a mode in which the keyboard focus is on the
      ;; minibuffer, and the enter or return keys are bound to
      ;; procedures which collect that input and perform some action
      ;; on it. When you call this procedure, it will not return right
      ;; away, it will save the current continuation into a slot of
      ;; the current window frame. When the end-user presses the
      ;; "enter" key, or otherwise somehow has `exit-minibuffer`
      ;; invoked, the continuation which invoked this procedure will
      ;; be continued with the result of the minibuffer input.
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
            )))))

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
                          (qstr input-string) (line-break)
                          )))
               (result
                (cond
                 ((string=? "" input-string) #f)
                 (else
                  (call/cc
                   (lambda (halt)
                     (with-exception-handler (lambda (caught) (halt caught))
                       (lambda () (eval-string input-string))
                       ))))))
               (output-string
                (cond
                 (result
                  (call-with-port (open-output-string)
                    (lambda (port)
                      (write result port)
                      (flush-output-port port)
                      (get-output-string port))))
                 (else #f)
                 )))
           (when output-string
             (display-in-echo-area winframe output-string)
             )
           result
           ))))

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
      ((*old-impl/command-error-default-function*) data context signal)
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
             (insert
              buffer
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
      (%buffer-write-line
       pp (pp-line-indent-char line)
       (pp-line-indent line)
       (pp-line-string line)
       ))

    (define (buffer-first-indent pp)
      (%buffer-write-line
       pp (pp-state-indent-char pp)
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
          buffer ;;line-buffer
          insert ;;output-port
          indent
          indent-char
          #f ;;start-of-line
          ))))

    ;; -------------------------------------------------------------------------------------------------

    (define default-keymap
      (km:keymap
       '*default-keymap*
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

    (define (main . args)
      ;; Calls `new-frame` and returns the `DIV` node that was
      ;; constructed for it.
      ;;--------------------------------------------------------------
      ;; TODO: handle configuration variables and other arguments.
      (let*((frame (new-frame #f #f)))
        (winframe-view frame)
        ))

    ;;----------------------------------------------------------------
    ))

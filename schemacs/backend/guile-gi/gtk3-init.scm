
(gi:use-typelibs
 (("GLib" "2.0")
  #:prefix glib:))

(gi:use-typelibs
 (("Gio" "2.0")
  #:select
  (activate)
  #:prefix gio:)
 )

;;(gi:use-typelibs
;; (("GObject" "2.0")
;;  #:prefix gobj:))

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
(gi-repo:load-by-name "Gtk" "Container")
(gi-repo:load-by-name "Gtk" "EventController")
(gi-repo:load-by-name "Gtk" "EventControllerKey")
(gi-repo:load-by-name "Gtk" "FlowBox")
(gi-repo:load-by-name "Gtk" "IMContext")
(gi-repo:load-by-name "Gtk" "Label")
(gi-repo:load-by-name "Gtk" "Orientation")
(gi-repo:load-by-name "Gtk" "Separator")
(gi-repo:load-by-name "Gtk" "ScrolledWindow")
(gi-repo:load-by-name "Gtk" "TextBuffer")
(gi-repo:load-by-name "Gtk" "TextIter")
(gi-repo:load-by-name "Gtk" "TextView")
(gi-repo:load-by-name "Gtk" "DeleteType")
(gi-repo:load-by-name "Gtk" "Widget") ;; show-all
(gi-repo:load-by-name "Gtk" "Window")
(gi-repo:load-by-name "Gtk" "Box")

;; -------------------------------------------------------------------------------------------------

(define (new-buffer-view _)
  (let*((buffer (gi:make <GtkTextBuffer>)))
    (display ";;guigi-shell:new-buffer-view:\n")
    (display ";;    buffer = ")(write buffer)(newline)
    buffer))

(define (ensure-string str)
  (cond
   ((string? str) str)
   ((char? str) (make-string 1 str))
   ((not string) #f)
   (else (error "not a char string object" str)))
  )

(define (gtk-insert-into-window window str)
  (let ((impl (gtk-window-or-minibuffer-text-view window))
        (str (ensure-string str))
        )
    (when string
      (text-view:insert-at-cursor impl str))
    ))

(define (gtk-insert-into-buffer buffer str)
  (let ((impl (view buffer ed:=>buffer-view))
        (string (ensure-string str))
        )
    (text-buffer:insert-at-cursor impl str (string-length str))
    ))


;; -------------------------------------------------------------------------------------------------

(define (simply-show-error cancel)
  (lambda (err)
    (write err)
    (display (error-object-message err))
    (newline)
    (display (error-object-irritants err))
    (cancel)))

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
         ((eq? mod 'shift-mask)
          (loop (cdr mod-list) (char-upcase keyval)))
         ((or (eq? mod 'alt-mask) (eq? 'mod1-mask))
          (cons 'meta (loop (cdr mod-list) keyval)))
         ((eq? mod 'control-mask)
          (cons 'control (loop (cdr mod-list) keyval)))
         (else
          (format #t ";; unknown Gtk key modifier symbol \"~a\"\n" mod)
          (loop (cdr mod-list) keyval))))))))

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
      (integer->char norm-keyval))
     (else 
      (integer->char (keyval-to-unicode keyval))))))

(define (controller-key-pressed-handler winframe keyval keycode mods)
  ;; This event handler is only used when we use a input method
  ;; controller dispatch key events.
  (format #t ;;DEBUG
   ";; got key-event\n;; (keyval: #x~x\n;; keycode: #x~x\n;; modifiers: ~a)\n" ;;DEBUG
   keyval keycode modifiers);;DEBUG
  (values)
  )

(define (widget-key-press-event-handler winframe event)
  ;; This is the function called directly by the Gtk event handler, it
  ;; formats the key event as a keymap index and dispatches to the
  ;; ed:key-event-handler function.
  (let*-values
      (((val-ok   keyval)    (event:get-keyval event))
       ((code-ok  keycode)   (event:get-keycode event))
       ((state-ok modifiers) (event:get-state event))
       ((mod-bits)           (modifier-type->number modifiers))
       ((unicode)            (gtk-keyval-normalize keyval))
       )
    (format #t ;;DEBUG
     ";;key-event (keyval: #x~x keycode: #x~x modifiers: ~s mod-bits: ~s unicode: ~a)\n" ;;DEBUG
     keyval keycode modifiers mod-bits unicode) ;;DEBUG
    ;; Example output of the above format statement after pressing space bar:
    ;;     (key-event #x20 #x41 ())
    (cond
     ((and (= 0 mod-bits) (char=? unicode #\null))
      ;; Ignored because this is a key press of a modifier without an
      ;; accompanying printing character.
      #t)
     (else
      (let*((mod-list (modifier-type->list modifiers))
            (km-index (km:keymap-index (gtk-keymod->keymap-mod mod-list unicode)))
            )
        (pretty (print ";;keymap-index " (km:keymap-index-print km-index) (line-break)));;DEBUG
        (with-gtk3-backend (lambda () (ed:key-event-handler winframe km-index)))
        ;; Must return #t to prevent this event from propagating up
        ;; the widget tree to the parent TextView object and
        ;; triggering the default event handler.
        #t)
      ))))

(define (self-insert-command window string)
  ;; Used to parameterize *impl/self-insert-command*
  (gtk-insert-into-window window string))

(define (gtk-delete-char char-count save-to-killring)
  (let*((window (ed:selected-window))
        (impl (gtk-window-or-minibuffer-text-view window))
        )
    (text-view:delete-from-cursor impl (symbol->delete-type 'chars) char-count)
    (values)))

;; -------------------------------------------------------------------------------------------------

(define-record-type <window-metadata-type>
  ;; - BOX    :: layout containing the GtkTextView and the header line
  ;;
  ;; - TEXT   :: the GtkTextView
  ;;
  ;; - WINDOW :: the <WINFRAME-TYPE> to which this "window" belongs
  ;;
  ;; - FOCUS  :: an event handler object for the GtkTextView, must be
  ;;             kept here in order to prevent it from being deleted
  ;;             when the view loses focus.
  ;;------------------------------------------------------------------
  (make<window-metadata> box text window focus)
  window-metadata-type?
  (box     window-metadata-box       set!window-metadata-box)
  (text    window-metadata-text      set!window-metadata-text)
  (window  window-metadata-parent    set!window-metadata-parent)
  (focus   window-metadata-focus-in  set!window-metadata-focus-in))

(define =>window-metadata-box
  (record-unit-lens
   window-metadata-box
   set!window-metadata-box
   '=>window-metadata-box))

(define =>window-metadata-text
  (record-unit-lens
   window-metadata-text
   set!window-metadata-text
   '=>window-metadata-text))


(define (gtk-emacs-window window buffer)
  ;; Construct a so-called "Emacs Window", which is actually a text
  ;; view for displaying text buffers. The view is a box that displays
  ;; the text view, the mode line, and the header line.
  (let*((box
         (gi:make <GtkBox>
          #:orientation 'vertical
          #:homogeneous #f
          #:hexpand #t
          #:vexpand #t
          #:visible #t))
        (text
         (gi:make <GtkTextView>
          #:buffer buffer
          #:hexpand #t
          #:vexpand #t
          #:visible #t))
        (mode-line
         (view window
          ed:=>window-mode-line
          ed:=>line-display-view
          =>mode-line-metadata-widget))
        (focus-in-handler-obj (gi:make <signal> #:name "focus-in-event"))
        (this (make<window-metadata> box text window focus-in-handler-obj))
        (focus-in-handler-callback
         (lambda _
           (display ";;focus-in-event ")(write text)(newline);;DEBUG
           (gtk-select-window window)))
        )
    (gi:connect text focus-in-handler-obj focus-in-handler-callback)
    (display ";;gtk-emacs-window:\n");;DEBUG
    (display ";;       box = ")(write box)(newline);;DEBUG
    (display ";;      text = ")(write text)(newline);;DEBUG
    (display ";; mode-line = ")(write mode-line)(newline);;DEBUG
    (box:pack-start box text #t #t 0)
    (box:pack-start box mode-line #f #f 0)
    this))

(define (gtk-app-window-inner-layout window-view mode-line)
  ;; Defines a box that groups the window (the text view) with the mode line.
  (let ((box
         (gi:make <GtkBox>
          #:orientation 'vertical
          #:homogeneous #f
          #:hexpand #t
          #:vexpand #t
          #:visible #t)))
    (display ";;gtk-app-window-inner-layout:");;DEBUG
    (display " window-view: ")(write window-view);;DEBUG
    (display " mode-line-view: ")(write mode-line)(newline);;DEBUG
    (display ";;    box = ")(write box)(newline);;DEBUG
    (box:pack-start box window-view #t #t 0)
    (box:pack-start box mode-line #f #f 0)
    box))

(define (new-window-view window)
  ;; This procedure is called by ed:new-window via the
  ;; ed:*new-window-view* parameter.
  ;;
  ;; In Emacs nomencalture a "window" is a view for a buffer, so a
  ;; GtkTextView widget is the nearest thing to what Emacs considers
  ;; to be a window. The buffer assigned to this view is taken from
  ;; the ed:window-buffer value of the given window object, which is a
  ;; value of type ed:<window-type>.
  (let*((buffer
         (view window
           ed:=>window-buffer
           ed:=>buffer-view))
        )
    (gtk-emacs-window window buffer)))

;; -------------------------------------------------------------------------------------------------

(define-record-type <minibuffer-metadata-type>
  (make<minibuffer-metadata> flow-box prompt text-input text-buffer)
  minibuffer-metadata-type?
  (flow-box    minibuffer-metadata-layout      set!minibuffer-metadata-layout)
  (prompt      minibuffer-metadata-prompt      set!minibuffer-metadata-prompt)
  (text-input  minibuffer-metadata-text-input  set!minibuffer-metadata-text-input)
  (text-buffer minibuffer-metadata-buffer      set!minibuffer-metadata-buffer)
  )

(define =>minibuffer-metadata-layout
  (record-unit-lens
   minibuffer-metadata-layout
   set!minibuffer-metadata-layout
   '=>minibuffer-metadata-layout))

(define =>minibuffer-metadata-prompt
  (record-unit-lens
   minibuffer-metadata-prompt
   set!minibuffer-metadata-prompt
   '=>minibuffer-metadata-prompt))

(define =>minibuffer-metadata-text-input
  (record-unit-lens
   minibuffer-metadata-text-input
   set!minibuffer-metadata-text-input
   '=>minibuffer-metadata-text-input))

(define =>minibuffer-metadata-buffer
  (record-unit-lens
   minibuffer-metadata-buffer
   set!minibuffer-metadata-buffer
   '=>minibuffer-metatdata-buffer))

(define (new-minibuffer-view minibuffer)
  (let*((gtk-buffer (gi:make <GtkTextBuffer>))
        (text-view
         (gi:make <GtkTextView>
          #:buffer gtk-buffer
          #:hexpand #t
          #:vexpand #f
          #:visible #t
          ))
        (prompt
         (gi:make <GtkLabel>
          #:label " "
          #:hexpand #f
          #:vexpand #f
          #:visible #f))
        (flow-box
         (gi:make <GtkBox>
          #:orientation 'horizontal
          #:hexpand #t
          #:vexpand #f
          #:visible #t))
        )
    (display ";;new-minibuffer-view:\n");;DEBUG
    (display ";;    text-view = ")(write text-view)(newline);;DEBUG
    (display ";;    flow-box  = ")(write flow-box)(newline);;DEBUG
    (display ";;    prompt    = ")(write prompt)(newline);;DEBUG
    ;; Now insert into the flow-box. Note: in Gtk 3, insert to index
    ;; -1 will appends the element, int Gtk4, use flow-box:append
    ;; instead.
    ;;(flow-box:insert flow-box prompt -1)
    ;;(flow-box:insert flow-box text-view -1)
    (box:pack-start flow-box prompt #f #f 0)
    (box:pack-start flow-box text-view #t #t 0)
    (make<minibuffer-metadata>
     flow-box prompt text-view gtk-buffer)))

(define (gtk-minibuffer)
  (let ((view (new-minibuffer-view #f)))
    (minibuffer-metadata-layout view)))

(define (gtk-focus-minibuffer winframe prompt-text init-input)
  (display ";;gtk-focus-minibuffer: prompt-text: ")(write prompt-text);;DEBUG
  (display " init-input: ")(write init-input)(newline);;DEBUG
  (let*((echo-area (view winframe ed:=>winframe-echo-area ed:=>line-display-view))
        (bufview   (view winframe ed:=>winframe-minibuffer ed:=>window-view))
        (prompt    (view bufview =>minibuffer-metadata-prompt))
        (miniview  (view bufview =>minibuffer-metadata-layout))
        (minibuf   (view bufview =>minibuffer-metadata-text-input))
        )
    (display ";;    widget:hide echo-area: ")(write echo-area)(newline);;DEBUG
    (widget:hide echo-area)
    (when (string? prompt-text)
      (label:set-text prompt prompt-text))
    (when (string? init-input)
      (text-buffer:insert-at-cursor minibuf init-input)
      )
    (display ";;    widget:show-all miniview ")(write miniview)(newline);;DEBUG
    (widget:show miniview)
    (widget:show prompt)
    (widget:show minibuf)
    (display ";;    widget:grab-focus minibuf ")(write minibuf)(newline);;DEBUG
    (widget:grab-focus minibuf)
    ))

(define (gtk-get-minibuffer-text winframe)
  (display ";;gtk-get-minibuffer-text frame\n");;DEBUG
  (let*((minibuf
         (view winframe
          ed:=>winframe-minibuffer
          ed:=>window-view
          =>minibuffer-metadata-buffer))
        (start (gi:make <GtkTextIter>))
        (end   (gi:make <GtkTextIter>))
        (text
         (begin
           (text-buffer:get-iter-at-offset! minibuf start  0)
           (text-buffer:get-iter-at-offset! minibuf end -1)
           (text-buffer:get-text minibuf start end #t)
           ))
        )
    (display ";;    minibuf: ")(write minibuf)(newline);;DEBUG
    (display ";; gtk-get-minibuffer-text: got text -> ")(write text)(newline) ;;DEBUG
    text))

(define (gtk-exit-minibuffer winframe)
  (display "gtk-exit-minibuffer\n")
  (let*((echo-area (view winframe ed:=>winframe-echo-area ed:=>line-display-view))
        (bufview   (view winframe ed:=>winframe-minibuffer ed:=>window-view))
        (miniview  (view bufview =>minibuffer-metadata-layout))
        )
    (display ";;    widget:hide miniview ")(write miniview)(newline);;DEBUG
    (widget:hide miniview)
    (display ";;    widget:show echo-area  ")(write echo-area)(newline);;DEBUG
    (widget:show echo-area)
    ;; No need to re-focus the (selected-window), the editor calls
    ;; *impl/select-window* after this procedure is called.
  ))

;; -------------------------------------------------------------------------------------------------

(define (gtk-window-or-minibuffer-text-view window)
  ;; Windows and minibuffers are both editor windows of type
  ;; `ED:<WINDOW-TYPE>`, but they have different back-end
  ;; implementations ("views"). In this back-end, the view for a
  ;; minibuffer is of record type `<MINIBUFFER-METADATA-TYPE>`, where
  ;; an ordinary window is of type `<WINDOW-METADATA-TYPE>`. Both of
  ;; these records contain a GtkTextView, this procedure returns that
  ;; text view regardless of implementation metadata type.
  ;;------------------------------------------------------------------
  (let ((winview (view window ed:=>window-view)))
    (cond
     ((window-metadata-type? winview)
      (view winview =>window-metadata-text))
     ((minibuffer-metadata-type? winview)
      (view winview =>minibuffer-metadata-text-input))
     (else
      (error "window does not contain window or minibuffer metadata" winview window))
     )))

;; -------------------------------------------------------------------------------------------------

(define-record-type <mode-line-metadata-type>
  (make<mode-line-metadata> elem-vec widget)
  mode-line-metadata-type?
  (elem-vec  mode-line-metadata-elements  set!mode-line-metadata-elements)
  (widget    mode-line-metadata-widget    set!mode-line-metadata-widget)
  )

(define =>mode-line-metadata-widget
  (record-unit-lens
   mode-line-metadata-widget
   set!mode-line-metadata-widget
   '=>mode-line-metadata-widget))

(define (string->gtk-label str)
  (gi:make <GtkLabel>
   #:label str
   #:hexpand #t
   #:vexpand #f
   ;; #:monospace #t
   ;; #:editable #f
   #:visible #t))

(define (new-mode-line-metadata elems)
  (display ";;new-mode-line-metadata:\n")
  (let ((widget
         (gi:make <GtkBox>
          #:orientation 'horizontal
          #:homogeneous #f
          #:hexpand #f
          #:vexpand #f
          #:border-width 1
          #:visible #t))
        )
    (display ";;    widget = ")(write widget)(newline);;DEBUG
    (make<mode-line-metadata> #f widget)
    (gtk-mode-line-display-items widget elems '())
    widget))

(define (new-mode-line-view line-display)
  (make<mode-line-metadata> '() (new-mode-line-metadata '())))

(define (gtk-mode-line-pack-single box item props)
  (cond
   ((eq? #:separator item)
    (box:pack-start box (gi:make <GtkSeparator> #:orientation 'horizontal) #t #t 0))
   ((string? item) ; is a string, insert it into the box.
    ;;TODO: lookup how to insert text with properties into a Gtk text buffer
    (box:pack-start box (string->gtk-label item) #f #f 0))
   ((pair? item)
    (cond
     ((eq? #:propertize (car item))
      ;; is a propertized string, extract properties and insert it into the box.
      (cond
       ((pair? (cdr item)) ; item after leading "#:propertize" keyword exists
        (let ((item-list (cadr item))
              (props (append props (cddr item)))
              )
          (cond
           ((pair?   item-list) (gtk-mode-line-pack-items  box item-list props))
           ((string? item-list) (gtk-mode-line-pack-single box item-list props))
           (else (error "unknown mode line display item" item-list))
           )))
       (else ;; propertized item is empty, ignore it
        (values))
       ))
     (else ;; otherwise, it is just some list
      (gtk-mode-line-pack-items box item props))
     ))
   (else
    (error "unknown mode line display item" item))
   ))

(define (gtk-mode-line-pack-items box elems props)
  (let loop ((elems elems))
    (cond
     ((null? elems)
      (box:pack-start box (gi:make <GtkSeparator> #:orientation 'horizontal) #t #t 0)
      (values))
     (else
      (display ";;gtk-mode-line-pack-items:loop ")(write (car elems))(newline);;DEBUG
      (gtk-mode-line-pack-single box (car elems) props)
      (loop (cdr elems))))))

(define (gtk-mode-line-display-items parent-window metadata elems)
  (unless (null? elems)
    (let*((box (mode-line-metadata-widget metadata)))
      (gtk-mode-line-pack-items box elems '()))))

;; -------------------------------------------------------------------------------------------------

(define (gtk-display-line)
  (display ";;gtk-display-line:\n")
  (let*((buffer (gi:make <GtkTextBuffer>))
        (text-view
         (gi:make <GtkTextView>
                  #:buffer buffer
                  #:visible #t)))
    (display ";;    buffer = ")(write buffer)(newline);;DEBUG
    (display ";; text-view = ")(write text-view)(newline);;DEBUG
    text-view))

(define (new-display-area-view _parent)
  ;; The view for a mode-line is simply a text buffer
  (gtk-display-line))

(define (new-echo-area-view _parent)
  ;; The view for a mode-line is simply a text buffer
  (gtk-display-line))

(define new-header-line-view new-display-area-view)

(define (display-in-echo-area winframe str)
  (display ";;display-in-echo-area ")(write str)(newline);;DEBUG
  (let*((textview
         (view winframe
           ed:=>winframe-echo-area
           ed:=>line-display-view))
        )
    (text-view:insert-at-cursor textview str)))

(define (clear-echo-area winframe)
  (display ";;clear-echo-area\n");;DEBUG
  (let*((textview
         (view winframe
           ed:=>winframe-echo-area
           ed:=>line-display-view))
        (buffer (text-view:get-buffer textview))
        (start (text-buffer:get-start-iter! buffer (gi:make <GtkTextIter>)))
        (end   (text-buffer:get-end-iter!   buffer (gi:make <GtkTextIter>)))
        )
    ;; window-metadata-view-handle
    (text-buffer:delete buffer start end)))

;; -------------------------------------------------------------------------------------------------

(define-record-type <winframe-metadata-type>
  (make<winframe-metadata> window box echo-area minibuf key-events)
  winframe-metadata-type?
  (window     winframe-metadata-window      set!winframe-metadata-window)
  (box        winframe-metadata-box         set!winframe-metadata-box)
  (echo-area  winframe-metadata-echo-area   set!winframe-metadata-echo-area)
  (minibuf    winframe-metadata-minibuffer  set!winframe-metadata-minibuffer)
  (key-events winframe-metadata-key-press   set!winframe-metadata-key-press)
  )

(define *gtk-application* (make-parameter #f))

(define (gtk-application-window layout application)
  (display "gtk-application-window: ")(write layout)(newline)
  (gi:make <GtkApplicationWindow>
   #:application application
   #:default-width  (* 8 8 5 4)
   #:default-height (* 6 6 5 4)
   #:hexpand #f
   #:vexpand #f
   #:title "Schemacs-Shell"
   #:child layout
   #:visible #t))

(define (new-winframe-view winframe window)
  ;; Create a new winframe-like and the Gtk application window to make it
  ;; usable in a GtkApplication.
  (display ";;new-winframe-view:\n");;DEBUG
  (let*((use-key-control #f) ;; I'm not sure which mechanism to use yet
        (editor-state (ed:winframe-parent-editor winframe))
        (window-view  (view window ed:=>window-view))
        (win-view-box (window-metadata-box window-view))
        (_ ;;DEBUG
         (begin ;;DEBUG
           (display ";;    window-view box: ")(write win-view-box)(newline);;DEBUG
           (display ";;    window-view text: ")(write (window-metadata-text window-view))(newline);;DEBUG
           ));;DEBUG
        (buffer       (view window ed:=>window-buffer))
        (echo-area    (view winframe ed:=>winframe-echo-area ed:=>line-display-view))
        (minibuffer   (view winframe ed:=>winframe-minibuffer ed:=>window-view))
        (minibuf-view (view minibuffer =>minibuffer-metadata-layout))
        (bufview      (view buffer ed:=>buffer-view));;DEBUG
        (setup-params
         (lambda (thunk)
           (parameterize
               ((*impl/selected-frame*  winframe)
                (*impl/current-buffer*  buffer)
                )
             (thunk))
           ))
        (outer-layout
         (gi:make <GtkBox>
          #:orientation 'vertical
          #:homogeneous #f
          #:hexpand #f
          #:vexpand #f
          #:visible #t))
        (window-handle
         (gtk-application-window
          outer-layout
          (view (ed:winframe-parent-editor winframe) ed:=>editor-view)))
        (_ (begin
             (display ";;    win-view-box = ")(write win-view-box)(newline);;DEBUG
             (display ";;    outer-layout = ")(write outer-layout)(newline);;DEBUG
             (display ";;    pack win-view-box = ")(write win-view-box)(newline);;DEBUG
             (box:pack-start outer-layout win-view-box #t #t 0)
             (display ";;    pack echo-area = ")(write echo-area)(newline);;DEBUG
             (box:pack-start outer-layout echo-area #f #f 0)
             (display ";;    pack minibuf-view = ")(write minibuf-view)(newline);;DEBUG
             (box:pack-start outer-layout minibuf-view #f #f 0)
             (display ";;    hide minibuf-view ")(write minibuf-view)(newline);;DEBUG
             (widget:hide minibuf-view)
             (display ";;    window-handle = ")(write window-handle)(newline);;DEBUG
             #f))
        (key-control (if use-key-control (gi:make <GtkEventControllerKey>) #f))
        (key-press-handler-obj (gi:make <signal> #:name "key-press-event"))
        (key-press-handler-proc
         (cond
          (use-key-control
           (lambda (controller keyval keycode mods)
             (setup-params
              (lambda () (controller-key-pressed-handler winframe keyval keycode mods)))))
          (else
           (lambda (_view event)
             ;; ;; NOTE: GI signals are defined as GOOPS generic methods on GI
             ;; ;; classes (which are also GOOPS classes). If you are not sure what
             ;; ;; arguments a method takes, use the 'generic-function-methods'
             ;; ;; function (imported from (oop goops)) to inspect the arguments of
             ;; ;; each method associated with the generic method. There are usually
             ;; ;; only one such method.
             ;; ;;
             ;; ;; NOTE also many Gtk signal functions (which are defined as
             ;; ;; C-language functions) take a final 'gpointer' function to pass
             ;; ;; arbitrary user data. Guile-GI seems to drop this argument
             ;; ;; (perhaps it is used internally, or not at all), so do not pass
             ;; ;; #f or any additional paramaters to a signal method to satisfy
             ;; ;; that final 'gpointer' argument, it will probably trigger a
             ;; ;; Scheme exception.
             ;; ;; ------------------------------------------------------------
             ;; (let ((imcontext (event-controller-key:get-im-context key-control)))
             ;;   (format #t "key-event ~a ~a ~a\n" keyval keycode modifiers);;DEBUG
             ;;   (when imcontext
             ;;     (commit imcontext (list->string (list (integer->char keyval)))))
             ;;   (unless imcontext ;;DEBUG
             ;;     (format #t "#;(imcontext not defined)"));;DEBUG
             ;;  )
             (setup-params
              (lambda () (widget-key-press-event-handler winframe event)))))))
        )
    (set! (widget:events window-handle)
      (list->event-mask
       '(key-press-mask
         focus-change-mask)))
    (display ";;    window-handle = ")(write window-handle)(newline);;DEBUG
    (if use-key-control
        (begin
          (gi:connect key-control event-controller-key:key-pressed key-press-handler-proc)
          (set! (event-controller:widget key-control) window-handle))
        (gi:connect window-handle key-press-handler-obj key-press-handler-proc))
    (gtk-select-window window)
    (application:add-window (view editor-state ed:=>editor-view) window-handle)
    (widget:set-visible window-handle #t)
    (make<winframe-metadata> window-handle outer-layout echo-area minibuffer key-press-handler-obj)))

(define (gtk-select-window window)
  (let*((winview (view window ed:=>window-view))
        (winframe (ed:window-parent-frame window))
        (impl
         (cond
          ((window-metadata-type? winview)
           (window-metadata-text winview))
          ((minibuffer-metadata-type? winview)
           (minibuffer-metadata-text-input winview))
          (else
           (error "window does not contain window or minibuffer metadata" winview window))
          ))
        )
    (widget:grab-focus impl)
    (lens-set window winframe ed:=>winframe-window)
    ;; Now we MUST return false here, telling Gtk that the event is
    ;; not fully handled yet and should propagate up the widget
    ;; tree. I believe this is because if the event is not propagated
    ;; upward, the widget can end up in an inconsistent state, and
    ;; this results in a crash.
    #f))

;; -------------------------------------------------------------------------------------------------

(define (make-gtk3-environment env)
  (case-lambda
    (()
     (display "get gtk3-environment: ")(write env)(newline);;DEBUG
     env)
    ((new-env)
     (display "set gtk2-environment: ")(write new-env)(newline);;DEBUG
     (set! env new-env) new-env)))

(define *gtk3-environment*
  (make-parameter (make-gtk3-environment #f)))

(define (gtk3-environment-closure . args)
  ;; This function is not exposed because passing arguments can change
  ;; the value stored within the global variable. The public API for
  ;; this function is `GTK3-ENVIRONMENT`, defined below, which takes
  ;; zero arguments and returns the current environment. This API is
  ;; exposed by way of the `THE-ENVIRONMENT` parameter defined in the
  ;; `(SCHEMACS EVAL)` library.
  (let ((getenv (*gtk3-environment*)))
    (display "to getenv ")(write getenv)(display " apply args: ")(display args)(newline);;DEBUG
    (apply getenv args)))

(define (gtk3-environment)
  ;; This value is set when `ENABLED/LAUNCH-GUI` is called, the
  ;; argument passed to that function is set here and is the value
  ;; returned by `THE-ENVIRONMENT`, which for the environment in which
  ;; the Schemacs interpreter is running is a reference to itself. It
  ;; is assumed that `ENABLED/LAUNCH-GUI` is only evaluated once per
  ;; envirnoment. If this symbol is bound to something else at any
  ;; other time outside of `ENABLED/LAUNCH-GUI`, it may result in race
  ;; conditions, especially as event handlers occur in parallel threads.
  (gtk3-environment-closure)
  )

(define guile-eval-string
  (case-lambda
    ((form) (guile-eval-string form (gtk3-environment)))
    ((form env)
     (display "guile-eval-string: ")(write form)(newline);;DEBUG
     (display "  env = ")(write env)(newline);;DEBUG
     (guile:eval-string form #:module env)
     )
    ))

(define (with-gtk3-backend proc . args)
  (parameterize
      ((*impl/new-buffer-view*          new-buffer-view)
       (*impl/new-window-view*          new-window-view)
       (*impl/new-winframe-view*        new-winframe-view)
       (*impl/new-minibuffer-view*      new-minibuffer-view)
       (*impl/new-echo-area-view*       new-echo-area-view)
       (*impl/new-mode-line-view*       new-mode-line-view)
       (*impl/mode-line-display-items*  gtk-mode-line-display-items)
       (*impl/new-header-line-view*     new-header-line-view)
       (*impl/self-insert-command*      self-insert-command)
       (*impl/insert-into-buffer*       gtk-insert-into-buffer)
       (*impl/delete-char*              gtk-delete-char)
       (*impl/display-in-echo-area*     display-in-echo-area)
       (*impl/clear-echo-area*          clear-echo-area)
       (*impl/get-minibuffer-text*      gtk-get-minibuffer-text)
       (*impl/focus-minibuffer*         gtk-focus-minibuffer)
       (*impl/exit-minibuffer*          gtk-exit-minibuffer)
       (*impl/select-window*            gtk-select-window)
       (*impl/is-graphical-display?*    #t)
       (schemacs:*the-environment-procedure*  gtk3-environment)
       (schemacs:*eval-string-procedure*  guile-eval-string)
       )
    (apply proc args)))

(define disabled/launch-gui
  (lambda _ (display "Error: `launch-gui' has alaredy been called once" (current-error-port))
     ))

(define (enabled/launch-gui start-repl? env)
  ;; This procedure initializes the GtkApplication itself and installs
  ;; a callback handler which is called when the application is
  ;; activated. The callback handler then initializes the editor state
  ;; which sets up the rest of the GUI. The callback handler never
  ;; returns until the GUI is shut down. This procedure takes an
  ;; environment "ENV" craeted by the Scheme standard
  ;; "INTERACTIVE-ENVIRONMENT" procedure defined in the "(scheme repl)"
  ;; built-in library.
  (let*((app
         (gi:make <GtkApplication>
          #:application-id "schemacs-shell.lambda-libre.org"))
        (_ (display ";;application:register\n"))
        (success (application:register? app #f))
        (with-gtk3-backend
         (lambda (proc . args)
           (with-gtk3-backend
            (lambda (args)
              (parameterize
                  ;; Now that the parameters that initailize the
                  ;; application have been used, remove the parameters
                  ;; that would cause problems if they were called again
                  ;; while the application is running.
                  ((*impl/new-editor-view* (lambda _ app))
                   (*launch-gui* disabled/launch-gui)
                   )
                (apply proc args)))
            args)))
        (the-repl-thread
         (if start-repl?
             (make-repl-thread gtk3-environment with-gtk3-backend)
             #f))
        )
    (cond
     (success
      (gtk3-environment-closure env) ;; must set this before calling `WITH-GTK3-BACKEND`
      (display ";;connect signal application:activate\n")
      (gi:connect
       app gio:activate
       (lambda (app)
         ;; This code is called once the event loop is started by the
         ;; call to (application:run).
         (with-gtk3-backend
          (lambda ()
            (ed:new-editor)
            (when the-repl-thread (thread-start! the-repl-thread))
            ))))
      (display ";;application:run\n")
      (application:run app (command-line))
      )
     (else (display ";;ERROR: (application:register?) failed\n"))
     )))

(define *launch-gui* (make-parameter enabled/launch-gui))

(define (launch-gui start-repl? env) ((*launch-gui*) start-repl? env))


;; TODO:
;;
;;  1. change the *eval-string-procedure* implementation
;;     (guile-eval-string) to make use of the interactive
;;     REPL procedures and environment.
;;
;;  2. Implement the schemacs:*eval-procedure* to also use the
;;     interactive REPL procedures when no environment is given.

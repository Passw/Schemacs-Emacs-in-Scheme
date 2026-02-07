(define-library (schemacs apps debugui)
  ;; The purpose of this app is to provide a minimal Emacs-like clone
  ;; that can serve as a test app for anyone who plans to implement a
  ;; `(schemacs ui)` back-end. The `main` procedure of this library is
  ;; a `<div-monad-type>` which produces a `div` node tree. The user
  ;; interface of this `div` node tree is a 2-pane window frame
  ;; consisting of a left and right text view, and along the bottom of
  ;; the window frame is an echo area for displaying messages and also
  ;; serving as a minibuffer. The layout is all of a fixed size and
  ;; never changes, so it is relatively easy to implement, but also
  ;; provides enough Emacs-like functionality to provide an
  ;; interactive programming environment that will help you to develop
  ;; a more complete `(schemacs ui)` back-end.
  ;;------------------------------------------------------------------

  (import
    (scheme base)
    (scheme case-lambda)
    (only (scheme write) display write)
    ;;---------------------------------
    (only (schemacs hash-table)
          make-hash-table  hash-table-set!  hash-table-ref/default
          )
    (prefix (schemacs keymap) km:)
    (only (schemacs pretty)
          pretty  pp-type?  print  line-break  qstr
          )
    (only (schemacs eval) eval-string)
    (prefix (schemacs ui text-buffer) impl/)
    (only (schemacs editor command)
          new-command  command-type?
          run-command  apply-command
          )
    (only (schemacs ui rectangle)
          point2D  point2D-x  point2D-y  print-rect2D
          rect2D-type?  rect2D  rect2D-point  rect2D-size
          size2D-type?  size2D  size2D-width  size2D-height
          )
    (only (schemacs ui)
          state-var  get-var  properties
          div  div-monad-type?  run-div-monad
          div-type?  div-record-type?  div-from-var?
          div-view-type  div-parent  div-content  div-on-update
          =>div-rect*!  =>div-widget*!  =>div-properties*!
          =>div-on-delete*!  =>div-on-update*!
          div-select  selector  top-div-select
          div-set-focus!
          div-prop-lookup  prop-lookup
          div-grid-type?  for-each-div-grid
          div-pack-type?  for-each-div-pack
          div-space-type?  div-space  div-space-elements
          div-space-inner-align  div-space-outer-align
          for-each-floater  floater floater-div  floater-rect
          div-pack-subdivs  div-pack-subdiv-sizes  div-pack-from
          div-pack-orientation  div-pack-flags  wrapping
          cut-vertical  cut-horizontal
          from-start  from-end  expand  enclose
          use-vars-type?   use-vars-value
          div-event-handler  div-run-update
          div-delete  =>div-on-delete*!
          push-button  check-box  text-input  radio-group
          labeled-group  text-editor  canvas  composite
          tiled-windows
          )
    (prefix (schemacs apps emacs) ed:)
    (prefix (schemacs keymap) km:)
    (only (schemacs ui text-buffer) *text-load-buffer-size*)
    (only (schemacs pretty) display-lines pretty print line-break qstr)
    (only (scheme case-lambda) case-lambda)
    )

  (export
   main  list-debug-commands  insert
   emergency-reset-debugger
   ;;---------------- commands ----------------;;
   self-insert-command  delete-char  delete-backward-char
   eval-expression  get-buffer  keyboard-quit
   )

  (begin
    ;; -------------------- Global debugger state --------------------
    (define messages-buffer-hstr  "*Messages*")
    (define scratch-buffer-hstr   "*scratch*")
    (define minibuffer-hstr       "*Minibuffer*")
    (define messages-buffer          #f)
    (define buffer-list              #f)

    (define minibuffer               #f)
    (define last-focused             'left)
    (define minibuffer-cont          '())
    (define minibuffer-prompt-length 0)

    (define left-content-state-var   #f)
    (define right-content-state-var  #f)
    (define left-info-state-var      #f)
    (define right-info-state-var     #f)

    (define current-buffer (make-parameter #f))

    (define escape-counter 0)
    ;; ^ The escape counter counts how many times the escape button
    ;; has been pressed. If it has been pressed 3 times, an emergency
    ;; reset occurs where the program does it's best to restore the
    ;; event handlers and `div` tree to it's initial state.

    ;; -------------------- Global Window layout ---------------------
    (define text-height   16) ;; == based on my system's console font size
    (define window-width  1080)                ;; == 6*180
    (define window-height (- 630 text-height)) ;; ==  (3/2)*180
    (define border-size   2)

    ;;----------------------------------------------------------------

    (define-record-type <debug-buffer-type>
      (make<debug-buffer> hstr keys kmst impl)
      debug-buffer-type?
      (hstr  buffer-string-handle  set!buffer-string-handle)
      (keys  buffer-local-keymap   set!buffer-local-keymap)
      (kmst  buffer-keymap-state   set!buffer-keymap-state)
      (impl  buffer-impl           set!buffer-impl)
      )

    (define (%new-buffer string-handle)
      (let ((buf (hash-table-ref/default buffer-list string-handle #f)))
        (or buf
            (let ((buf
                   (make<debug-buffer>
                    string-handle *default-keymap* #f
                    (impl/new-buffer)
                    )))
              (hash-table-set! buffer-list string-handle buf)
              buf
              ))))

    (define (message str)
      (let ((str
             (cond
              ((pp-type? str) (pretty #f str))
              ((string? str) str)
              (else (error "not a string or pretty-printer" str))
              ))
            (msgbuf (buffer-impl messages-buffer))
            )
        (impl/insert-string msgbuf str)
        (impl/insert-string msgbuf "\n")
        (clear-minibuffer)
        (impl/insert-string (buffer-impl minibuffer) str)
        #t))

    ;;----------------------------------------------------------------
    ;; Commands

    (define (debug-cancel-action)
      (display "; cancel\n")
      ;;TODO
      #t)

    (define (emergency-reset-debugger)
      (display ";------ Schemacs Debugger: emergency reset ------;\n")
      (set! escape-counter 0)
      (debug-cancel-action)
      ;; TODO
      #t)

    (define keyboard-quit
      (new-command
       "keyboard-quit"
       (lambda () (debug-cancel-action))
       debug-cancel-action
       "Signal a `quit` condition."
       ))

    (define (uarg->integer dflt uarg)
      (cond
       ((eq? #f uarg) dflt)
       ((eq? #t uarg) 4)
       ((integer? uarg) uarg)
       ((number? uarg) (round uarg))
       (else (error "U-argument cannot be cast to integer" uarg))
       ))

    (define key-event-self-insert
      (case-lambda
        ((uarg)
         (let*((buf        (current-buffer))
               (state      (buffer-keymap-state buf))
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
               (impl/insert-char (buffer-impl buf) c)
               (loop (- uarg 1))
               )
              (else #f)
              ))))))

    (define self-insert-command
      (new-command
       "self-insert-command"
       (lambda () (key-event-self-insert #f))
       key-event-self-insert
       "Takes a key event and converts it to a character based on the
 keyboard key that was pressed, then inserts that character into
 the current buffer of the current window."
       ))

    (define (insert string-or-char)
      (let ((buf (current-buffer)))
        (cond
         ((char? string-or-char) (impl/insert-char (buffer-impl buf) string-or-char))
         ((string? string-or-char) (impl/insert-string (buffer-impl buf) string-or-char))
         (error "argument must be a string or char" string-or-char)
         )))

    (define delete-char
      (new-command
       "delete-char"
       (lambda () (apply-command delete-char 1 #f))
       (lambda args (impl/delete-from-cursor (buffer-impl (current-buffer)) 1))
       "Takes 2 arguments, an integer number of characters to delete after
 the cursor (negative integers delete before the cursor), followed
 by a boolean indicating whether the deleted characters should be
 copied to the \"kill ring\"."
       ))

    (define delete-backward-char
      (new-command
       "delete-backward-char"
       (lambda () (apply-command delete-char -1 #f))
       (lambda args (impl/delete-from-cursor (buffer-impl (current-buffer)) -1))
       "Delete the previous N characters (following if N is negative).
 If Transient Mark mode is enabled, the mark is active, and N is 1,
 delete the text in the region and deactivate the mark instead.
 To disable this, set option 'delete-active-region' to nil."
       ))

    (define eval-expression-string
      ;; This is the Scheme built-in implementation for the Emacs Lisp
      ;; `EVAL-EXPRESSION` API. You can pass an optional string to be read
      ;; and evaluated, with no arguments `SIMPLE-READ-MINIBUFFER` is used
      ;; to prompt the end user for an input string to evaluate.
      (case-lambda
        (() (eval-expression-string #f))
        ((input-string)
         (let*((input-string
                (cond
                 ((string? input-string) input-string)
                 ((not input-string)
                  (simple-read-minibuffer "Eval: ")
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
             (display-in-echo-area output-string)
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

    (define (clear-minibuffer)
      (impl/delete-range (buffer-impl minibuffer) 0 -1)
      )

    (define (display-in-echo-area str)
      (clear-minibuffer)
      (impl/insert-string (buffer-impl minibuffer) str)
      (impl/insert-string (buffer-impl messages-buffer) str)
      )

    (define (debug-div-select . args)
      (apply top-div-select 'top args)
      )

    (define (focus-minibuffer)
      (div-set-focus! (debug-div-select 'minibuffer))
      )

    (define (simple-read-minibuffer prompt)
      (clear-minibuffer)
      (focus-minibuffer)
      (parameterize ((current-buffer minibuffer))
        (insert prompt)
        (insert ": ")
        (set! minibuffer-prompt-length
          (+ (string-length ": ")
             (string-length prompt)
             ))
        (call/cc
         (lambda (wait-prompt)
           (call/cc
            (lambda (resume-prompt)
              (set! minibuffer-cont (cons resume-prompt minibuffer-cont))
              (wait-prompt #t)
              ))
           (set! minibuffer-cont (cdr minibuffer-cont))
           (let ((result
                  (impl/copy-string
                   minibuffer minibuffer-prompt-length -1
                   )))
             (clear-minibuffer)
             (set! minibuffer-prompt-length 0)
             result
             )))))

    (define (minibuffer-prompt-resume)
      (cond
       ((null? minibuffer-cont) (error "no minibuffer prompt"))
       (else ((car minibuffer-cont)))
       ))

    (define exit-minibuffer
      (new-command
       "exit-minibuffer"
       minibuffer-prompt-resume
       minibuffer-prompt-resume
       "Terminate this minibuffer argument."
       ))

    (define *command-list*
      (list
       keyboard-quit
       self-insert-command
       delete-char
       delete-backward-char
       eval-expression
       exit-minibuffer
       ))

    (define (list-debug-commands) *command-list*)

    ;;----------------------------------------------------------------
    ;; Keymaps

    (define self-insert-layer
      ;; This is the `SELF-INSERT-COMMAND` wrapped in a
      ;; `KM:<KEYMAP-INDEX-PREDICATE-TYPE>` layer. When defining keymaps,
      ;; you can use use this as a layer to catch all key events that
      ;; translate to self-inserting characters. Control characters are
      ;; not considered self-inserting by this layer.
      ;;------------------------------------------------------------------
      (km:new-self-insert-keymap-layer #f (lambda (c) self-insert-command) (lambda () #f))
      )

    (define *default-keymap*
      (km:keymap
       '*default-keymap*
       (km:alist->keymap-layer
        `(((#\backspace) . ,delete-backward-char)
          ((#\delete)    . ,delete-char)
          ((meta #\:)    . ,eval-expression)
          ))
       self-insert-layer
       ))

    ;;----------------------------------------------------------------

    (define current-event-div (make-parameter #f))
    ;; ^ Keeps a reference to the `div` node which trapped the
    ;; most recent key or mouse event.

    (define key-event-buffer (make-parameter #f))
    ;; ^ Keeps a reference to the `<debug-buffer-type>` which
    ;; trapped the most recent key event.

    (define (dispatch-action o buf key-index action)
      (parameterize
          ((current-event-div  o)
           (key-event-buffer   buf)
           (current-buffer     buf)
           (*main-procedure*   repl-main)
           )
        (cond
         ((command-type? action) (run-command action) #t)
         ((procedure? action) (action) #t)
         (else (error "not a command" action))
         )))

    (define (key-event-handler buf)
      (lambda (o key-path)
        (cond
         ((equal? (km:keymap-index->list key-path) '(ctrl #\g))
          (cond
           ((< escape-counter 3)
            (set! escape-counter (+ 1 escape-counter))
            (debug-cancel-action)
            )
           (else (emergency-reset-debugger))
           ))
         (else
          (set! escape-counter 0)
          (cond
           ((debug-buffer-type? buf)
            (let*((keymap (buffer-local-keymap buf))
                  (km-state (buffer-keymap-state buf))
                  )
              (unless km-state
                (set! km-state (km:new-modal-lookup-state keymap))
                (set!buffer-keymap-state buf km-state)
                )
              (km:modal-lookup-state-step!
               km-state key-path
               (lambda (key-path action) (dispatch-action o buf key-path action) #t)
               (lambda (key-path subkeymap) #t) ;;TODO: show keys bound to other commands
               (lambda (key-path) (message (print (qstr key-path) " is undefined")) #t)
               )))
           (else
            (error "current buffer value unrecognized type" buf)
            ))))))

    (define (construct-gui . args)
      (unless buffer-list
        (set! buffer-list (make-hash-table))
        )
      (unless messages-buffer
        (set! messages-buffer (%new-buffer messages-buffer-hstr))
        )
      (unless minibuffer
        (set! minibuffer (%new-buffer minibuffer-hstr))
        )
      (let*((scratch-buffer (%new-buffer scratch-buffer-hstr))
            ;; These define the framing of the elements on screen
            ;; but not the actual position of the widgets in the
            ;; space, because these widgets need to be offset by
            ;; the borders. When wrapping text, the border size
            ;; is added to the text size.
            (origin  0)
            (w0  window-width)
            (h0  window-height)
            (both-borders     (* 2 border-size))
            (right-h-offset   (/ w0 2))
            (minibuf-height   (+ text-height both-borders))
            (minibuf-width    w0)
            (minibuf-v-offset (- h0 minibuf-height))
            (info-width       (/ w0 2))
            (info-height      minibuf-height)
            (info-v-offset    (- minibuf-v-offset info-height))
            (content-width    info-width)
            (content-height   (- h0 info-v-offset))
            )
        (define (make-content h-offset sel buf)
          (floater
           (rect2D
            (+ h-offset        border-size)
            (+ origin          border-size)
            (- content-width   both-borders)
            (- content-height  both-borders)
            )
           (text-editor
            (selector sel)
            (properties 'on-key-event: (key-event-handler buf))
            (buffer-impl buf)
            )))
        (define (make-info h-offset sel message)
          (floater
           (rect2D
            (+ h-offset       border-size)
            (+ info-v-offset  border-size)
            (- info-width     both-borders)
            (- info-height    both-borders)
            )
           (div (selector sel) message)
           ))
        (unless left-content-state-var
          (set! left-content-state-var
            (state-var (make-content origin 'left scratch-buffer))
            ))
        (unless left-info-state-var
          (set! left-info-state-var
            (state-var
             (make-info origin 'left-info scratch-buffer-hstr)
             )))
        (unless right-content-state-var
          (set! right-content-state-var
            (state-var
             (make-content right-h-offset 'right messages-buffer)
             )))
        (unless right-info-state-var
          (set! right-info-state-var
            (state-var
             (make-info right-h-offset 'right-info messages-buffer-hstr)
             )))
        (floater
         (rect2D 0 0 w0 h0)
         (div-space
          (selector 'top)
          (properties 'title: "Schemacs Debugger")
          (get-var left-content-state-var)
          (get-var left-info-state-var)
          (get-var right-content-state-var)
          (get-var right-info-state-var)
          (floater
           (rect2D
            (+ origin            border-size)
            (+ minibuf-v-offset  border-size)
            (- minibuf-width     both-borders)
            (- minibuf-height    both-borders)
            )
           (text-editor
            (selector 'minibuffer)
            (properties 'on-key-event: (key-event-handler minibuffer))
            (buffer-impl minibuffer)
            ))))))

    (define *main-procedure* (make-parameter construct-gui))

    (define (repl-main . args)
      (display "; Hello, world!\n")
      ;; TODO
      )

    (define (main . args)
      (apply (*main-procedure*) args)
      )))

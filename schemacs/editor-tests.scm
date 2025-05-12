(import
  (scheme base)
  (scheme lazy)
  (srfi srfi-64) ;; test suite
  (gypsum sim-agent)
  (only (gypsum editor command)
        new-command show-command command-procedure)
  (only (gypsum lens) view)
  (prefix (gypsum editor) ed:)
  (prefix (gypsum keymap) km:)
  (only (gypsum lens) view =>hash-key)
  )

;; Stub functions ----------------------------------------------------------------------------------

(define (new-buffer-view buffer)
  (new-sim-agent "buffer-view" (open-output-string)))

(define (new-window-view window)
  (new-sim-agent "window-view" #f))

(define (new-winframe-view frame window)
  (let ((this (new-sim-agent "winframe-view" #f)))
    (sim-agent-log
     this "(test:new-winframe-view) ;; ~a\n"
     (sim-agent->string (view window ed:=>window-view) #f))
    this))

(define (buffer-insert buffer str)
  (let ((buffer (view buffer ed:=>buffer-view)))
    (sim-agent-log buffer "(buffer-insert ~s)\n" str)))

(define test:self-insert-command
  (new-command
   "self-insert-command"
   (lambda (window str)
     (buffer-insert (view window ed:=>window-buffer) str)
     (sim-agent-log
      (view window ed:=>window-view)
      "(test:self-insert-command) ;;~s\n" str))
   "Convert the key-press event argument to a character and inserts it into the current buffer."))

(define (unbound-key-index window key-path)
  (sim-agent-log
   (view window ed:=>window-view)
   "(test:unbound-key-index) ;; ~a\n"
   (km:keymap-index->list (force key-path))))

(define (waiting-key-index window key-path keymap)
  (sim-agent-log
   (view window ed:=>window-view)
   "(test:waiting-key-index) ;; ~a\n"
   (km:keymap-index->list (force key-path))))

(define (dispatch-key-event window key-path action)
  (let ((log
         (lambda (msg . args)
           (apply sim-agent-log (view window ed:=>window-view) msg args))))
    (log
     "(test:dispatch-key-event ~a ~a)\n"
     (km:keymap-index->list (force key-path))
     (show-command action))
    ((command-procedure action) window key-path)))

;; Tests -------------------------------------------------------------------------------------------

(test-begin "gypsum_editor")

(define (run-test proc . eds)
  (parameterize
      ((ed:*impl/new-buffer-view* new-buffer-view)
       (ed:*impl/new-window-view* new-window-view)
       (ed:*impl/new-winframe-view* new-winframe-view)
       (ed:*unbound-key-index* unbound-key-index)
       (ed:*waiting-key-index* waiting-key-index)
       (ed:*dispatch-key-event* dispatch-key-event)
       (ed:*impl/self-insert-command* test:self-insert-command)
       )
    (cond
     ((null? eds) (proc (ed:new-editor)))
     (else
      (let loop ((eds eds))
        (cond
         ((null? eds) (values))
         (else
          (proc (car eds))
          (loop (cdr eds)))))))))

(define (test-key-event-handler)
  (run-test
   (lambda (ed)
     (let ((win (view ed ed:=>editor-window-table (=>hash-key "main"))))
       (test-assert (ed:window-type? win))
       (ed:key-event-handler win (km:keymap-index '(#\space)))))))

(define ed (run-test (lambda (ed) ed)))

(run-test
 (lambda (ed)
   (sim-agent-reset-id-gen)
   (let*((buf (view ed ed:=>editor-buffer-table   (=>hash-key "*Messages*")))
         (fr  (view ed ed:=>editor-winframe-table (=>hash-key "GUIgi-Shell")))
         (win (view ed ed:=>editor-window-table   (=>hash-key "main")))
         )
     (test-assert (ed:buffer-type? buf))
     (test-assert (ed:window-type? win))
     (test-assert (ed:winframe-type?  fr))
     (ed:key-event-handler win (km:keymap-index '(#\space)))
     (test-assert
         (equal? "" (sim-agent-logs-to-string (view buf ed:=>buffer-view) #f)))
     (test-assert
         (equal?
          "(test:new-winframe-view) ;; (window-view 1)\n\n"
          (sim-agent-logs-to-string (view fr ed:=>winframe-view)  #f)))
     (test-assert
         (equal?
          "(test:self-insert-command) ;;( )\n"
          (sim-agent-logs-to-string (view win ed:=>window-view) #f)))
     )
   ed))

(when #f
  ;; This is a print-only test that does the same thing as the above
  ;; test. Evaluate the inside of this "when" block interactively to
  ;; see the expected results.
  (run-test
   (lambda (ed)
     (sim-agent-reset-id-gen)
     (let*((buf (view ed ed:=>editor-buffer-table   (=>hash-key "*Messages*")))
           (fr  (view ed ed:=>editor-winframe-table (=>hash-key "GUIgi-Shell")))
           (win (view ed ed:=>editor-window-table   (=>hash-key "main")))
           )
       (ed:key-event-handler win (km:keymap-index '(#\space)))
       (display "ed:key-event-handler\nexpecting: #f\n   actual: ")
       (write (sim-agent-logs-to-string (view buf ed:=>buffer-view) #f))
       (newline)
       (display "test:new-winframe-view\nexpecting: \"(test:new-winframe-view) ;; (window-view 1)\\n\\n\"\n   actual: ")
       (write (sim-agent-logs-to-string (view fr ed:=>winframe-view)  #f))
       (newline)
       (display "test:self-insert-command:\nexpecting: \"(test:self-insert-command) ;;( )\\n\"\"\n   actual: ")
       (write (sim-agent-logs-to-string (view win ed:=>window-view) #f))
       (newline))))
  ;;
  )

(test-end "gypsum_editor")

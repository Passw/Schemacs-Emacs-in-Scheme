
(gi:use-typelibs
 (("GLib" "2.0")
  #:prefix glib:))

(gi-repo:load-by-name "GLib" "PRIORITY_DEFAULT")
(gi-repo:load-by-name "GLib" "timeout_add")

;;--------------------------------------------------------------------------------------------------

(define repl-welcome-message
  ;; A parameter containing a string that is displayed when the REPL
  ;; thread begins.
  ;;------------------------------------------------------------------
  (make-parameter
   ";;========================= Gtk3 REPL =========================\n\
    ;; Your controlling terminal is now attached to a REPL running\n\
    ;; inside of a Gtk3 event loop.\n\
    ;;=============================================================\n\
    \n"
   ))

(define (async-apply-in-main-thread proc . args)
  ;; Takes a procedure `PROC` and some arguments `ARGS` and applies
  ;; `ARGS` to the `PROC` in the Gtk main event loop. This procedure
  ;; returns a promise-like procedure which can be evaluated to await
  ;; results from the `PROC` procedure. All values returned by the
  ;; procedure are returned in a list via the promise. If the
  ;; procedure raises an exception, an error object is returned by way
  ;; of the promise.
  ;;------------------------------------------------------------------
  (let ((outbox (new-channel)))
    (timeout-add
     PRIORITY_DEFAULT 1
     (lambda _
       (call/cc
        (lambda (continue)
          (with-exception-handler
              (lambda (errobj)
                (channel-put! outbox errobj)
                (continue)
                )
            (lambda ()
              (channel-put! outbox
               (call-with-values (lambda () (apply proc args)) list))
              (continue)
              ))))
       ;; The thunk evaluated in the main-loop returns #f to prevent
       ;; this thunk from being called repeatedly. Each call to this
       ;; procedure should install one timeout handler which executes
       ;; just once.
       #f))
    ;; Return a promise-like procedure.
    (lambda ()
      (th:thread-yield!)
      (channel-take! outbox))
    ))

(define (display-error errobj)
  (let ((port (current-error-port)))
    (display "error, " port)
    (display (error-object-message errobj) port)
    (display ":" port)
    (newline port)
    (for-each
     (lambda (irritant) (write irritant port) (newline port))
     (error-object-irritants errobj))
    ))

(define (the-repl get-environment-proc param-proc)
  ;; This procedure is applied to `TH:MAKE-THREAD` to create the REPL
  ;; thread.
  ;;------------------------------------------------------------------
  (lambda ()
    (let ((repl (guile:make-repl 'scheme)))
      (display (repl-welcome-message))
      (let loop ()
        (call/cc
         (lambda (continue)
           (with-exception-handler
               (lambda (errobj)
                 (display-error errobj)
                 (continue)
                 )
             (lambda ()
               (let ((get-results
                      (async-apply-in-main-thread
                       (lambda () (display (guile:repl-prompt repl)))))
                     )
                 (get-results))
               (let*((form (guile:repl-read repl))
                     (get-results
                      (cond
                       ((and form (not (null? form)))
                        (async-apply-in-main-thread
                         (lambda ()
                           (save-module-excursion
                            (lambda ()
                              (set-current-module (get-environment-proc))
                              (let ((form (guile:repl-expand repl form)))
                                (cond
                                 ((and form (not (null? form)))
                                  (cond
                                   (param-proc 
                                    (param-proc (lambda () (guile:repl-eval repl form))))
                                   (else (guile:repl-eval repl form))))
                                 (else (values))))
                              ))
                           )))
                       (else #f)
                       ))
                     (results (if get-results (get-results) #f))
                     )
                 (cond
                  ((not results) (values))
                  ((error-object? results) (display-error results))
                  ((or (pair? results) (null? results))
                   (for-each
                    (lambda (result)
                      (guile:repl-print repl result))
                    results))
                  (else
                   (let ((port (current-error-port)))
                     (display "error, unexpected result: " port) 
                     (write results port)
                     (newline port)))
                  ))
               (continue)
               ))))
        (loop)
        ))))

(define make-repl-thread
  ;; Constructs a new REPL thread but does not start it running yet.
  ;; This procedure returns a thread handle. Apply this thread handle
  ;; to `THREAD-START!` as soon as your Gtk application has been
  ;; initialized. Takes an optional procedure `GET-ENVIRONMENT-PROC`
  ;; which should return an environment (or Guile module) when
  ;; applied. If this `GET-ENVIRONMENT-PROC` is not applied, the Guile
  ;; procedure `CURRENT-MODULE` is used by default.
  ;;------------------------------------------------------------------
  (case-lambda
    (() (make-repl-thread current-module #f))
    ((get-environment-proc) (make-repl-thread get-environment-proc #f))
    ((get-environment-proc param-proc)
     (let ((repl-thread
            (th:make-thread
             (the-repl get-environment-proc param-proc)
             "gtk3-repl")))
       repl-thread
       )))
  )

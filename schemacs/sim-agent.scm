
;; A thing that sort-of acts like a GUI object -----------------------------------------------------

(define-record-type <sim-agent-type>
  (make<sim-agent-type> type-name unique-id log-messages state)
  is<sim-agent-type>?
  (type-name     sim-agent-type-name)
  (unique-id     sim-agent-unique-id)
  (log-messages  sim-agent-log-messages  set!sim-agent-log-messages)
  (state         sim-agent-state         set!sim-agent-state)
  ;; Interesting note: it was here where I discovered that Guile does
  ;; NOT allow field names to be called "typename" When I did try
  ;; using "typename" as a field name, I got some completely weird
  ;; error about the <sim-agent-type> symbol begin unbound. This is
  ;; definitely a bug and should be reported.
  )

(define *sim-agent-id-gen* (make-parameter 0))

(define (sim-agent-reset-id-gen) (*sim-agent-id-gen* 0))

(define *sim-agent-display-port*
  (make-parameter (current-output-port)))

(define =>sim-agent-state
  (record-unit-lens
   sim-agent-state
   set!sim-agent-state
   '=>sim-agent-state))

(define =>sim-agent-log-messages
  (record-unit-lens
   sim-agent-log-messages
   set!sim-agent-log-messages
   '=>sim-agent-log-messages))

(define (sim-agent-type? name)
  ;; Creates a function that checks if a sim agent has a
  ;; "sim-agent-type-name" of the given name. This function is very
  ;; different from "is<sim-agent-type>?" which only checks if the
  ;; given record is of "<sim-agent-type>".
  (lambda (obj)
    (and (is<sim-agent-type>? obj)
         (equal? name (sim-agent-type-name obj)))))

(define (new-sim-agent type-name state-data . log-msgs)
  ;; Construct a new agent
  (let*((id (*sim-agent-id-gen*))
        (log (new-mutable-vector (min 1 (length log-msgs))))
        (this (make<sim-agent-type> type-name id log state-data)))
    (apply mutable-vector-append! log log-msgs)
    (*sim-agent-id-gen* (+ 1 id))
    this))

(define (sim-agent-clear-logs obj)
  ;; Erase all log messages
  (set!sim-agent-log-messages obj (new-mutable-vector 1)))

(define (sim-agent-log obj msg . values)
  ;; Write a single log message applying msg and values to the SRFI-28
  ;; basic string "format" function.
  (mutable-vector-append!
   (sim-agent-log-messages obj)
   (apply basic:format msg values)))

(define (sim-agent-for-each-log proc obj)
  ;; Apply a procedure to each log message
  (mutable-vector-for-each proc (sim-agent-log-messages obj)))

(define (sim-agent-print-logs obj do-clear)
  ;; Print all log messages to "current-output-port" using the
  ;; "display" function.
  (sim-agent-for-each-log display obj)
  (when do-clear (sim-agent-clear-logs obj)))

(define (sim-agent-logs-to-string obj do-clear)
  ;; Concatenate all log messages into a single string.
  (let ((port (open-output-string)))
    (sim-agent-for-each-log (lambda (log) (display log port)) obj)
    (when do-clear (sim-agent-clear-logs obj))
    (flush-output-port port)
    (get-output-string port)))

(define (sim-agent->string obj show-logs)
  ;; Create a string representation of a <sim-agent-type> object. The
  ;; show-logs argument can be #f to eschew making logs a part of the
  ;; string, or it can be #t to show all logs, or it can be a positive
  ;; number N to show the last N log messages, or it can be a negative
  ;; number to show the first N log messages.
  (let ((port (open-output-string)))
    (display "(" port)
    (display (sim-agent-type-name obj) port)
    (display " " port)
    (write (sim-agent-unique-id obj) port)
    (cond
     ((eq? show-logs #f) #f)
     ((eq? show-logs #t)
      (sim-agent-for-each-log
       (lambda (log)
         (newline port)
         (display "  " port)
         (write log port))
       obj)))
    (display ")\n" port)
    (flush-output-port port)
    (get-output-string port)))

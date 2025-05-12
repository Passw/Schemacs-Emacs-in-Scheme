
(define-record-type <channel-type>
  ;; A thread-safe box. Storing or retreiving values from this box
  ;; using the various CHANNEL APIs in this library are guaranteed to
  ;; not cause race conditions, though some care must be taken to
  ;; avoid deadlocks. A channel can contain only one value at a time.
  (make<channel> mutex condvar err-handler is-full value)
  channel-type?
  (mutex       channel-mutex)
  (condvar     channel-condvar)
  (err-handler channel-error-handler %set!channel-error-handler)
  (is-full     channel-is-full?      set!channel-is-full?)
  (value       channel-value         set!channel-value))

(define (%new-channel err-handler is-full value)
  (make<channel>
   (th:make-mutex)
   (th:make-condition-variable)
   err-handler is-full value))

(define (%channel-pop! ch update)
  ;; Blocks if the channel state is not full, then evaluates UPDATE
  ;; and changes the channel state to not full.
  (with-exception-handler (channel-error-handler ch)
    (lambda ()
      (let loop ()
        (th:mutex-lock! (channel-mutex ch))
        (cond
         ((not (channel-is-full? ch))
          (th:mutex-unlock! (channel-mutex ch) (channel-condvar ch))
          (loop))
         (else
          (let ((return (update ch)))
            (th:condition-variable-signal! (channel-condvar ch))
            (th:mutex-unlock! (channel-mutex ch))
            return)))))))

(define (%channel-push! ch update)
  ;; Blocks if the channel state is full, then evaluates UPDATE and changes
  ;; the channel state to full.
  (with-exception-handler (channel-error-handler ch)
    (lambda ()
      (let loop ()
        (th:mutex-lock! (channel-mutex ch))
        (cond
         ((not (channel-is-full? ch))
          (let ((return (update ch)))
            (th:condition-variable-signal! (channel-condvar ch))
            (th:mutex-unlock! (channel-mutex ch))
            return))
         (else
          (th:mutex-unlock! (channel-mutex ch) (channel-condvar ch))
          (loop)))))))

(define (channel-take! ch)
  ;; Take a value from the channel, and empty the channel. Block if it
  ;; is not full.
  (%channel-pop! ch
   (lambda (ch)
     (let ((return (channel-value ch)))
       (set!channel-value ch #f)
       (set!channel-is-full? ch #f)
       return))))

(define (channel-put! ch new-value)
  ;; Place a value into the channel without emptying the
  ;; channel. Block if it is already full.
  (%channel-push! ch
   (lambda (ch)
     (let ((return (channel-value ch)))
       (set!channel-value ch new-value)
       (set!channel-is-full? ch #t)
       return))))

(define (channel-ref ch)
  ;; Return the value in the channel, block if it is not full. Does
  ;; not empty the channel.
  (%channel-pop! ch (lambda (ch) (channel-value ch))))

(define (channel-try-ref ch)
  ;; Return the value in the channel "V" as (values #t v) without
  ;; blocking, if the channel is not full, returns (values #f #f)
  (with-exception-handler (channel-error-handler ch)
    (lambda ()
      (th:mutex-lock! (channel-mutex ch))
      (let ((full (channel-is-full? ch))
            (return (channel-value ch)))
        (th:mutex-unlock! (channel-mutex ch))
        (values full return)))))

(define (set!channel-error-handler ch err-handler)
  (with-exception-handler (channel-error-handler ch)
    (lambda ()
      (th:mutex-lock! (channel-mutex ch))
      (%set!channel-error-handler ch err-handler)
      (th:mutex-unlock! (channel-mutex ch)))))

(define (channel-default-error-handler err)
  (display err)
  (newline))

(define new-channel/error-handler
  ;; Create a new channel with an error handler as the first argument
  (case-lambda
    ((err-handler)
     (%new-channel err-handler #f #f))
    ((err-handler init-value)
     (%new-channel err-handler #t init-value))))

(define new-channel
  ;; Create a new channel. No arguments creates an empty channel, one
  ;; argument creates a full channel containing that argument.
  (case-lambda
    (()
     (new-channel/error-handler channel-default-error-handler))
    ((init-value)
     (new-channel/error-handler channel-default-error-handler init-value))))

(define (channel-update! ch update)
  ;; The UPDATE function must take one value, the value stored in the
  ;; channel, and return 2 values:
  ;;   1. the updated channel value, and
  ;;   2. one other arbitrary return value.
  (%channel-pop! ch
   (lambda (ch)
     (let-values (((new-value return) (update (channel-value ch))))
       (set!channel-value ch new-value)
       return))))

;; -------------------------------------------------------------------------------------------------

(define-record-type <log-buffer-type>
  ;; You can record information to this buffer from any thread in a
  ;; thread-safe way. This data structure can be used alone, but
  ;; usually you would use this by calling NEW-LOG-BUFFER, which
  ;; creates a concurrent <channel-type> that stores a log buffer, and
  ;; pass this channel around to the LOG-MESSAGE!,
  ;; LOG-DISPLAY-MESSAGES, and LOG-CLEAR-MESSAGES! procedures.
  (make<log-buffer-type> counter buffer)
  log-buffer-type?
  (counter log-buffer-counter set!log-buffer-counter)
  (buffer  log-buffer-buffer  set!log-buffer-buffer))

(define-record-type <log-message-type>
  ;; This is an entry in a <LOG-BUFFER-TYPE> buffer created by the
  ;; LOG-MESSAGE! procedure. When calling LOG-ITERATE_MESSAGE-BUFFER
  ;; the procedure you provide is evaluated for each log message and
  ;; each message given to the iterated procedure is a data structure
  ;; of this type.
  (make<log-message-type> event-id from-thread string)
  log-message-type?
  (event-id    log-message-id)
  (from-thread log-message-thread)
  (string      log-message-string))

(define (new-unsafe-log-buffer)
  ;; A log buffer not stored in a channel, and so is not thread
  ;; safe. This is actually perfectly safe if your program is
  ;; single-threaded.
  (make<log-buffer-type> 0 (vec:new-mutable-vector 16)))

(define (new-log-buffer)
  ;; Create a new <LOG-BUFFER-TYPE> contained within a thread-safe
  ;; <CHANNEL-TYPE> box.
  (new-channel (new-unsafe-log-buffer)))

(define (log! ch string)
  ;; Produce a log message in a channel CH, where CH must have been
  ;; created by the NEW-LOG-BUFFER function.
  (cond
   ((log-buffer-type? ch)
    (let ((new-id (+ 1 (log-buffer-counter ch))))
      (set!log-buffer-counter ch new-id)
      (vec:mutable-vector-append!
       (log-buffer-buffer ch)
       (make<log-message-type> new-id (th:current-thread) string))))
   ((channel-type? ch)
    (channel-update! ch (lambda (ch) (values ch (log! ch string)))))
   (else (error "not a log buffer or channel type" ch string))))

(define (log-clear! ch)
  ;; Return all log messages in the given log buffer CH, where CH must
  ;; have been craeted by the NEW-LOG-BUFFER function. Before this
  ;; function returns, all log messages are deleted from the log buffer.
  (cond
   ((log-buffer-type? ch)
    (let ((return (log-buffer-buffer ch)))
      (set!log-buffer-buffer ch (vec:new-mutable-vector 16))
      return))
   ((channel-type? ch)
    (channel-update! ch (lambda (ch) (values ch (log-clear! ch)))))
   (else (error "not a log buffer or channel type" ch))))

(define (log-buffer-iterate buffer action)
  ;; Iterate over all messages in a <LOG-BUFFER-TYPE> calling the
  ;; given ACTION procedure for each log message. The message data
  ;; type given to the ACTION procedure is a value of type
  ;; <LOG-MESSAGE-TYPE>. The BUFFER argument is typically retrieved by
  ;; the LOG-CLEAR-MESSAGES! procedure.
  (cond
   ((vec:mutable-vector-type? buffer)
    (vec:mutable-vector-for-each action buffer))
   ((log-buffer-type? buffer)
    (log-buffer-iterate (log-buffer-buffer buffer) action))
   (else (error "not a log buffer object" buffer))))

(define log-message-display
  ;; This function takes a value of tyoe <LOG-MESSAGE-TYPE> and
  ;; displays it to the given PORT, or to CURRENT-OUTPUT-PORT if no
  ;; port is passed as an argument. This function is typically used
  ;; within an iteration procedure passed to the
  ;; LOG-BUFFER-ITERATE function.
  (case-lambda
    ((msg)
     (display (log-message-id msg))
     (display ": ")
     (write (log-message-string msg))
     (newline))
    ((msg port)
     (display (log-message-id msg) port)
     (display ": " port)
     (write (log-message-string msg) port)
     (newline port))))

(define log-dump!
  ;; Clear the current log and print all messages in the message
  ;; buffer CH, where CH must have been created by the NEW-LOG-BUFFER
  ;; function.
  (case-lambda
    ((ch)
     (log-buffer-iterate
      (log-clear! ch)
      log-message-display))
    ((ch port)
     (log-buffer-iterate
      (log-clear! ch)
      (lambda (msg) (log-message-display msg port))))))

;; -------------------------------------------------------------------------------------------------

(define-record-type <worker-type>
  (make<worker> label channel thread job-desc fire)
  worker-type?
  (label     worker-label)
  (channel   worker-channel)
  (thread    worker-thread)
  (job-desc  worker-job-description)
  (fire      worker-fire))

(define-record-type <work-payload-type>
  (make<work-payload-type> work)
  work-payload-type?
  (work work-payload))

(define (request-work! worker payload)
  ;; This function sends a payload of WORK to the specified `WORKER`
  ;; to be evaluated in a separate thread.
  (channel-put! (worker-channel worker) (make<work-payload-type> payload)))

(define (relieve-worker! worker)
  (cond
   ((not worker) (values))
   ((worker-type? worker)
    (display ";;halting previous worker: ")
    (display (worker-label worker))
    (newline)
    (channel-put! (worker-channel worker) #f))
   (else
    (error
     "*worker-thread* is not a <worker-type>"
     worker))))

(define (fire-worker! worker)
  ;; Forces a worker thread to stop working.
  ;;------------------------------------------------------------------
  ((worker-fire worker) (worker-thread worker)))

(define (%specialist-job apply-to-payload state)
  (lambda (ch)
    (lambda ()
      (let loop ((state state))
        (let ((payload (channel-take! ch)))
          (cond
           ((not payload) (values))
           ((work-payload-type? payload)
            (let-values
                (((continue state)
                  (apply-to-payload (work-payload payload) state)))
              (cond
               ((not continue) (values))
               (else (loop state)))))
           (else (error "bad signal"))))))))

(define specialist-job
  ;; This procedure can be used to create a worker that is passed as
  ;; the second argument to the `START-WORKER-WITH` function.
  ;;
  ;; This procedure creates a worker using a worker procedure that
  ;; takes input of a specific type -- creating a worker that
  ;; specializes in that particular type of work. When a specialist
  ;; worker is running, the `REQUEST-WORK!` function must take a value
  ;; of a type that is expected by the specialist worker.
  ;;
  ;; This function takes an optional initial `STATE` as the second
  ;; argument which changes the nature of the worker procedure that
  ;; you must pass as the first argument to this function.
  ;;
  ;; - If an initial state *is not* given, the worker procedure
  ;;   argument must take only one argument, the payload received from
  ;;   `REQUEST-WORK!`. The worker must also return only one value: a
  ;;   #t or #f value to indicate whether or not the worker should
  ;;   continue to accept work payloads or quit.
  ;;
  ;; - If an initial state *is* given, then worker procedure argument
  ;;   must take two arguments:
  ;;
  ;;    1. the payloads received from the `REQUEST-WORK!` function
  ;;    2. the current state as the second.
  ;;
  ;;   The work payload must also return two values:
  ;;
  ;;    1. #t or #f indicating whether or not the worker should
  ;;       continue waiting for more work payloads or quit.
  ;;
  ;;    2. an updated state value.
  ;;
  ;; Not all workers need to be specialists. Consider using the
  ;; `ORDINARY-JOB` procedure instead, which accepts arbitrary thunks
  ;; as work payloads from `REQUEST-WORK!`, and evaluates those thunks
  ;; in the work thread.
  ;; ------------------------------------------------------------------
  (case-lambda
    ((apply-to-payload)
     (%specialist-job (lambda (payload _) (values (apply-to-payload payload #f) #f)) #f))
    ((apply-to-payload state)
     (%specialist-job apply-to-payload state))))

(define ordinary-job
  ;; This procedure can be used to create a worker that is passed as
  ;; the second argument to the `NEW-WORKER-WITH` function.
  ;;
  ;; Creates a simple worker that takes thunks from `REQUEST-WORK!`
  ;; and evaluates them in the work thread. This means if you launch a
  ;; thread that was created with `ORDINARY-JOB`, every time you call
  ;; `REQUEST-WORK!` you must pass a thunk as the argument which
  ;; becomes the work payload to be performed in the worker thread.
  ;;
  ;; Like `SPECIALIST-JOB`, this function takes an optional initial
  ;; state which changes the nature of the procedure that you must
  ;; pass as work to the `REQUEST-WORK` function.
  ;;
  ;; - If an initial state *is not* given, the work procedure that is
  ;;   sent to the worker via `REQUEST-WORK!` must be a procedure that
  ;;   takes no values, and returns a single a #t or #f value to
  ;;   indicate whether or not the worker should continue to accept
  ;;   work payloads or quit.
  ;;
  ;; - If an initial state *is* given, then work procedure that is
  ;;   sent to the worker thread via `REQUEST-WORK!` must take a
  ;;   single argument, which is the current state. The work payload
  ;;   must also return two values:
  ;;
  ;;   1. #t or #f indicating whether or not the worker should
  ;;      continue waiting for more work payloads or quit.
  ;;
  ;;   2. an updated state value.
  ;;------------------------------------------------------------------
  (case-lambda
    (()
     (specialist-job (lambda (payload _) (values (payload) #f)) #f))
    ((state)
     (specialist-job (lambda (payload state) (payload state)) state))))

(define (new-worker-with label describe-job channel make-thread fire)
  ;; Construct a `<WORKER-TYPE>`, halting the previous worker (if
  ;; there is one), and launching a new worker with MAKE-THREAD and
  ;; FIRE. The `DESCRIBE-JOB` argument can be constructed by one of
  ;; `ORDINARY-JOB` or `SPECIALIST-JOB`, are procedures that construct
  ;; procedures that must take a <CHANNEL-TYPE> and construct a thunk,
  ;; and this thunk will be passed as the first argument to
  ;; `MAKE-THREAD` to be evaluated in the new thread -- *note*
  ;; therefore that DESCRIBE-JOB is *not* passed to MAKE-THREAD, but
  ;; the result of calling DESCRIBE-JOB is passed to MAKE-THREAD, and
  ;; the procedure that MAKE-THREAD receives is a thunk (with no
  ;; arguments) that is evaluated in a separate thread.
  ;; ------------------------------------------------------------------
  (let*((channel
         (if (channel-type? channel) channel (new-channel)))
        (proc (describe-job channel)))
    (cond
     ((not (procedure? proc))
      (error "failed to construct worker procedure"))
     (else
      (let ((thread (make-thread proc label)))
        (cond
         ((not thread) (error "failed to start thread"))
         (else
          (let ((worker (make<worker> label channel thread describe-job fire)))
            (th:thread-start! thread)
            worker))))))))

(define *worker-counter* (make-parameter 0))

(define (next-worker-id!)
  (let ((n (+ 1 (*worker-counter*))))
    (*worker-counter* n)
    (call-with-port (open-output-string)
      (lambda (port)
        (display "(worker " port)
        (write n port)
        (display " #:launched-by " port)
        (write (th:current-thread) port)
        (display ")" port)))))

(define (new-worker label describe-job channel)
  ;; `DESCRIBE-JOB` is a thunk constructor, it takes a <CHANNEL-TYPE>
  ;; argument and must construct a thunk that is evaluated in a
  ;; separate thread. This function passed to `START-WORKER`, and used
  ;; by `NEW-WORKER-WITH` to launch a worker using SRFI-18
  ;; threads. The CHANNEL argument is created if #f, the channel is
  ;; passed to DESCRIBE-JOB.
  ;; ------------------------------------------------------------------
  (let ((label (if (not label) (next-worker-id!) label))
        (worker
         (new-worker-with
          label
          (if (not describe-job) (ordinary-job) describe-job)
          channel
          th:make-thread
          th:thread-terminate!)))
    (th:thread-start! (worker-thread worker))
    worker))

(define (make-recruiter new-worker)
  ;; This is a procedure that creates a procedure that inspects 3
  ;; optional arguments, then applies these three arguments to the
  ;; given `NEW-WORKER` function. The `RECRUIT-WORKER` function
  ;; applies the `NEW-WORKER` function above to this procedure, but if
  ;; you want to define your own version of NEW-WORKER that creates
  ;; threads in some way other than the procedures defined in SRFI-18
  ;; (for example using an FFI to some threading library), then you
  ;; can make alternative versions of `RECRUIT-WORKER` that run in
  ;; alternative types of thread.
  ;; ------------------------------------------------------------------
  (case-lambda
    (() (new-worker "*worker-thread*" #f #f))
    ((a)
     (cond
      ((string?    a)    (new-worker a #f #f))
      ((procedure? a)    (new-worker #f a #f))
      ((channel-type? a) (new-worker #f #f a))
      (else (error "first argument must be a string or procedure"))))
    ((a b)
     (cond
      ((and (string? a) (procedure? b))
       (new-worker a b #f))
      ((and (string? a) (channel-type? b))
       (new-worker a #f b))
      ((and (procedure? a) (channel-type? b))
       (new-worker #f a b))))
    ((a b c) (new-worker a b c))))

(define recruit-worker
  ;; This function takes three arguments, any or all of them are
  ;; optional, but the order in which they are applied must always be
  ;; the same:
  ;;
  ;;  1. LABEL: a string to describe the worker. This label need not
  ;;     be unique. If this argument is omitted, a unique ID is
  ;;     generated for each worker.
  ;;
  ;;  2. DESCRIBE-JOB: a procedure constructed by `SPECIALIST-JOB` or
  ;;     `ORDINARY-JOB` that is used to construct the working
  ;;     procedure that actually runs in the new thread.
  ;;
  ;;  3. CHANNEL: you can construct your own channel to be used to
  ;;     transmit work payloads to the worker.
  ;;------------------------------------------------------------------
  (make-recruiter new-worker))

(define (recruit-clone mentor label)
  ;; This function recruits a new worker that does the exact same job
  ;; and takes input from the same input channel as the `MENTOR`
  ;; argument, where `MENTOR` is the worker that is being cloned. This
  ;; will result in two threads doing the same job and accepting work
  ;; payloads from the same source, with each worker performing their
  ;; work in parallel.
  ;;
  ;; *Note* that if you signal `RELIEVE-WORKER!` or `REQUEST-WORK!` to
  ;; the mentor or clone workers, only one or the other will receive
  ;; the signal. You will have to call `RELIEVE-WORKER!` for the
  ;; mentor and every clone, but there is no guarantee that the worker
  ;; you wanted to be relieved is the one that is actually relieved.
  ;;
  ;; If you want to halt only a specific worker, you must use
  ;; `FIRE-WORKER!` instead of `RELIEVE-WORKER!`
  ;; ------------------------------------------------------------------
  (new-worker label (worker-channel mentor) (worker-job-description mentor)))

;; -------------------------------------------------------------------------------------------------

(define (%make-delayed-loop before after wait-time state)
  (cond
   ((and (not (and (not before) (not after)))
         (or (procedure? before) (procedure? after)))
    (lambda ()
      (let loop ((state state))
        (let-values
            (((continue state)
              (if before (before state) (values #t state))))
          (cond
           ((not continue) (values))
           (else
            (th:thread-sleep! wait-time)
            (let-values
                (((continue state)
                  (if after (after state) (values #t state))))
              (when continue (loop state)))))))))
   (else
    (error
     "must have one non-null procedure, before or/and after"
     before after))))

(define make-delayed-loop
  ;; This function creates a simple thunk that loops, with WAIT-TIME
  ;; seconds between each loop. This function takes an optional
  ;; `STATE` argument.
  ;;
  ;; When the loop starts the `BEFORE` procedure is called with
  ;; `STATE` as an argument, and must return another `STATE`
  ;; value. Then the thread sleeps for `WAIT-TIME` amount of
  ;; time. Then the `AFTER` procedure is called with the updated
  ;; `STATE` argument. Then this function loops, calling `BEFORE`
  ;; again unless the thread is stopped.
  ;;
  ;; Either `BEFORE` or `AFTER`, but not both, may be #f, in which
  ;; case that procedure is not used.
  ;;
  ;; If the `STATE` argument is passed to this function, then both
  ;; `BEFORE` and `AFTER` procedures (if they are not #f) take a
  ;; single `STATE` argument, but return two values:
  ;;
  ;;  1. #t or #f indicating whether looping should continue.
  ;;
  ;;  2. an updated STATE which is passed to the procedure on the next
  ;;     loop.
  ;;
  ;; If the optinoal `STATE` argument is not given, `BEFORE` and
  ;; `AFTER` take no arguments and must only return on value, #t or #f
  ;; indicating whether looping should continue.
  ;;------------------------------------------------------------------
  (case-lambda
    ((before after wait-time)
     (%make-delayed-loop
      (if (not before) #f (lambda _ (values (before) #f)))
      (if (not after)  #f (lambda _ (values (after)  #f)))
      wait-time #f))
    ((before after wait-time state)
     (%make-delayed-loop before after wait-time state))))

(define delayed-loop
  ;; Create a delayed loop with `MAKE-DELAYED-LOOP` and evaluate the
  ;; loop immediately in the current thread.
  (lambda args ((apply make-delayed-loop args))))

(define make-delayed-counter
  ;; Uses MAKE-DELAYED-LOOP to iterate an initial value FROM, and
  ;; continues looping until 
  (case-lambda
    ((proc wait-time from to step)
     (let ((step (if (not step) 1 step))
           (stop? (if (< step 0) <= >=)))
       (%make-delayed-loop
        #f
        (lambda (n)
          (cond
           ((stop? n to) (values #f n))
           (else
            (proc n)
            (values #t (+ n step)))))
        wait-time from)))
    ((proc wait-time from to)
     (make-delayed-counter proc wait-time from to 1))))

(define delayed-counter
  ;; Craeted a delayed counter with `MAKE-DELAYED-COUNTER` and
  ;; evaluated the loop immediately in the current thread.
  (lambda args ((apply make-delayed-counter args))))

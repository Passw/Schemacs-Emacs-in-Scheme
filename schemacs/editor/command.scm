
(define-record-type <command-type>
  ;; A command is a structure containing a function that responds to
  ;; input events. Every command-procedure receives 2 arguments:
  ;;
  ;;  1. the window in which the event occurred (from this you can
  ;;     obtain the current buffer and the parent frame), and
  ;;
  ;;  2. the event value itself.
  ;;
  (make<command> name procedure api docstr srctype srcloc srcstr)
  command-type?
  (name      command-name            set!command-name)
  (procedure command-procedure       set!command-procedure)
  (api       command-api             set!command-api)
  (docstr    command-doc-string      set!command-doc-string)
  (srctype   command-source-type     set!command-source-type)
  (srcloc    command-source-location set!command-source-location)
  (srcstr    command-source-code     set!command-source-code))

(define (new-command name proc api docstr)
  (make<command> name proc api docstr 'built-in #f #f))

(define =>command-name*!
  (record-unit-lens command-name set!command-name '=>command-name))

(define =>command-procedure*!
  (record-unit-lens command-procedure set!command-procedure '=>command-procedure))

(define =>command-api*!
  (record-unit-lens command-api set!command-api '=>command-api))

(define =>command-doc-string*!
  (record-unit-lens command-doc-string set!command-doc-string '=>command-doc-string))

(define =>command-source-type*!
  (record-unit-lens command-source-type set!command-source-type '=>command-source-type))

(define =>command-source-location*!
  (record-unit-lens command-source-location set!command-source-location '=>command-source-location))

(define =>command-source-code*!
  (record-unit-lens command-source-code set!command-source-code '=>command-source-code))

(define (run-command cmd)
  ;; Run a the `COMMAND-PROCEDURE` procedure which takes no arguments.
  ((command-procedure cmd)))

(define (apply-command cmd . args)
  ;; Some commands have an "API" (application programming interface)
  ;; which are procedures that take arguments. This is different from
  ;; the `COMMAND-PROCEDURE` which takes no arguments and so typically
  ;; must be paramaterized.  There is a rule that all
  ;; `COMMAND-PROCEDURE`s must call into the API associated with it in
  ;; it's `<COMMAND-TYPE>` structure if the command procedure itself
  ;; is implemented in terms of that API procedure. If no API
  ;; procedure is provided by the given `CMD` command, the `ARGS` are
  ;; ignored and the CMD is invoked with no arguments.
  (if (command-api cmd)
      (apply (command-api cmd) args)
      ((command-api cmd))))

(define show-command
  (case-lambda
    ((cmd) (show-command cmd (current-output-port)))
    ((cmd port)
     (cond
      ((command-type? cmd)
       (display "(command #:name " port)
       (display (command-name cmd) port)
       (when (symbol? (command-source-type cmd))
         (display " #:source-type '" port)
         (display (symbol->string (command-source-type cmd)) port))
       (when (command-source-location cmd)
         (display "\n #:source-location " port)
         (write (command-source-location cmd) port)
         (display "\n #:source-code\n " port)
         (write (command-source-code cmd) port))
       (display ")"))
      (else
       (error "not a command-type value" cmd))))))

(import
  (scheme base)
  (scheme case-lambda)
  (scheme write)
  (srfi srfi-64)
  (gypsum pretty))

(define test-mode
  ;; Choose one of 'print-mode or 'unit-test-mode.
  ;;
  ;; - 'unit-test-mode :: the SRFI-64 APIs are used
  ;;   to declare unit tests.
  ;;
  ;; - 'print-mode :: tests are run and their results
  ;;   are output in human-readable form.
  (if #f 'unit-test-mode 'print-mode))

(define run-all-tests?
  ;; The can are all run at once, but if you discover smoe of the
  ;; tests have failed and you want to isolate them, you can set this
  ;; variable to false and modify the code at the bottom of this file
  ;; to just run the failing tests.
  #f)

(when (eq? test-mode 'unit-test-mode)
  (test-begin "gypsum_pretty"))

;; Testing different printing modes with simplest possible inputs.

(define-record-type <test-print-proc-type>
  (make<test-print-proc-type> name proc)
  test-print-proc-type?
  (name test-print-proc-name)
  (proc test-print-proc-runner))


(define (run-test-print-proc test-print printer)
  ((test-print-proc-runner test-print) printer))


(define (%with-output-string-port name run-port)
  (make<test-print-proc-type> name
   (lambda (print-proc)
     (let ((port (open-output-string)))
       (parameterize
           ((current-output-port port))
         (run-port port print-proc))
       (let ((result (get-output-string port)))
         (close-port port)
         (values result #f))))))


(define with-false
  (make<test-print-proc-type> "with-false"
   (lambda (print-proc) (values (pretty #f print-proc) #f))))


(define with-true
  (%with-output-string-port "with-true"
   (lambda (port print-proc) (pretty #t print-proc))))


(define with-default
  (%with-output-string-port "with-default"
   (lambda (port print-proc) (pretty print-proc))))


(define with-output-string-port
  (%with-output-string-port "with-output-string-port" pretty))


(define (iterate-text-buffer buffer port)
  (pp-fold-lines
   (lambda (_i line _f) (pp-display-line line port) #f)
   #f buffer))


(define (dont-get-stats _) #f)

(define (do-get-stats pp)
  (pp-line-buffer-stats (pp-state-line-buffer pp)))

(define (%with-line-buffer name make-buffer iterate-buffer get-stats)
  (make<test-print-proc-type> name
   (lambda (print-proc)
     (let*((port (open-output-string))
           (buffer (make-buffer))
           (result (pretty buffer print-proc))
           (_ (iterate-buffer result port))
           (str (get-output-string port))
           (stats (get-stats buffer))
           )
       (close-port port)
       (values str stats)))))


(define with-line-buffer
  (%with-line-buffer "with-line-buffer"
   new-line-buffer iterate-text-buffer dont-get-stats))

(define with-sized-line-buffer
  (%with-line-buffer "with-sized-line-buffer"
   (lambda () (new-line-buffer 16)) iterate-text-buffer dont-get-stats))

(define with-buffer-state-no-stats
  (%with-line-buffer "with-buffer-state-no-stats"
   (lambda () (print-to-buffer #f)) iterate-text-buffer dont-get-stats))

(define with-buffer-state-stats
  (%with-line-buffer "with-buffer-state-stats"
   (lambda () (print-to-buffer #t)) iterate-text-buffer do-get-stats))


(define all-test-runners
  (list
   with-false
   with-true
   with-default
   with-output-string-port
   with-line-buffer
   with-sized-line-buffer
   with-buffer-state-no-stats
   with-buffer-state-stats
   ))

(define-record-type <test-case-type>
  (test-case expected-str expected-stats print-proc)
  test-case-type?
  (expected-str    test-case-expected-string)
  (expected-stats  test-case-expected-stats)
  (print-proc      test-case-printer))


(define all-test-cases
  (list
   (test-case
    "Hello"
    (new-line-buffer-stats 5 5 0 5 0 0)
    (print "Hello"))
   (test-case
    "Hello\nHello"
    (new-line-buffer-stats 6 5 0 11 0 0)
    (print "Hello" (line-break) "Hello"))
   (test-case
    "" (new-line-buffer-stats #f #f 0 0 0 0)
    (print #f))
   (test-case
    "" (new-line-buffer-stats #f #f 0 0 0 0)
    (print #f #f))
   (test-case
    "0-Hello()?\nyes.1"
    (new-line-buffer-stats 11 5 0 16 0 0)
    (print #f 0 #\- "Hello" '() #\?  #f (line-break) (print "yes" #\. #f 1)))
   (test-case
    "----------"
    (new-line-buffer-stats 10 10 0 10 0 0)
    (repeat 10 #\-))
   (test-case
    "<><><><><>"
    (new-line-buffer-stats 10 10 0 10 0 0)
    (repeat 5 "<>"))
   (test-case
    "<><><><><>"
    (new-line-buffer-stats 10 10 0 10 0 0)
    (repeat 5 (print #\< #\>)))
   (test-case
    "Hello?\nHello?\nHello?\n"
    (new-line-buffer-stats 6 6 0 18 0 0)
    (repeat 3 "Hello" #\? (line-break)))
   (test-case
    "  Hello\n\n  Hello\n\n  Hello\n\nHello\n\nHello"
    (new-line-buffer-stats 7 7 4 39 0 2)
    (print
     (indent-by 2
      "Hello" (line-break) (line-break)
      "Hello" (line-break) (line-break)
      "Hello")
     (line-break) (line-break)
     "Hello" (line-break) (line-break)
     "Hello"))
   (test-case
    "  Hello\n\n  Hello\n\n  Hello\n\nHello\n\nHello"
    (new-line-buffer-stats 7 7 4 39 0 2)
    (print
     (indent-by 2 "Hello\n\nHello\n\nHello")
     "\n\nHello\n\nHello"))
   ))

;;------------------------------------------------------------------
;; First test the test runners against a simple test case, all test
;; runners should return the same string

(define basic-test-case
  (test-case
   "--- hello ---\n    hello\n\n\n    hello"
   (new-line-buffer-stats 14 0 5 27 0 4)
   (print
    (repeat 3 #\-) " hello " (repeat 3 #\-) (line-break)
    (indent-by 4 "hello" (line-break) (line-break) (line-break) "hello"))))

(define (show-basic-test-case)
  (let*((port (open-output-string))
        (_ (write (test-case-expected-string basic-test-case) port))
        (str (get-output-string port))
        )
    (close-port port)
    str))

(define (display-test-results show-ok expected-string result-string expected-stats result-stats)
  (let ((string-pass (equal? expected-string result-string))
        (stats-pass
         (or (not result-stats) ;; skip checking stats if no stats were generated
             (equal? expected-stats  result-stats)))
        )
    (cond
     ((and string-pass stats-pass) (when show-ok (display "OK\n")))
     (else
      (when show-ok (display "FAILED:\n"))
      (unless string-pass
        (display " result-string: ") (write result-string) (newline)
        (display " expect-string: ") (write expected-string) (newline)
        )
      (unless stats-pass
        (display "expected-stats: ") (write expected-stats) (newline)
        (display "  result-stats: ") (write result-stats) (newline)
        ))
     )))

(for-each
 (lambda (test-runner)
   (let-values
       (((expected-string) (test-case-expected-string basic-test-case))
        ((expected-stats) (test-case-expected-stats basic-test-case))
        ((result-string result-stats)
         (run-test-print-proc test-runner (test-case-printer basic-test-case)))
        )
     (cond
      ((eq? test-mode 'unit-test-mode)
       (test-equal
           (string-append (test-print-proc-name test-runner) "_strings")
         expected-string result-string)
       (test-equal
           (string-append (test-print-proc-name test-runner) "_stats")
         expected-stats result-stats))
      ((eq? test-mode 'print-mode)
       (display "check ")
       (display (test-print-proc-name test-runner))
       (display " ... ")
       (display-test-results #t expected-string result-string expected-stats result-stats))
      (else (error "unknown test mode" test-mode)))
     ))
 all-test-runners
 )

;;------------------------------------------------------------------
;; Now that we know all the test runners work the same, run a series
;; of more advanced tests on just three of the most representative
;; test runners: 1. print to string port, 2. print to buffer,
;; 3. print to buffer with statistics.

(for-each
 (lambda (test-runner)
   (when (eq? test-mode 'print-mode)
     (display "--------------------< ")
     (display (test-print-proc-name test-runner))
     (display " >--------------------\n"))
   (for-each
    (lambda (test-case)
      (let-values
          (((expected-string) (test-case-expected-string test-case))
           ((expected-stats) (test-case-expected-stats test-case))
           ((result-string result-stats)
            (run-test-print-proc test-runner (test-case-printer test-case)))
           )
        (cond
         ((eq? test-mode 'unit-test-mode)
          (test-equal expected-string result-string)
          (test-equal expected-stats result-stats))
         ((eq? test-mode 'print-mode)
          (display-test-results #f expected-string result-string expected-stats result-stats))
         (else (error "unknown test mode" test-mode)))
        ))
    all-test-cases)
   )
 (list
  with-false
  with-buffer-state-no-stats
  with-buffer-state-stats))

(when (eq? test-mode 'unit-test-mode)
  (test-end "gypsum_pretty"))

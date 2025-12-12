(import
  (scheme base)
  (scheme case-lambda)
  (schemacs test) ;;testing
  ;;(srfi  1) ;;lists
  (only (scheme file) open-input-file open-binary-input-file)
  (only (srfi 111) box box? unbox set-box!)
  (only (schemacs elisp-eval environment)
        =>lambda-args!
        =>lambda-optargs!
        =>lambda-rest!
        =>lambda-body!
        =>env-lexical-mode?!
        *elisp-output-port*
        *elisp-error-port*
        make<elisp-eval-error>
        elisp-eval-error-equal?
        ; --- the rest are imported because they are useful in the REPL ---
        write-elisp-eval-error
        )
  (schemacs elisp-eval)
  (schemacs lens)
  (schemacs pretty)
  (only (schemacs cursor) new-cursor cursor-step!)
  (only (schemacs lens vector)
        mutable-vector-type? =>mvector-index!
        new-mutable-vector
        mutable-vector-length
        mutable-vector-append!)
  (prefix (schemacs editor-impl) *impl/)
  (only (schemacs elisp-eval parser)
        list->elisp-form  elisp-form->list
        elisp-form-equal?  write-elisp-form
        elisp-read
        )
  (only (schemacs hash-table)
        hash-table-size
        hash-table-ref/default
        alist->hash-table
        ))

(test-begin "schemacs_elisp_eval_tests")

;;--------------------------------------------------------------------------------------------------

(define test-elisp-env (new-environment (*elisp-init-env*) #f "env-test"))

;; Raw results of evaluation
(define (test-elisp-reset-env!)
  (set! test-elisp-env (new-environment (*elisp-init-env*) #f "env-test"))
  )

(define (test-elisp-eval! expr)
  (elisp-eval! expr test-elisp-env)
  )

(define (test-error-elisp-eval! expr)
  (parameterize ((handle-scheme-exceptions* #f))
    (let ((result (test-elisp-eval! expr)))
      (and (elisp-eval-error-type? result)
           (view result =>elisp-eval-error-message)
           ))))

(define (test-run same? expected case-runner list-form)
  (let ((iden (lambda (a) a))
        (form-form (list->elisp-form list-form))
        )
    (define (run display-type writer conv-result input-form)
      (let ((result (conv-result (case-runner input-form))))
        (cond
         ((same? expected result) #t)
         (else
          (display ";; unexpected result: ") (write result) (newline)
          (display ";;     was expecting: ") (write expected) (newline)
          (display ";; lisp-eval! on ") (write-string display-type)
          (display ": ") (writer input-form) (newline)
          (display ";;--------------------------------------------------------------------\n")
          #f
          ))))
    (and
     (run "list" write iden list-form)
     (run "form" write-elisp-form (lambda (form) (elisp-form->list #t form)) form-form)
     )))

(define (tc n)
  (display ";; <<test ") (write n) (display ">>\n")
  )

;;--------------------------------------------------------------------------------------------------
;; Test basic function application for built-in arithmetic functions.

(test-assert (elisp-environment-type? test-elisp-env))

(test-assert (test-run equal? "hello" test-elisp-eval! "hello"))

(test-assert (test-run equal? 3 test-elisp-eval! '(+ 1 2)))

(test-assert (test-run eq? #t test-elisp-eval! '(eq nil nil)))

(test-assert (test-run eq? '() test-elisp-eval! '(eq nil t)))

(test-assert (test-run eq? #t test-elisp-eval! '(eq nil '())))

(test-assert (test-run eq? #t test-elisp-eval! '(eq '() nil)))

(test-assert (test-run eq? #t test-elisp-eval! '(eq 5 5)))

(test-assert (test-run eq? #t test-elisp-eval! '(= 5 5 5)))

(test-assert (test-run eq? '() test-elisp-eval! '(= 5 5 6)))

(test-assert
    (test-run
     equal? "wrong type argument" test-error-elisp-eval!
     '(= 5 "hello")
     ))

(test-assert
    (test-run
     equal? "wrong type argument" test-error-elisp-eval!
     '(+ 5 "hello")
     ))

(test-assert
    (test-run
     equal? '(1 2 3) test-elisp-eval!
     '(list 1 2 (+ 1 2))
     ))

(test-assert
    (test-run
     eq? #t test-elisp-eval!
     '(equal (list 5) (list 5))
     ))

(test-assert
    (test-run
     eq? '() test-elisp-eval!
     '(equal (list 5) (list 5 6))))

;;--------------------------------------------------------------------------------------------------
;; Test `PROGN`, `PROG1`, and `PROG2` special forms.

(test-assert
    (test-run
     eqv? 2 test-elisp-eval!
     '(progn 1 2)
     ))

(test-assert
    (test-run
     eqv? 3 test-elisp-eval!
     '(progn 1 2 (+ 1 2))))

(test-assert
    (test-run
     eqv? 4 test-elisp-eval!
     '(progn (setq a 4) a)
     ))

(test-assert
    (test-run
     eqv? 5 test-elisp-eval!
     '(prog1 5 4 3 2 1)
     ))

(test-assert
    (test-run
     eqv? 4 test-elisp-eval!
     '(prog2 5 4 3 2 1)
     ))

(test-assert
    (test-run
     eqv? 2 test-elisp-eval!
     '(prog1 (+ 1 1) (+ 1 2) (+ 2 3))
     ))

(test-assert
    (test-run
     eqv? 3 test-elisp-eval!
     '(prog2 (+ 1 1) (+ 1 2) (+ 2 3))
     ))

;;--------------------------------------------------------------------------------------------------
;; Test `OR` and `AND` special forms.

(test-assert
    (test-run
     eq? #t test-elisp-eval!
     '(or nil nil nil t)
     ))

(test-assert
    (test-run
     eq? #t test-elisp-eval!
     '(or (= 1 1 1) nil nil)
     ))

(test-assert
    (test-run
     eq? '() test-elisp-eval!
     '(or nil nil nil (= 0 0 1))
     ))

(test-assert
    (test-run
     eq? '() test-elisp-eval!
     '(and nil nil nil t)
     ))

(test-assert
    (test-run
     eq? '() test-elisp-eval!
     '(and (= 1 1 1) nil nil)
     ))

(test-assert
    (test-run
     eq? #t test-elisp-eval!
     '(and t t t (= 0 0 0))
     ))

;;--------------------------------------------------------------------------------------------------
;; Test `IF`, `WHEN`, `UNLESS`, and `COND` special forms.

(test-assert
    (test-run
     eqv? 10 test-elisp-eval!
     '(if t 10 20)
     ))

(test-assert
    (test-run
     eqv? 20 test-elisp-eval!
     '(if (null t) 10 20)
     ))

(test-assert
    (test-run
     eqv? 30 test-elisp-eval!
     '(if (null t) 10 20 30)))

(test-assert
    (test-run
     eq? #f test-elisp-eval!
     '(when nil 40)
     ))

(test-assert
    (test-run
     eq? 50 test-elisp-eval!
     '(when t 50)
     ))

(test-assert
    (test-run
     eq? 60 test-elisp-eval!
     '(unless nil 60)
     ))

(test-assert
    (test-run
     eq? #f test-elisp-eval!
     '(unless t 70)
     ))

(test-assert
    (test-run
     eqv? 30 test-elisp-eval!
     '(cond (nil 10) (nil 20) (t 30) (t 40) (nil 50))))

(test-assert
    (test-run
     eqv? 30 test-elisp-eval!
     '(cond
       ((or nil nil) 10)
       ((and t nil) 20)
       ((or t nil) 30)
       (t 40)
       )))

(test-assert
    (test-run
     equal? (new-symbol "hello" 5) test-elisp-eval!
     '(let ((a (make-symbol "hello")))
        (set a 5)
        a)))

(test-assert
    (test-run
     equal? "hello" test-elisp-eval!
     '(let ((a (make-symbol "hello")))
        (symbol-name a)
        )))

(test-assert
    (test-run
     equal? '(3.0 7.0 2.0) test-elisp-eval!
     '(progn
       (defun linear-transform-3D (x y z)
         (let ((x (+ (* -1   x) (*  2 y) (* 0.5 z)))
               (y (+ (*  1.5 x) (* -1 y) (* 2   z)))
               (z (+ (* -0.5 x) (*  1 y) (* 0.5 z))))
           (list x y z)))
       (linear-transform-3D 4 3 2)
       )))

(test-assert
    (test-run
     equal? '(1 2 4 8) test-elisp-eval!
     '(let*((a 1)
            (b (* a 2))
            (c (* b 2))
            (d (* c 2)))
        (list a b c d)
        )))

;; Test whether closures can properly capture variables
(test-assert
    (test-run
     equal? 6 test-elisp-eval!
     '(progn
       (defun test-closure-capture (a b)
         (let ((f (lambda (c) (+ a b c))))
           (funcall f 1)
           ))
       (test-closure-capture 2 3)
       )))

;;--------------------------------------------------------------------------------------------------
;; Test `SETQ`, `LET`, and `LET*` special forms.

(test-assert
    (test-run
     eqv? 5 test-elisp-eval!
     '(progn
       (setq a 2 b 3)
       (+ a b)
       )))

(test-assert
    (test-run
     eqv? 8 test-elisp-eval!
     '(progn
       (setq a 3 b 5 c (+ a b))
       c)))

(test-assert
    (test-run
     equal? "wrong number of arguments, setq" test-error-elisp-eval!
     '(setq a)
     ))

(test-assert
    (test-run
     eqv? 13 test-elisp-eval!
     '(let ((a 5) (b 8)) (+ a b))
     ))

(test-assert
    (test-run
     eqv? 21 test-elisp-eval!
     '(let*((a 8) (b (+ 5 a)))
        (+ a b)
        )))

(test-assert
    (test-run
     eqv? 34 test-elisp-eval!
     '(let ((a 21))
        (setq a (+ a 13))
        a)))

(test-assert
    (test-run
     equal? '(89 55) test-elisp-eval!
     '(progn
       (setq a 21)
       (list
        (let ((b 34))
          (setq a (+ a b))
          (setq b (+ a b))
          b)
        a))))

(test-assert
    (test-run
     eq? '() test-elisp-eval!
     '(setq)))

;;--------------------------------------------------------------------------------------------------
;; Test quote and quasiquote forms.

(test-assert
    (test-run
     equal? '() test-elisp-eval!
     '(quote ())
     ))

(test-assert
    (test-run
     equal? '(1 2 3)
     test-elisp-eval!
     '(quote (1 2 3))
     ))

;;--------------------------------------------------------------------
;; The following test cases skip the usual `TEST-RUN` procedure
;; because it tests both the list form and `<ELISP-FORM-TYPE>` of the
;; input, but the result of `SCHEME->ELISP` does not (and should not)
;; be translated to an equivalent `<ELISP-FORM-TYPE>`. So for these
;; test cases, only the list form of the input should be tested.

(test-equal '(1 2 3)
  (test-elisp-eval!
   (scheme->elisp
    '(quasiquote (1 2 (unquote (+ 1 2))))
    )))

(test-equal '() (test-elisp-eval! (scheme->elisp '(quasiquote ()))))

(test-equal '(1 2 3) (test-elisp-eval! '(|`| (1 2 (|,| (+ 1 2))))))

(test-equal '(1 2 3) (test-elisp-eval! (car (scheme->elisp '(`(1 2 ,(+ 1 2)))))))

(test-equal '((+ 3 5) = 8)
  (test-elisp-eval!
   (scheme->elisp
    '(quasiquote
      ((+ ,(+ 1 2) ,(+ 2 3)) = ,(+ 1 2 2 3))
      ))))

(test-equal '(a (quote ()))
  (elisp->scheme (scheme->elisp '(a (quote ()))))
  )

;;--------------------------------------------------------------------------------------------------
;; `LAMBDA`, `DEFUN`, `APPLY`, and `FUNCALL` tests.

(test-assert
    (let ((func (test-elisp-eval! '(lambda () nil))))
      (and
       (null? (view func =>lambda-args!))
       (null? (view func =>lambda-optargs!))
       (not (view func =>lambda-rest!))
       (equal? '(nil) (view func =>lambda-body!))
       )))

(test-assert
    (test-run
     equal? '(1 + 2 = 3) test-elisp-eval!
     '(progn
       (setq a 3 b 5)
       (apply
        (lambda (a b)
          (list a '+ b '= (+ a b)))
        '(1 2)
        ))))

(test-assert
    (test-run
     equal? '() test-elisp-eval!
     '(apply (lambda () t nil) '())
     ))

(test-assert
    (test-run
     equal? #t  test-elisp-eval!
     '(apply (lambda () nil t) '())
     ))

(test-assert
    (test-run
     equal? '() test-elisp-eval!
     '(apply '(lambda () t nil) '())
     ))

(test-assert
    (test-run
     equal? #t  test-elisp-eval!
     '(apply '(lambda () nil t) '())
     ))

(test-assert
    (test-run
     equal? '((+ 1 1)(+ 1 2)(+ 2 3)(+ 3 5)(+ 5 8))
     test-elisp-eval!
     '(apply (function list) '((+ 1 1)(+ 1 2)(+ 2 3)(+ 3 5)(+ 5 8)))
     ))

(test-assert
    (test-run
     equal? '(2 + 3 = 5) test-elisp-eval!
     '(progn
       (setq a 5 b 8)
       (defun f (a b) (list a '+ b '= (+ a b)))
       (f 2 3)
       )))

(test-assert
    (test-run
     equal? (scheme->elisp '(a '())) test-elisp-eval!
     (scheme->elisp '(let ((a '())) (debug-print-stack) `(a ',a)))
     ))

;;--------------------------------------------------------------------
;; Test evaluating an expression with quote and unquote forms, and
;; specifically whether evaluating the expression in the form of a
;; list data structure and evaluating the expression in the form of an
;; AST data structure produces the same result.

(define (elisp-test-eval-list! expr)
  ;; Evaluate an `EXPR`, which must be a list data structure, and then
  ;; convert it's result to an AST data structure and then back to a
  ;; list. The reason this procedure is necessary is because the
  ;; `ELISP-EVAL!` procedure DOES NOT output unquoted forms as:
  ;;
  ;;   `(list '|,| ...)`
  ;;
  ;; and DOES NOT output quoted forms as:
  ;;
  ;;   `(list 'quote (list ...))`
  ;;
  ;; So to ensure that `EQUAL?` can correctly test equality between
  ;; the two results, the result of evaluation needs to be converted
  ;; to a form then back to a list to ensure that
  ;; `<ELISP-QUOTE-SCHEME-TYPE>` and `<ELISP-UNQUOTED-FORM-TYPE>`
  ;; record types are used to represent quoted and unquoted forms
  ;; uniformly, regardless of whether a list or AST form is being
  ;; evaluated.
  ;;------------------------------------------------------------------
  (elisp-form->list
   #t #t
   (list->elisp-form
    #t #t
    (test-elisp-eval! (scheme->elisp expr))
    )))

(define (elisp-test-eval-form! expr)
  ;; Convert an `EXPR`, which must be an AST data structure, and then
  ;; convert the result to a list.
  ;;------------------------------------------------------------------
  (test-elisp-eval! (list->elisp-form expr))
  )

(define (elisp-test-eval-list-and-form! expr)
  ;; Evaluate an `EXPR`, which must be a list data structure, using
  ;; both `ELISP-TEST-EVAL-FORM!` and `ELISP-TEST-EVAL-LIST!` and
  ;; check that the results are the same. This ensures there is no
  ;; difference between evaluating lists and AST forms.
  ;;------------------------------------------------------------------
  (let*((a (elisp-test-eval-list! expr))
        (b (elisp-test-eval-form! expr))
        (ok (equal? a b))
        )
    (cond
     (ok #t)
     (else
      (display "; ERROR: evaluation of list and AST form produces different results:\n")
      (display "; as a list: ") (write a) (newline)
      (display "; as a form: ") (write b) (newline)
      #f))))

(test-assert
    (elisp-test-eval-list-and-form!
     '(let ((a '())) `(',a))
     ))

(test-equal (scheme->elisp '(()))
  (elisp-test-eval-list! '(let ((a '())) `(,a)))
  )

(test-assert
    (elisp-test-eval-list-and-form!
     '(let ((a 1) (b 2)) `('(,a) '(,b)))
     ))

;;--------------------------------------------------------------------------------------------------
;; Testing the debugger

(define debug-expr #f)

(define (debug-start expr)
  (let ((expr (list->elisp-form expr)))
    (set! debug-expr (elisp-debug-eval expr))
    (write-elisp-form expr)
    (newline)
    ))

(define (ds)
  (elisp-debug-show-form debug-expr)
  (let ((return (elisp-debug-step! debug-expr)))
    (elisp-debug-show-result debug-expr)
    return
    ))

(define (capture-debug-step dbg writer)
  (let*((outport (open-output-string))
        (more    (writer dbg outport))
        )
    (values (get-output-string outport) more)
    ))

(define (capture-debug-eval form)
  ;; This runs the stepper and captures every output and every
  ;; intermediate result into a list. The result is a list of "result
  ;; lists" where each result list consists of:
  ;; 
  ;;  1. the evaluation step number
  ;;
  ;;  2. the current form being evaluated
  ;;
  ;;  3. the form result value
  ;;
  ;;  4. a string containing any output to the current-output-port
  ;;     (this is ignored if there is no output string).
  ;;------------------------------------------------------------------
  (let ((log '())
        (dbg (if (debugger-state-type? form) form
                 (elisp-debug-eval form test-elisp-env)
                 )))
    (let loop ((step 0))
      (let*-values
          (((form-str _t)
            (capture-debug-step dbg
                                (lambda (dbg port) (elisp-debug-show-form dbg port) #t)
                                ))
           ((outstr more)
            (capture-debug-step dbg
                                (lambda (dbg _port) (elisp-debug-step! dbg))
                                ))
           ((result) (view dbg =>debugger-last-value*!))
           ((log-item)
            `( ,step ,form-str ,result
               ,@(if (string=? "" outstr) '() (list outstr))
               ))
           )
        (cond
         ((not more) (reverse log))
         (else
          (set! log (cons log-item log))
          (loop (+ 1 step))
          ))))))

(define (compare-debug-eval form expected)
  (define (write-remaining elems)
    (cond
     ((null? elems) #f)
     (else
      (write (car elems)) (newline)
      (write-remaining (cdr elems))
      )))
  (let loop
      ((results (capture-debug-eval form))
       (expected expected)
       )
    (cond
     ((null? expected)
      (cond
       ((null? results) #t)
       (else
        (display "Debugger test stopped prematurely.") (newline)
        (display "The following outputs were expected but not produced:") (newline)
        (write-remaining results)
        )))
     (else
      (cond
       ((null? results)
        (display "Debugger test stopped prematurely.") (newline)
        (display "The following outputs were expected but not produced:") (newline)
        (write-remaining expected)
        )
       ((equal? (car results) (car expected))
        (loop (cdr results) (cdr expected))
        )
       (else
        (display "Unexpected debugger output.") (newline)
        (display "  expected: ") (write (car expected)) (newline)
        (display "    actual: ") (write (car results)) (newline)
        #f
        ))))))

(define debugger-test-form
  '(let ((a 3) (b 6))
     (+ 1 2 (+ a 4) (+ 5 b (* 7 8)) (* 9 10) 11)
     ))

(test-assert
    (compare-debug-eval
     debugger-test-form
     '((0 "(let ((a 3) (b 6)) (+ 1 2 (+ a 4) (+ 5 b (* 7 8)) (* 9 10) 11))\n" #f)
       (1 "3\n" 3)
       (2 "3\n" 3)
       (3 "6\n" 6)
       (4 "6\n" 6)
       (5 "(+ 1 2 (+ a 4) (+ 5 b (* 7 8)) (* 9 10) 11)\n" #f)
       (6 "1\n" 1)
       (7 "1\n" 1)
       (8 "2\n" 2)
       (9 "2\n" 2)
       (10 "(+ a 4)\n" #f)
       (11 "a\n" 3)
       (12 "a\n" 3)
       (13 "4\n" 4)
       (14 "4\n" 7)
       (15 "(+ a 4)\n" 7)
       (16 "(+ 5 b (* 7 8))\n" #f)
       (17 "5\n" 5)
       (18 "5\n" 5)
       (19 "b\n" 6)
       (20 "b\n" 6)
       (21 "(* 7 8)\n" #f)
       (22 "7\n" 7)
       (23 "7\n" 7)
       (24 "8\n" 8)
       (25 "8\n" 56)
       (26 "(* 7 8)\n" 67)
       (27 "(+ 5 b (* 7 8))\n" 67)
       (28 "(* 9 10)\n" #f)
       (29 "9\n" 9)
       (30 "9\n" 9)
       (31 "10\n" 10)
       (32 "10\n" 90)
       (33 "(* 9 10)\n" 90)
       (34 "11\n" 11)
       (35 "11\n" 178)
       (36 "(+ 1 2 (+ a 4) (+ 5 b (* 7 8)) (* 9 10) 11)\n" 178)
       )))

(test-equal 178
  (let ((dbg (elisp-debug-eval debugger-test-form test-elisp-env)))
    (elisp-debug-continue! dbg)
    (view dbg =>debugger-last-value*!)
    ))

(test-equal 3
  (let ((dbg (elisp-debug-eval debugger-test-form test-elisp-env)))
    (elisp-debug-set-break! dbg 'b)
    (elisp-debug-set-break! dbg 'a)
    (elisp-debug-clear-break! dbg 'b)
    (elisp-debug-continue! dbg)
    (elisp-debug-continue! dbg)
    (view dbg =>debugger-last-value*!)
    ))

;;--------------------------------------------------------------------------------------------------
;; Testing `WHILE`, `DOTIMES`, and `DOLIST`

(test-assert
    (test-run
     eqv? 45 test-elisp-eval!
     '(let ((sum 0) (i 0))
        (while (< i 10)
          (setq sum (+ sum i))
          (setq i (|1+| i)))
        sum)))

(test-assert
    (test-run
     eqv? 55 test-elisp-eval!
     '(let ((sum 0))
        (dotimes (n 11 sum)
          (setq sum (+ sum n))
          nil))))

(test-assert
    (test-run
     eqv? (+ 1 1 2 3 5 8 13 21 34) test-elisp-eval!
     '(let ((sum 0))
        (dolist (n '(1 1 2 3 5 8 13 21 34) sum)
                (setq sum (+ n sum))
                nil))))

;;--------------------------------------------------------------------------------------------------
;; Testing `&OPTIONAL` keyword in `DEFUN`

(define (defun-test-optargs . exprs)
  `(progn
    (defun test-optargs (&optional x y)
      (cond
       ((and (null x) (null y)) '(17 23))
       ((null x) (list y y))
       ((null y) (list x x))
       (t (list (+ x y) (* x y)))
       ))
    (test-optargs ,@exprs)
    ))

(test-assert
    (test-run
     equal? '(17 23) test-elisp-eval!
     (defun-test-optargs)
     ))

(test-assert
    (test-run
     equal? '(29 29) test-elisp-eval!
     (defun-test-optargs 29)
     ))

(test-assert
    (test-run
     equal? '(31 31) test-elisp-eval!
     (defun-test-optargs 'nil 31)
     ))

(test-assert
    (test-run
     equal? '(12 35) test-elisp-eval!
     (defun-test-optargs 5 7)
     ))

(define (defun-test-restargs . exprs)
  `(progn
    (defun test-restargs (&optional x y &rest args)
      (list (+ (if x x 0) (if y y 0)) (apply '+ args))
      )
    (test-restargs ,@exprs)
    ))

(test-assert (test-run equal? '(0  0) test-elisp-eval! (defun-test-restargs)))
(test-assert (test-run equal? '(3  0) test-elisp-eval! (defun-test-restargs 3)))
(test-assert (test-run equal? '(8  0) test-elisp-eval! (defun-test-restargs 3 5)))
(test-assert (test-run equal? '(8  8) test-elisp-eval! (defun-test-restargs 3 5 8)))
(test-assert (test-run equal? '(8 21) test-elisp-eval! (defun-test-restargs 3 5 8 13)))
(test-assert (test-run equal? '(8 42) test-elisp-eval! (defun-test-restargs 3 5 8 13 21)))

(test-assert
    (test-run
     equal? '(0 . 1) test-elisp-eval!
     '(let ((pair (cons nil nil)))
        (setcar pair 0)
        (setcdr pair 1)
        pair
        )))

;;--------------------------------------------------------------------------------------------------
;; Testing `PRINC`, `FORMAT`, and `MESSAGE`.

(define (test-elisp-eval-both-ports! expr)
  (call-with-port (open-output-string)
    (lambda (out)
      (call-with-port (open-output-string)
        (lambda (err)
          (parameterize
              ((*elisp-output-port* out)
               (*elisp-error-port*  err)
               )
            (let ((result (test-elisp-eval! expr)))
              (list result (get-output-string out) (get-output-string err))
              )))))))

(define (test-elisp-eval-out-port! expr)
  (call-with-port (open-output-string)
    (lambda (out)
      (parameterize
          ((*elisp-output-port* out))
        (let ((result (test-elisp-eval! expr)))
          (list result (get-output-string out))
          )))))

(test-equal (list "Hello, world!" "Hello, world!")
  (test-elisp-eval-out-port!
   '(princ "Hello, world!")
   ))

(test-equal (list (list "a" "b" "c") "(a b c)")
  (test-elisp-eval-out-port!
   '(princ (list "a" "b" "c"))
   ))

(test-equal (list "Hello, world!\n" "\"Hello, world!\\n\"")
  (test-elisp-eval-out-port!
   '(prin1 "Hello, world!\n")
   ))

(test-equal (list (list "a" "b" "c") "(\"a\" \"b\" \"c\")")
  (test-elisp-eval-out-port!
   '(prin1 (list "a" "b" "c"))
   ))

(test-equal "Hello, world!"
  (test-elisp-eval!
   '(format "Hello, %s" "world!")
   ))

(test-equal "Hello 1234"
  (test-elisp-eval!
   '(format "Hello %S" 1234)
   ))

(test-equal (list '() "" "Hello, world!\n")
  (test-elisp-eval-both-ports!
   '(message "Hello, %s" "world!")
   ))

;;--------------------------------------------------------------------------------------------------
;; Testing lexical and dynamic scoping

(define test-elisp-progn-var-scope-test
  (list->elisp-form
   '(progn
     (princ "------------------------------\n")
     (setq glo "top")
     (defun printglo (who)
       (princ (format "%s: glo = %s\n" who glo))
       nil)
     (defun runfn (sym)
       (princ (format "--begin-- %s\n" sym))
       (funcall sym)
       (princ (format "----end-- %s\n" sym))
       )
     (defun fn-A ()
       (printglo 'fn-A)
       (setq glo "in-fn-A")
       (printglo 'fn-A)
       )
     (defun fn-B ()
       (printglo 'fn-B)
       (let ((glo "in-fn-B"))
         (printglo 'fn-B-let1)
         (runfn 'fn-A)
         (printglo 'fn-B-let2)
         (setq glo "fn-B-after-setq")
         (printglo 'fn-b-let3)
         (runfn 'fn-A)
         (printglo 'fn-b-let4)
         )
       (printglo 'fn-B)
       )
     (runfn 'fn-A)
     (printglo 'top)
     (setq glo "top")
     (printglo 'top-reset-A)
     (runfn 'fn-B)
     (printglo 'top)
     (princ "------------------------------\n")
     t)))

(define lexical-scope-test-expected-result
  "------------------------------
--begin-- fn-A
fn-A: glo = top
fn-A: glo = in-fn-A
----end-- fn-A
top: glo = in-fn-A
top-reset-A: glo = top
--begin-- fn-B
fn-B: glo = top
fn-B-let1: glo = top
--begin-- fn-A
fn-A: glo = top
fn-A: glo = in-fn-A
----end-- fn-A
fn-B-let2: glo = in-fn-A
fn-b-let3: glo = in-fn-A
--begin-- fn-A
fn-A: glo = in-fn-A
fn-A: glo = in-fn-A
----end-- fn-A
fn-b-let4: glo = in-fn-A
fn-B: glo = in-fn-A
----end-- fn-B
top: glo = in-fn-A
------------------------------
")

(define dynamic-scope-test-expected-result
  "------------------------------
--begin-- fn-A
fn-A: glo = top
fn-A: glo = in-fn-A
----end-- fn-A
top: glo = in-fn-A
top-reset-A: glo = top
--begin-- fn-B
fn-B: glo = top
fn-B-let1: glo = in-fn-B
--begin-- fn-A
fn-A: glo = in-fn-B
fn-A: glo = in-fn-A
----end-- fn-A
fn-B-let2: glo = in-fn-A
fn-b-let3: glo = fn-B-after-setq
--begin-- fn-A
fn-A: glo = fn-B-after-setq
fn-A: glo = in-fn-A
----end-- fn-A
fn-b-let4: glo = in-fn-A
fn-B: glo = top
----end-- fn-B
top: glo = top
------------------------------
")

(test-equal (list #t lexical-scope-test-expected-result)
  (test-elisp-eval-out-port! test-elisp-progn-var-scope-test)
  )

(lens-set #f test-elisp-env =>env-lexical-mode?!)

(test-equal (list #t dynamic-scope-test-expected-result)
  (test-elisp-eval-out-port! test-elisp-progn-var-scope-test)
  )

(lens-set #t test-elisp-env =>env-lexical-mode?!)

;;--------------------------------------------------------------------------------------------------
;; Testing `DEFALIAS`

(test-assert
    (test-run
     equal? '(1 1 2 2 2 1 1) test-elisp-eval! 
     '(progn
       (unintern 'abcd)
       (unintern 'bcde)
       (defun abcd () 1)
       (defalias 'bcde 'abcd)
       (let ((a (abcd))
             (b (bcde)))
         (defun abcd () 2)
         (let ((c (abcd))
               (d (bcde)))
           (unintern 'abcd)
           (let ((e (bcde)))
             (defun abcd () 1)
             (let ((f (abcd))
                   (g (bcde)))
               (list a b c d e f g)
               )))))))

;;--------------------------------------------------------------------------------------------------
;; Testing hooks

(test-assert
    (test-elisp-eval!
     '(progn
       (defvar hook-test-var 0 "test hook functions")
       (defun test-hook-success+1 (&optional n)
         (unless n (setq n 1))
         (setq hook-test-var (+ n hook-test-var))
         t)
       (defun test-hook-failure+1 (&optional n)
         (unless n (setq n 1))
         (setq hook-test-var (+ n hook-test-var))
         nil)
       (defvar test-hook-A 'test-hook-success+1)
       (defvar test-hook-B 'test-hook-failure+1)
       (defvar test-hook-ABA
         '(test-hook-success+1 test-hook-failure+1 test-hook-success+1)
         )
       (defvar test-hook-BAB
         '(test-hook-failure+1 test-hook-success+1 test-hook-failure+1)
         )
       t)))

(test-assert
    (test-run
     eqv? 1 test-elisp-eval!
     '(progn
       (setq hook-test-var 0)
       (run-hooks 'test-hook-A)
       hook-test-var
       )))

(test-assert
    (test-run
     eqv? 8 test-elisp-eval!
     '(progn
       (setq hook-test-var 0)
       (run-hooks 'test-hook-ABA 'test-hook-B 'test-hook-BAB 'test-hook-A)
       hook-test-var
       )))

(test-assert
    (test-run
     eqv? 0 test-elisp-eval!
     ;; In order to make sure hooks are as confusing as possible, Emacs
     ;; `RUN-HOOKS` requires a symbol that evaluates to a symbol that was
     ;; defined with `DEFUN`, or a symbol to a list to a symbol defined
     ;; with `DEFUN`. You cannot simply pass a symbol that was defined
     ;; with `DEFUN`, nor can you pass a symbol to a symbol to a symbol
     ;; to a `DEFUN`. The symbol dereferencing must use *exactly* 2
     ;; levels of indirection, and the first level of indirection *must*
     ;; resolve to either a symbol or a list of symbols.
     '(progn
       (setq hook-test-var 0)
       (run-hooks
        test-hook-B
        test-hook-A
        'test-hook-success+1
        'test-hook-failure+1
        )
       hook-test-var
       )))

(test-assert
    (test-run
     equal? '(4 3) test-elisp-eval!
     '(list
       (progn
        (setq hook-test-var 0)
        (run-hook-with-args-until-failure 'test-hook-A)
        (run-hook-with-args-until-failure 'test-hook-B)
        (run-hook-with-args-until-failure 'test-hook-ABA)
        hook-test-var
        )
       (progn
        (setq hook-test-var 0)
        (run-hook-with-args-until-failure 'test-hook-A)
        (run-hook-with-args-until-failure 'test-hook-B)
        (run-hook-with-args-until-failure 'test-hook-BAB)
        hook-test-var
        ))))

(test-assert
    (test-run
     equal? '(3 4) test-elisp-eval!
     '(list
       (progn
        (setq hook-test-var 0)
        (run-hook-with-args-until-success 'test-hook-A)
        (run-hook-with-args-until-success 'test-hook-B)
        (run-hook-with-args-until-success 'test-hook-ABA)
        hook-test-var
        )
       (progn
        (setq hook-test-var 0)
        (run-hook-with-args-until-success 'test-hook-A)
        (run-hook-with-args-until-success 'test-hook-B)
        (run-hook-with-args-until-success 'test-hook-BAB)
        hook-test-var
        ))))

;;--------------------------------------------------------------------------------------------------
;; Testing `FEATUREP` and `REQUIRE`

(test-assert (not (test-elisp-eval! '(featurep 'feature-A))))
(test-assert (test-elisp-eval! '(progn (provide 'feature-A 'subfeature-B 'subfeature-C) t)))
(test-assert (test-elisp-eval! '(featurep 'feature-A)))
(test-assert (not (test-elisp-eval! '(featurep 'feature-A 'wrong-subfeature))))
(test-assert (test-elisp-eval! '(featurep 'feature-A 'subfeature-B)))
(test-assert (test-elisp-eval! '(featurep 'feature-A 'subfeature-C)))
(test-eq 'feature-A (test-elisp-eval! '(require 'feature-A)))

;;--------------------------------------------------------------------------------------------------
;; Testing other built-in functions

(test-equal '(one two three four)
  (test-elisp-eval!
   '(progn
     (setq a '(zero zero zero one two three zero four zero))
     (delq 'zero a)
     )))

(test-equal '(one two three four)
  (test-elisp-eval!
   '(progn
     (setq a '(zero one two three zero four))
     (delq 'zero a)
     )))

(test-equal '(one two three four)
  (test-elisp-eval!
   '(progn
     (setq a '(one two three four))
     (delq 'zero a)
     )))

(test-equal '(1 2 3)
  (test-elisp-eval! '(mapcar (lambda (x) (+ x 1)) '(0 1 2)))
  )

(test-equal '(16 9 4 1 0 1 4)
  (test-elisp-eval!
   '(let ((p (function (lambda (x) (+ (* x x) (* -2 x) 1)))))
      (mapcar p '(-3 -2 -1 0 1 2 3))
      )))

(test-equal '(#t #t #t #t #t)
  (test-elisp-eval! '(mapcar (function symbolp) '(zero one two three four)))
  )

(test-equal '((+ 1 1) (+ 1 2) (+ 2 3) (+ 3 5) (+ 5 8))
  (test-elisp-eval!
   '(mapcar (lambda (x) x) '((+ 1 1) (+ 1 2) (+ 2 3) (+ 3 5) (+ 5 8)))
   ))

(test-equal '()
  (test-elisp-eval! '(delq t (mapcar (function symbolp) '(zero one two three four))))
  )

(test-assert (test-elisp-eval! '(memq 'b '(a b c))))
(test-assert (test-elisp-eval! '(memv 'b '(a b c))))

(define named-numbers ''((zero . 0) (one . 1) (two . 2) (three . 3) (four . 4)))

(test-equal '(zero . 0) (test-elisp-eval! (scheme->elisp `(assq 'zero ,named-numbers))))
(test-equal '(four . 4) (test-elisp-eval! (scheme->elisp `(assq 'four ,named-numbers))))
(test-equal '(zero . 0) (test-elisp-eval! (scheme->elisp `(rassq 0 ,named-numbers))))
(test-equal '(four . 4) (test-elisp-eval! (scheme->elisp `(rassq 4 ,named-numbers))))

;;--------------------------------------------------------------------------------------------------

(test-end "schemacs_elisp_eval_tests")

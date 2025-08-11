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
        )
  (only (schemacs hash-table)
        hash-table-size
        hash-table-ref/default
        alist->hash-table
        )
  )

(test-begin "schemacs_elisp_eval_tests")

;;--------------------------------------------------------------------------------------------------

(define test-elisp-env (new-environment (*elisp-init-env*)))

;; Raw results of evaluation
(define (test-elisp-reset-env!)
  (set! test-elisp-env (new-environment (*elisp-init-env*))))

(define (test-elisp-eval! expr)
  (elisp-eval! expr test-elisp-env))

(define (test-error-elisp-eval! expr)
  (let ((result (test-elisp-eval! expr)))
    (and (elisp-eval-error-type? result)
         (view result =>elisp-eval-error-message))))


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
        (symbol-name a))))

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
  (elisp-eval!
   (scheme->elisp
    '(quasiquote (1 2 (unquote (+ 1 2))))
    )))

(test-equal '() (elisp-eval! (scheme->elisp '(quasiquote ()))))

(test-equal '(1 2 3) (elisp-eval! '(|`| (1 2 (|,| (+ 1 2))))))

(test-equal '(1 2 3) (elisp-eval! (car (scheme->elisp '(`(1 2 ,(+ 1 2)))))))

(test-equal '((+ 3 5) = 8)
  (elisp-eval!
   (scheme->elisp
    '(quasiquote
      ((+ ,(+ 1 2) ,(+ 2 3)) = ,(+ 1 2 2 3))))))

;;--------------------------------------------------------------------------------------------------
;; `LAMBDA`, `DEFUN`, `APPLY`, and `FUNCALL` tests.

(test-assert
  (let ((func (test-elisp-eval! '(lambda () nil))))
    (and
     (null? (view func =>lambda-args!))
     (null? (view func =>lambda-optargs!))
     (not (view func =>lambda-rest!))
     (equal? '(nil) (view func =>lambda-body!)))
    ))

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
     equal? '(2 + 3 = 5) test-elisp-eval!
     '(progn
       (setq a 5 b 8)
       (defun f (a b) (list a '+ b '= (+ a b)))
       (f 2 3)
       )))

(test-equal '(13 + 21 = 34)
  ;; `TEST-EQUAL` is used here because the Scheme form input must be
  ;; converted to Elisp using `SCHEME->ELSIP` which encodes
  ;; quasiquotes in a way that is compatible with Emacs Lisp, and is a
  ;; different encoding from `LIST->ELISP-FORM`. So we test the
  ;; `ELISP-FORM-TYPE?` encoding on it's own here.
  (elisp-form->list
   (elisp-eval!
    (list->elisp-form
     '(progn
       (setq x 13 y 21)
       (defmacro mac1 (a b) `(list ,a '+ ,b '= (+ ,a ,b)))
       (mac1 x y)
       )))))

(test-equal '(13 + 21 = 34)
  ;; `TEST-EQUAL` is used here because the Scheme form input must be
  ;; converted to Elisp using `SCHEME->ELSIP` which encodes
  ;; quasiquotes in a way that is compatible with Emacs Lisp, and is a
  ;; different encoding from `LIST->ELISP-FORM`. So we test the
  ;; `LIST?` encoding on it's own here.
  (elisp-eval!
   (scheme->elisp
    '(progn
      (setq x 13 y 21)
      (defmacro mac1 (a b) `(list ,a '+ ,b '= (+ ,a ,b)))
      (mac1 x y)
      ))))

(test-equal '(list 13 '+ 21 '= (+ 13 21))
  (elisp-eval!
   (scheme->elisp
    '(progn
      (defmacro mac1 (a b) `(list ,a '+ ,b '= (+ ,a ,b)))
      (macroexpand '(mac1 13 21))
      ))))


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

(test-assert
  (test-elisp-eval!
   '(progn
      (defun test-optargs (&optional x y)
        (cond
         ((and (null x) (null y)) '(17 23))
         ((null x) (list y y))
         ((null y) (list x x))
         (t (list (+ x y) (* x y)))
         ))
      t
      )))

(test-assert
    (test-run
     equal? '(17 23) test-elisp-eval!
     '(test-optargs)
     ))

(test-assert
    (test-run
     equal? '(29 29) test-elisp-eval!
     '(test-optargs 29)
     ))

(test-assert
    (test-run
     equal? '(31 31) test-elisp-eval!
     '(test-optargs nil 31)
     ))

(test-assert
    (test-run
     equal? '(12 35) test-elisp-eval!
     '(test-optargs 5 7)
     ))


(test-assert
  (test-elisp-eval!
   '(defun test-restargs (&optional x y &rest args)
      (list (+ (if x x 0) (if y y 0)) (apply '+ args))
      )))

(test-assert (test-run equal? '(0  0) test-elisp-eval! '(test-restargs)))
(test-assert (test-run equal? '(3  0) test-elisp-eval! '(test-restargs 3)))
(test-assert (test-run equal? '(8  0) test-elisp-eval! '(test-restargs 3 5)))
(test-assert (test-run equal? '(8  8) test-elisp-eval! '(test-restargs 3 5 8)))
(test-assert (test-run equal? '(8 21) test-elisp-eval! '(test-restargs 3 5 8 13)))
(test-assert (test-run equal? '(8 42) test-elisp-eval! '(test-restargs 3 5 8 13 21)))


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
   '(princ "Hello, world!")))

(test-equal (list (list "a" "b" "c") "(a b c)")
  (test-elisp-eval-out-port!
   '(princ (list "a" "b" "c"))))

(test-equal (list "Hello, world!\n" "\"Hello, world!\\n\"")
  (test-elisp-eval-out-port!
   '(prin1 "Hello, world!\n")))

(test-equal (list (list "a" "b" "c") "(\"a\" \"b\" \"c\")")
  (test-elisp-eval-out-port!
   '(prin1 (list "a" "b" "c"))))

(test-equal "Hello, world!"
  (test-elisp-eval!
   '(format "Hello, %s" "world!")))

(test-equal "Hello 1234"
  (test-elisp-eval!
   '(format "Hello %S" 1234)))

(test-equal (list '() "" "Hello, world!\n")
  (test-elisp-eval-both-ports!
   '(message "Hello, %s" "world!")))

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
       (printglo 'fn-B))
     (runfn 'fn-A)
     (printglo 'top)
     (setq glo "top")
     (printglo 'top-reset-A)
     (runfn 'fn-B)
     (printglo 'top)
     (princ "------------------------------\n")
     t))
  )

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
  (test-elisp-eval-out-port! test-elisp-progn-var-scope-test))

(lens-set #f test-elisp-env =>env-lexical-mode?!)

(test-equal (list #t dynamic-scope-test-expected-result)
  (test-elisp-eval-out-port! test-elisp-progn-var-scope-test))

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
      (defvar ramin-hook-test 0 "test hook functions")
      (defun ramin-hook-success+1 (&optional n)
        (unless n (setq n 1))
        (setq ramin-hook-test (+ n ramin-hook-test))
        t)
      (defun ramin-hook-failure+1 (&optional n)
        (unless n (setq n 1))
        (setq ramin-hook-test (+ n ramin-hook-test))
        nil)
      (defvar ramin-hook-A 'ramin-hook-success+1)
      (defvar ramin-hook-B 'ramin-hook-failure+1)
      (defvar ramin-hook-ABA
        '(ramin-hook-success+1 ramin-hook-failure+1 ramin-hook-success+1))
      (defvar ramin-hook-BAB
        '(ramin-hook-failure+1 ramin-hook-success+1 ramin-hook-failure+1))
      t)
   ))

(test-assert
    (test-run
     eqv? 1 test-elisp-eval!
     '(progn
       (setq ramin-hook-test 0)
       (run-hooks 'ramin-hook-A)
       ramin-hook-test
       )))

(test-assert
    (test-run
     eqv? 8 test-elisp-eval!
     '(progn
       (setq ramin-hook-test 0)
       (run-hooks 'ramin-hook-ABA 'ramin-hook-B 'ramin-hook-BAB 'ramin-hook-A)
       ramin-hook-test
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
       (setq ramin-hook-test 0)
       (run-hooks
        ramin-hook-B
        ramin-hook-A
        'ramin-hook-success+1
        'ramin-hook-failure+1
        )
       ramin-hook-test
       )))

(test-assert
    (test-run
     equal? '(4 3) test-elisp-eval!
     '(list
       (progn
        (setq ramin-hook-test 0)
        (run-hook-with-args-until-failure 'ramin-hook-A)
        (run-hook-with-args-until-failure 'ramin-hook-B)
        (run-hook-with-args-until-failure 'ramin-hook-ABA)
        ramin-hook-test
        )
       (progn
        (setq ramin-hook-test 0)
        (run-hook-with-args-until-failure 'ramin-hook-A)
        (run-hook-with-args-until-failure 'ramin-hook-B)
        (run-hook-with-args-until-failure 'ramin-hook-BAB)
        ramin-hook-test
        ))))

(test-assert
    (test-run
     equal? '(3 4) test-elisp-eval!
     '(list
       (progn
        (setq ramin-hook-test 0)
        (run-hook-with-args-until-success 'ramin-hook-A)
        (run-hook-with-args-until-success 'ramin-hook-B)
        (run-hook-with-args-until-success 'ramin-hook-ABA)
        ramin-hook-test
        )
       (progn
        (setq ramin-hook-test 0)
        (run-hook-with-args-until-success 'ramin-hook-A)
        (run-hook-with-args-until-success 'ramin-hook-B)
        (run-hook-with-args-until-success 'ramin-hook-BAB)
        ramin-hook-test
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
  (test-elisp-eval! '(mapcar (lambda (x) (+ x 1)) '(0 1 2))))

(test-equal '(#t #t #t #t #t)
  (test-elisp-eval! '(mapcar (function symbolp) '(zero one two three four))))

(test-equal '()
  (test-elisp-eval! '(delq t (mapcar (function symbolp) '(zero one two three four)))))

(test-assert (test-elisp-eval! '(memq 'b '(a b c))))
(test-assert (test-elisp-eval! '(memv 'b '(a b c))))

;;--------------------------------------------------------------------------------------------------

(test-end "schemacs_elisp_eval_tests")

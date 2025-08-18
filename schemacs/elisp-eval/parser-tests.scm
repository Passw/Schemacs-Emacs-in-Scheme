(import
  (scheme base)
  (scheme case-lambda)
  (only (schemacs lens vector) mutable-vector-for-each)
  (only (srfi 64) test-begin test-end test-assert test-equal)
  (only (schemacs lexer) lexer-state lex-all)
  (schemacs elisp-eval parser)
  )

;;--------------------------------------------------------------------------------------------------

(define token-error         0)
(define token-eof           1)
(define token-space         2)
(define token-open-bracket  3)
(define token-close-bracket 4)
(define token-symbol        5)
(define token-dot           6)
(define token-int           7)
(define token-float         8)
(define token-string        9)
(define token-char         10)
(define token-quote        11)
(define token-backquote    12)
(define token-unquote      13)
(define token-splice       14)
(define token-hashcode     15)


(define token-type-names
  (vector
   "error" "EOF" "space" "open-bracket" "close-bracket" "symbol" "dot" "int"
   "float" "string" "char" "quote" "backquote" "unquote" "splice" "hashcode"))


(define (token-type-name id)
  (if (< id (vector-length token-type-names))
      (vector-ref token-type-names id)
      (string-append "unknown:" (number->string id))
      ))

;;------------------------------------------------------------------------------

(define (test-lexer input-string expected-type expected-token)
  (let*-values
      (((st) (lexer-state input-string))
       ((token-type token) (run-elisp-tokenizer st))
       ((success)
        (and (equal? expected-type token-type)
             (equal? expected-token token)))
       )
    (cond
     (success #t)
     (else
      (display ";;   on input: ")(write input-string)(newline)
      (display ";;  expecting: ")(write (token-type-name expected-type))
      (display ": ")(write expected-token)(newline)
      (display ";; got result: ")(write (token-type-name token-type))
      (display ": ")(write token)(newline)
      (display ";;      state: ")(write st)(newline)
      (display ";;----------------------------------------------------------\n")
      #f
      ))))


(define all-lexer-test-cases
  (list
   ;;------------- numerical literal tests ---------------
   (list "0" token-int 0)
   (list "+0" token-int 0)
   (list "-0" token-int 0)
   (list "1" token-int 1)
   (list "+1" token-int 1)
   (list "-1" token-int -1)
   (list "2691979" token-int 2691979)
   (list "-2691979" token-int -2691979)
   (list "+2691979" token-int 2691979)
   (list "269.1979" token-float 2691979/10000)
   (list "-269.1979" token-float -2691979/10000)
   (list "+269.1979" token-float 2691979/10000)
   (list "269.1979e4" token-float 2691979)
   (list "-269.1979e4" token-float -2691979)
   (list "+269.1979e4" token-float 2691979)
   (list "269.1979E4" token-float 2691979)
   (list "269.1979e-2" token-float 2691979/1000000)
   (list "269.1979E-2" token-float 2691979/1000000)
   (list "1.0" token-float 1)
   (list "1.0e10" token-float 10000000000)
   (list "1.0e-10" token-float 1/10000000000)
   (list ".0" token-float 0)
   (list ".1" token-float 1/10)
   (list ".2691979" token-float 2691979/10000000)
   (list "-.0" token-float 0)
   (list "+.0" token-float 0)
   (list ".0e100" token-float 0)
   (list ".0e-100" token-float 0)
   (list ".2691979E3" token-float 2691979/10000)
   ;;------------- character literal tests ---------------
   (list "?a" token-char #\a)
   (list "?\\n" token-char #\newline)
   (list "?\\u03bb" token-char #\x3bb)
   (list "?\\U000003bb" token-char #\x3bb)
   (list "?\\N{U+3BB}" token-char #\x3bb)
   (list "?\\N{GREEK SMALL LETTER LAMBDA}" token-char #\x3bb)
   (list "?\\^I" token-char #\tab)
   (list "?\\^i" token-char #\tab)
   (list "?\\C-i" token-char #\tab)
   (list "?\\C-?" token-char #\delete)
   (list "?\\^+"  token-char #x400002B)
   ;;-------------- string literal tests ---------------
   (list "\"Hello, world!\"" token-string "Hello, world!")
   (list "\"Hello, world!\\n\"" token-string "Hello, world!\n")
   (list "\"\\\\\\n\\\\\\n\"" token-string "\\\n\\\n")
   ;;----------------- comment tests -------------------
   (list "  ; Comment\n  ; hello\n" token-space #t)
   ))


(define (run-lexer-tests test-cases)
  (let loop ((failed 0) (passed 0) (test-cases test-cases))
    (cond
     ((null? test-cases)
      (unless (= 0 failed)
        (display "(passed ")(write passed)(display ")\n")
        (display "(failed ")(write failed)(display ")\n")
        )
      (= 0 failed)
      )
     (else
      (let*((result (apply test-lexer (car test-cases)))
            (failed (if result failed (+ 1 failed)))
            (passed (if result (+ 1 passed) passed))
            )
        (loop failed passed (cdr test-cases))
        )))))

(define (compare-square-form result expected)
  (and (equal? expected (elisp-form->list result))
       (square-bracketed-form? result)
       ))

;;--------------------------------------------------------------------------------------------------

(define all-parser-test-cases
  (list
   (list "1290" 1290 =)
   (list "12.34" 617/50 =)
   (list "12.34e2" 1234 =)
   (list "12.34e-1" 617/500 =)
   (list "\"Hello, world!\\n\"" "Hello, world!\n" string=?)
   (list "(+ 1 2)" (list->elisp-form '(+ 1 2)) elisp-form-equal?)
   (list "(\"zero\" . .0e+0)" (list->elisp-form '("zero" . 0)) elisp-form-equal?)
   (list "[]" '() compare-square-form)
   (list "[1 2 3]" '(1 2 3) compare-square-form)
   (list "'(1 2 3)"
    (elisp-quote-scheme (list->elisp-form '(1 2 3)) #f)
    (elisp-quote-scheme-equal? elisp-form-equal?)
    )
   (list
    "`(1 2 ,(three) ,@four)"
    (elisp-quote-scheme
     (list->elisp-form
      (list 1 2
       (elisp-unquoted-form (list->elisp-form '(three)) #f)
       (elisp-unquoted-form 'four #t)
       ))
     #t)
    (elisp-quote-scheme-equal? elisp-form-equal?)
    )
   (list "#b00001010" 10 =)
   (list "#x007F" 127 =)
   (list "#o077" 63 =)
   (list "#'+" (make<elisp-function-ref> #f '+) elisp-form-equal?)
   (list
    "(defun test-defun (arg1 &optional arg2 &rest rest-args)  \"This is a test function\\nwith a multi-line docstring.\"\n  (ignore)\n  )\n"
    (list->elisp-form '(defun test-defun (arg1 &optional arg2 &rest rest-args) "This is a test function\nwith a multi-line docstring." (ignore)))
    elisp-form-equal?
    )))


(define test-parser
  (case-lambda
    ((input-string expected-value)
     (test-lexer input-string expected-value equal?)
     )
    ((input-string expected-value equal?)
     (let*((st (parse-state input-string))
           (result (elisp-read st))
           )
       (cond
        ((equal? result expected-value) #t)
        (else
         (display ";;   on input: ")(write input-string)(newline)
         (display ";;  expecting: ")(write expected-value)(newline)
         (display ";; got result: ")(write result)(newline)
         (display ";;      state: ")(write st)(newline)
         (display ";;----------------------------------------------------------\n")
         #f
         ))))))


(define (run-parser-tests test-cases)
  (let loop ((test-cases test-cases) (failed 0) (passed 0))
    (cond
     ((null? test-cases)
      (unless (= 0 failed)
        (display "(passed ")(write passed)(display ")\n")
        (display "(failed ")(write failed)(display ")\n")
        )
      (= 0 failed)
      )
     (else
      (let ((result (apply test-parser (car test-cases))))
        (if result
            (loop (cdr test-cases) failed (+ 1 passed))
            (loop (cdr test-cases) (+ 1 failed) passed)
            ))))))

;;--------------------------------------------------------------------------------------------------

(test-begin "schemacs_elisp-eval_parser")

(test-assert (run-lexer-tests all-lexer-test-cases))

(test-assert
    (elisp-form-equal?
     (list->elisp-form '(+ a b))
     (list->elisp-form '(+ a b))))

(test-assert
    (elisp-form-equal?
     (list->elisp-form '(+ a b . 0))
     (list->elisp-form '(+ a b . 0))))

(test-assert
    (not
     (elisp-form-equal?
      (list->elisp-form '(+ a b . 0))
      (list->elisp-form '(+ a b)))))

(test-assert
    (not
     (elisp-form-equal?
      (list->elisp-form '(+ a 1))
      (list->elisp-form '(+ a b)))))

(test-assert
    (elisp-form-equal?
     (list->elisp-form
      '(1234 (+ 11/10 -1 99/10) "Hello, \"world\"!\n")
      )
     (elisp-read-all
      "1234 (+ 1.1 -1.0 990e-2) \"Hello, \\\"world\\\"!\\n\""
      )))

(test-assert (run-parser-tests all-parser-test-cases))

(test-assert
    (eq? 'lexical-binding
         (select-elisp-dialect! (parse-state "   ; -*- lexical-binding:t -*-\n;\n"))
         ))

(test-assert
    (eq? 'lexical-binding
         (select-elisp-dialect! (parse-state "   ;  -*-   lexical-binding  :  t   -*-\n;\n"))
         ))

(test-assert
    (eq? 'lexical-binding
         (select-elisp-dialect! (parse-state ";-*- lexical-binding :t -*-\n;\n"))
         ))

(test-assert
    (eq? 'dynamic-binding
         (select-elisp-dialect! (parse-state ";-*- lexical-binding :nil -*-\n;\n"))
         ))

(test-assert
    (eq? 'dynamic-binding
         (select-elisp-dialect! (parse-state ";-*- lexical-binding:nil -*-"))
         ))

(test-assert
    (not
     (select-elisp-dialect! (parse-state ";-*- lexical-binding:nil"))
     ))

(test-assert
    (not
     (select-elisp-dialect! (parse-state ";-*-lexical-binding:t"))
     ))

(test-assert
    (not
     (select-elisp-dialect! (parse-state ";lexical-binding:t-*-"))
     ))

(test-assert
    (not
     (select-elisp-dialect! (parse-state ";lexical-binding:t-*-"))
     ))

;;--------------------------------------------------------------------------------------------------
;; Source location reporting testing

(define (test-location-reporting expecting run)
  (string=?
   expecting
   (call-with-port (open-output-string)
     (lambda (port) (run port) (get-output-string port)))))

(test-assert
    (let*((in-port (open-input-string "(hello)"))
          (st (parse-state in-port))
          (form (elisp-read st))
          )
      (close-port in-port)
      (and
       (test-location-reporting
        "1:2" (lambda (port) (write-parser-location form port)))
       (test-location-reporting
        "1:8" (lambda (port) (write-parser-location st port)))
       )))

(test-end "schemacs_elisp-eval_parser")

(import
  (scheme base)
  (only (scheme char)
        digit-value char-lower-case? char-upper-case?
        char-whitespace? char-numeric?
        )
  (only (scheme write) display write)
  (only (schemacs lens) view)
  (only (srfi 64) test-begin test-end test-assert test-equal)
  (schemacs lexer)
  )

;;--------------------------------------------------------------------------------------------------

(define (non-space c) (not (char=? c #\space)))
(define (space c) (char=? c #\space))

(define (test-report expected-end-line expected-end-col expected-result st result)
  (let ((end-line (view st =>lexer-line*!))
        (end-col  (view st =>lexer-column*!))
        (success  #t)
        )
    (unless (= end-line expected-end-line)
      (set! success #f)
      (display "expected end-line: ") (write expected-end-line)
      (display ", instead got: ") (write end-line)
      (newline)
      )
    (unless (= end-col expected-end-col)
      (set! success #f)
      (display "expected end-col: ") (write expected-end-col)
      (display ", instead got: ") (write end-col)
      (newline)
      )
    (unless
        (if (procedure? expected-result)
            (expected-result result)
            (equal? result expected-result)
            )
      (set! success #f)
      (display "expected result: ") (write expected-result)
      (display ", instead got: ") (write result)
      (newline)
      )
    success
    ))


(define (lex-all-test expected-end-line expected-end-col expected-result input . lexers)
  (let*((st (lexer-state input))
        (result (apply lex-all st lexers)))
    (test-report expected-end-line expected-end-col expected-result st result)
    ))

;;--------------------------------------------------------------------------------------------------

(test-begin "schemacs_lexer")


(test-assert
    (lex-all-test 1 3 #\a "?a" (lex #\? (any))))


(test-assert
    (lex-all-test 1 6 "hello" "hello world" (many1/buffer (char non-space))))


(test-assert
    (lex-all-test 1 1 #f "hello world" (many1/buffer (char space))))


(test-assert
    (lex-all-test
     1 12 "hello world"
     "hello world"
     (lex/buffer
      (many1/buffer (char non-space))
      (many1/buffer (char space))
      (many1/buffer (char non-space))
      (eof))))


(test-assert
    (lex-all-test 1 6 #\o "hello world" (many1 (char non-space))))


(test-assert
    (lex-all-test 1 1 #f "hello world" (many1 space)))


(test-assert
    (lex-all-test
     1 12 eof-object?
     "hello world"
     (lex
      (many1 non-space)
      (many1 space)
      (many1 non-space)
      (eof)
      )))


(test-assert
    (lex-all-test
     1 6 "hello"
     "hello world" 
     (either
      (many1/buffer (char char-upper-case?))
      (many1/buffer (char char-lower-case?))
      (many1/buffer (char char-upper-case?))
      )))


(define lextab #f)

(define (run-lextab) (if lextab (many1/buffer lextab) (lex #f)))

(test-assert
    (let*((lower (many1/buffer (char char-lower-case?)))
          (upper (many1/buffer (char char-upper-case?)))
          (digit (many1/buffer (char char-numeric?)))
          (space (lex (many1 space) (lex-const #t)))
          )
      (set! lextab
        (alist->parse-table #f
         `(((#\0 . #\9) . ,digit)
           ((#\A . #\Z) . ,upper)
           ((#\a . #\z) . ,lower)
           (" \t\n\r\f\v" . ,space)
           )))
      #t))


(test-assert
    (lex-all-test
     1 6 "hello"
     "hello."
     (lex/buffer (run-lextab))
     ))


(test-assert
    (lex-all-test
     1 7 "SCHEME"
     "SCHEME."
     (lex/buffer (run-lextab))
     ))


(test-assert
    (lex-all-test
     1 8 "1234567"
     "1234567."
     (lex/buffer (run-lextab))
     ))


(test-assert
    (lex-all-test
     1 21 "hello-SCHEME-1234567"
     "hello-SCHEME-1234567"
     (lex/buffer
      (run-lextab) #\-
      (run-lextab) #\-
      (run-lextab)
      )))


(test-assert
    (lex-all-test
     1 6 532
     "hello-SCHEME-1234567"
     (lex-fold 0
      (lambda (sum c) (values #t (+ sum (char->integer c))))
      (char char-lower-case?)
      )))


(test-assert
    (lex-all-test
     1 21 '("1234567" "scheme" "hello")
     "hello-scheme-1234567"
     (lex-fold '()
      (lambda (accum sym)
        (values #t (if (string? sym) (cons sym accum) accum)))
      (run-lextab) #\-
      (run-lextab) #\-
      (run-lextab)
      )))

(test-assert
    (lex-all-test
     1 7 12345
     "0123456789A"
     (lex-fold-count 0
      (lambda (accum count sym)
        (values (< count 5) (+ (* 10 accum) (digit-value sym)))
        )
      (lambda (accum count) accum)
      (char char-numeric?)
      )))


(test-assert
  (lex-all-test
   1 10 "hello" "( hello )"
   (lex-brackets
    (lex #\(  space)
    (lex space #\) )
    (many1/buffer (char char-lower-case?)))
   ))

(test-assert
    (lex-all-test
     2 1 #t ";;----------\n"
     (skip-to-next-line)
     ))

(test-assert
    (lex-all-test
     3 1 #t ";;----------\nhello\nworld!"
     (skip-to-next-line 2)
     ))

(define match-repeat-first2
  (lex/buffer
   (lex-join
    (lex-apply
     (lambda (a b) (lex-put a b (many/buffer a b)))
     (any)
     (any)
     ))))

(test-assert
    (lex-all-test
     1 9 "abababab" "abababab"
     match-repeat-first2
     ))

(test-assert
    (lex-all-test
     1 6 "xyxyx" "xyxyx"
     match-repeat-first2
     ))

(test-assert
    (lex-all-test
     1 3 #\a "abc"
     (lex-first #\a #\b)
     ))

(test-assert
    (lex-all-test
     1 8 12345 "  12345--"
     (lex (many #\space) (lex-digits))
     ))

(test-assert
    (lex-all-test
     1 8 5349 "  12345--"
     (lex (many #\space) (lex-digits 8))
     ))

;;--------------------------------------------------------------------------------------------------
;; Tests for the grep-like algorithm `SCAN-FOR-STRING`.

(define longstr-A "01234567890123abcd89\n0efgh567890123456789\nijkl4567890123456789")
(define longstr-B "a aa aaa aaaa aaaaa aaaaaa aaaaaaa aaaaaaaa")

(define (run-scanner line-limit instr findstr)
  (let*((st (lexer-state instr))
        (result (run-lexer st (scan-for-string line-limit findstr)))
        )
    (if result 
        (cons (view st =>lexer-line*!) (view st =>lexer-column*!))
        #f
        )))

(define (test-scanner line-limit instr findstr expecting)
  (let ((result (run-scanner line-limit instr findstr)))
    (cond
     ((equal? expecting result) #t)
     (else
      (display "; test failed, searching for ")
      (write findstr) (display " in ") (write instr) (newline)
      (display "; returned ") (write result)
      (display " but was expecting ") (write expecting) (newline)
      (display ";----------------------------------------\n")
      #f
      ))))

(test-assert (test-scanner #f longstr-A "abcd" '(1 . 19)))
(test-assert (test-scanner #f longstr-A "efgh" '(2 . 6)))
(test-assert (test-scanner #f longstr-A "ijkl" '(3 . 5)))
(test-assert (test-scanner 1 longstr-A "abcd" '(1 . 19)))
(test-assert (test-scanner 1 longstr-A "ijkl" #f))
(test-assert (test-scanner 2 longstr-A "ijkl" #f))
(test-assert (test-scanner 3 longstr-A "ijkl" '(3 . 5)))
(test-assert (test-scanner #f longstr-A "abcde" #f))
(test-assert (test-scanner #f longstr-A "iefgh" #f))
(test-assert (test-scanner #f longstr-A "ijklm" #f))
(test-assert (test-scanner #f longstr-A "89\nijkl" '(3 . 5)))
(test-assert (test-scanner #f longstr-A "89\nijk" '(3 . 4)))
(test-assert (test-scanner #f longstr-A "9\nijkx" #f))
(test-assert (test-scanner #f longstr-A "x9\nijk" #f))
(test-assert (test-scanner #f longstr-A "a" '(1 . 16)))
(test-assert (test-scanner #f longstr-B "a" '(1 . 2)))
(test-assert (test-scanner #f longstr-B "aaa" '(1 . 9)))
(test-assert (test-scanner #f longstr-B "aaaaaa" '(1 . 27)))
(test-assert (test-scanner #f longstr-B "aaaaaaaa" '(1 . 44)))
(test-assert (test-scanner #f "" "" '(1 . 1)))
(test-assert (test-scanner #f "abc" "" '(1 . 1)))
(test-assert (test-scanner #f "" "a" #f))
(test-assert (test-scanner #f "abcdef" "abcdef" '(1 . 7)))

;;--------------------------------------------------------------------------------------------------
;; Source location reporting testing

(define (test-location-reporting expecting run)
  (string=?
   expecting
   (call-with-port (open-output-string)
     (lambda (port) (run port) (get-output-string port))))
  )

(test-assert
    (test-location-reporting
     "a.scm:12:34"
     (lambda (port)
       (write-lexer-location "a.scm" 12 34 port))))

(test-assert
    (test-location-reporting
     "./b.scm:23:45"
     (lambda (port)
       (write-lexer-location
        (make<source-file-location> "./b.scm" 23 45)
        port
        ))))

(test-assert
    (test-location-reporting
     "1:1"
     (lambda (out-port)
       (call-with-port (open-input-string "")
         (lambda (in-port)
           (write-lexer-location (lexer-state in-port) out-port))))))

;;--------------------------------------------------------------------------------------------------
;; Testing writing direct to output buffer

(test-assert
    (lex-all-test
     1 1 "\"hello world\"" "ignored input string"
     (lex/buffer (with-buffer-port (lambda (buf) (write "hello world" buf))))
     ))


;;--------------------------------------------------------------------------------------------------
;; parse table tests

(define (merge-tables-test)
  (let*((decimal     (alist->parse-table '(((#\0 . #\9) . "decimal"))))
        (octal       (alist->parse-table '(((#\0 . #\7) . "octal"))))
        (uppercase   (alist->parse-table '(((#\A . #\Z) . "uppercase"))))
        (lowercase   (alist->parse-table '(((#\a . #\z) . "lowercase"))))
        (hexadecimal
         (alist->parse-table
          '(((#\0 . #\9) . "hexadecimal")
            ((#\A . #\F) . "hexadecimal")
            ((#\a . #\f) . "hexadecimal"))))
        (join
         (lambda (a b)
           (cond
            ((not a) b)
            ((not b) a)
            (else (string-append a "||" b)))
           ))
        (table
         (alist->parse-table
          `(,decimal
            ,uppercase
            ,lowercase
            (,octal . ,join)
            (,hexadecimal . ,join)
            )))
        )
    (list
     (parse-table-ref table #\f)
     (parse-table-ref table #\7)
     )))

(test-equal '("lowercase||hexadecimal" "decimal||octal||hexadecimal")
  (merge-tables-test)
  )

(define table-1 #f)
(define table-2 #f)

(test-assert
    (begin
      (set! table-1 (alist->parse-table '((#\0 . "zero") (#\1 . "one"))))
      (set! table-2 (alist->parse-table '((#\1 . "ONE")  (#\2 . "two"))))
      #t
      ))

(test-equal '("zero" "one" "ONE" "two")
  (list
   (parse-table-ref table-1 #\0)
   (parse-table-ref table-1 #\1)
   (parse-table-ref table-2 #\1)
   (parse-table-ref table-2 #\2)
   ))

(test-equal '("zero" "ONE" "two")
  (begin
    (parse-tables-merge-into! table-1 table-2)
    (list
     (parse-table-ref table-1 #\0)
     (parse-table-ref table-1 #\1)
     (parse-table-ref table-1 #\2)
     )))


(define (consonant-vowel-action c)
  (lex (any) (with-buffer-port (lambda (out-port) (write-char c out-port) #t))))


(define consonant-vowel-table
  (alist->parse-table
   `(((#\a . #\z) . ,(consonant-vowel-action #\C))
     ("aeiou" . ,(consonant-vowel-action #\V))
     )))

(test-equal "CVCCV" (lex-all "hello" (many/buffer (lex-table consonant-vowel-table))))


(test-end "schemacs_lexer")

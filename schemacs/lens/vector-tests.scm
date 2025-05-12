(import
  (scheme base)
  (schemacs lens)
  (schemacs lens vector)
  (srfi 64))

(test-begin "schemacs_lens_vector")

(define testvec (new-mutable-vector 4))
(test-assert (mutable-vector-empty? testvec))
(test-eqv 4 (mutable-vector-store-size testvec))
(test-eqv 0 (mutable-vector-length testvec))

(define (append-test item size len)
  (mutable-vector-append! testvec item)
  (test-eqv size (mutable-vector-store-size testvec))
  (test-eqv len  (mutable-vector-length testvec)))

(append-test "zero"   4  1)
(append-test "one"    4  2)
(append-test "two"    4  3)
(append-test "three"  4  4)
(append-test "four"   8  5)
(append-test "five"   8  6)
(append-test "six"    8  7)
(append-test "seven"  8  8)
(append-test "eight" 16  9)
(append-test "nine"  16 10)

(test-equal "zero" (view testvec (=>mvector-index! 0)))
(test-equal "nine" (view testvec (=>mvector-index! 9)))

(test-assert '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")
  (mutable-vector->list testvec))

(test-equal '()
  (mutable-vector->list testvec 0))

(test-assert '("zero" "one" "two" "three" "four" "five" "six" "seven")
  (mutable-vector->list testvec 7))

(test-assert '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")
  (mutable-vector->list testvec -7))

(test-assert '("seven" "six" "five" "four" "three" "two")
  (mutable-vector->list testvec 7 2))

(test-assert '("seven" "six" "five" "four" "three" "two" "one" "zero")
  (mutable-vector->list testvec 7 -2))

(define testvec-copy (mutable-vector-copy testvec))

(test-assert
    (let ()
      (lens-set! 4 testvec-copy =>mutable-vector-min-index)
      #t))

(test-equal "four" (view testvec-copy (=>mvector-index! 0)))
(test-equal "nine" (view testvec-copy (=>mvector-index! 5)))

(test-assert '("four" "five" "six" "seven" "eight" "nine")
  (mutable-vector->list testvec-copy))

(test-equal " zero-four one-five two-six three-seven four-eight five-nine"
  (let ((result
         (mutable-vector-fold
          (lambda (a b sum) (string-append sum " " a "-" b))
          "" testvec testvec-copy)))
    result))

(test-equal
    '(("zero" . "four")
      ("one" . "five")
      ("two" . "six")
      ("three" . "seven")
      ("four" . "eight")
      ("five" . "nine"))
  (let ((folded
         (mutable-vector-fold
          (lambda (a b fold) (lambda (next) (fold (cons (cons a b) next))))
          (lambda (final) final)
          testvec
          testvec-copy)))
    (folded '())))

(test-equal "four,five,six,seven,eight,nine,"
  (call-with-port (open-output-string)
    (lambda (port)
      (mutable-vector-for-each
       (lambda (item)
         (display item port)
         (display #\, port))
       testvec-copy)
      (get-output-string port))))

(test-equal '(#t #t #t #t #t #t)
  (let ()
    (mutable-vector-fill! testvec-copy #t)
    (mutable-vector->list testvec-copy)))

(test-equal '()
  (let ()
    (mutable-vector-clear! testvec-copy)
    (mutable-vector->list testvec-copy)))

(mutable-vector-clear! testvec)

(test-equal '() (mutable-vector->list testvec))

(test-equal '("zero" "one" "two")
  ;; Check that appending to a mutable vector initially of size zero
  ;; works correctly.
  (let ((v (new-mutable-vector 0)))
    (mutable-vector-append! v)
    (mutable-vector-append! v "zero")
    (mutable-vector-append! v "one" "two")
    (mutable-vector->list v)))

(mutable-vector-append! testvec
 "zero" "one" "two" "three" "four" "five" "six"
 "seven" "eight" "nine" "ten" "eleven" "twelve")

(test-equal
    '("zero" "one" "two" "three" "four" "five" "six"
      "seven" "eight" "nine" "ten" "eleven" "twelve")
  (mutable-vector->list testvec))

(define (starts-with-T str) (char=? #\t (string-ref str 0)))

(test-equal 2 (mutable-vector-iterate starts-with-T testvec))

(test-equal 3 (mutable-vector-iterate starts-with-T testvec 2))

(test-equal 12 (mutable-vector-iterate starts-with-T testvec 4))

(test-equal 12 (mutable-vector-iterate starts-with-T testvec 1 -1))

(test-equal 10 (mutable-vector-iterate starts-with-T testvec 2 -1))

(test-equal 10 (mutable-vector-iterate starts-with-T testvec -1 -4))

(test-equal 12 (mutable-vector-iterate starts-with-T testvec -2 -4))

(test-equal 3 (mutable-vector-iterate starts-with-T testvec 1 -4))

(test-equal 2 (mutable-vector-iterate starts-with-T testvec 2 -4))

(define (slash-string port)
  (lambda (str)
    (display "/" port)
    (display str port))
  )

(define (remove-concat-cons start nelems)
  (call-with-port (open-output-string)
    (lambda (port)
      (mutable-vector-delete-range! testvec start nelems (slash-string port))
      (cons
       (get-output-string port)
       (mutable-vector->list testvec))))
  )

(test-equal
    '(("zero") "one" "two" "three" "four" "five" "six" "seven"
      "eight" "nine" "ten" "eleven" "twelve")
  (cons
   (mutable-vector-shift! testvec)
   (mutable-vector->list testvec))
  )

(test-equal
    '("" "one" "two" "three" "four" "five" "six" "seven"
      "eight" "nine" "ten" "eleven" "twelve")
  (remove-concat-cons 5 5)
  )

(test-equal
    '("/six/seven" "one" "two" "three" "four" "five"
      "eight" "nine" "ten" "eleven" "twelve")
  (remove-concat-cons 5 7))

(test-equal
    '("/one" "two" "three" "four" "five" "eight" "nine" "ten" "eleven" "twelve")
  (remove-concat-cons 0 1))

(test-equal '("/twelve/eleven/ten" "two" "three" "four" "five" "eight" "nine")
  (remove-concat-cons 8 5))

(test-equal '("/eight/five/four/three" "two" "nine")
  (remove-concat-cons 4 0))

(mutable-vector-shift! testvec -1)
;;(lens-set! "zero" testvec (=>mvector-index! 0))

(test-equal '(#f "two" "nine")
  (mutable-vector->list testvec))

(test-equal '("/#f/two/nine")
  (remove-concat-cons 0 3))

(set! testvec (new-mutable-vector 4 "zero" "one" "two" "three"))
(define =>test-ref (=>mvector-index! 1))

(test-equal "one" (view testvec =>test-ref))

(test-equal "three"
  (begin
    (mutable-vector-shift! testvec 2)
    (view testvec =>test-ref)
    ))

(test-equal '("two" "THREE")
  (begin
    (lens-set "THREE" testvec =>test-ref)
    (mutable-vector->list testvec)
    ))

(test-equal '(#f #f "two" "THREE")
  (begin
    (mutable-vector-shift! testvec -2)
    (mutable-vector->list testvec)
    ))

(test-equal '(#f "one" "two" "THREE")
  (begin
    (lens-set "one" testvec =>test-ref)
    (mutable-vector->list testvec)
    ))

(test-equal "THREE" (view testvec 3))

(test-end "schemacs_lens_vector")

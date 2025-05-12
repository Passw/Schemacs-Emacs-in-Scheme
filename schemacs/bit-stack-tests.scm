(import
  (scheme base)
  (gypsum bit-stack)
  (srfi 64))

(test-begin "gypsum_bitstack")

(test-equal 0 (let ((stack (new-bit-stack))) (bit-stack-count stack)))
(test-equal 0 (let ((stack (new-bit-stack 1))) (bit-stack-count stack)))

(test-equal #t
  (let ((stack (new-bit-stack 1)))
    (bit-stack-push! stack #t) (bit-stack-look stack)))

(test-equal #f
  (let ((stack (new-bit-stack 1)))
    (bit-stack-push! stack #f) (bit-stack-pop! stack)))

(test-equal (list #t #t #f #t #f #f)
  (let*((stack (new-bit-stack 1))
        (p (lambda () (bit-stack-pop! stack)))
        )
    (bit-stack-push! stack #f #f #t #f #t #t)
    (list (p) (p) (p) (p) (p) (p))
    ))

(test-equal (list #t #f #f #f #t #f #t #f #t #t #t #t #f #f #f #f #f)
  (let ()
    (define stack (new-bit-stack 1))
    (define (end) '())
    (define (pops n next)
      (if (> n 0)
          (cons (bit-stack-pop! stack) (pops (- n 1) next))
          (next)
          ))
    (define (push . vals) (apply bit-stack-push! stack vals))
    (push #f #f #f #f #f #f #f #f #t)
    (pops 4
      (lambda ()
        (push #t #t #t #t #f #t #f #t)
        (pops 13 end)
        ))
    ))

(test-end "gypsum_bitstack")

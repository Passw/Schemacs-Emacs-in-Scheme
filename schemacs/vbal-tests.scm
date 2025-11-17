(import
  (scheme base)
  (schemacs vbal)
  (only (srfi 64) test-assert test-begin test-end)
  )

(test-begin "schemacs_ui_vbal")

(define vbal0 (alist->vbal '()))

(test-assert (vbal-type? vbal0))
(test-assert (= 0 (vbal-length vbal0)))

(define number-assocs '((zero . 0) (one . 1) (two . 2) (three . 3)))

(define vbal-numbers (alist->vbal number-assocs))

(test-assert (vbal-type? vbal-numbers))
(test-assert (= 4 (vbal-length vbal-numbers)))
(test-assert (equal? (vbal->alist vbal-numbers) number-assocs))
(test-assert (equal? '(OK . 2) (vbal-find (lambda (i k v) (and (eq? v 2) `(OK . ,i))) vbal-numbers)))
(test-assert (equal? '(one . 1) (vbal-assoc 'one vbal-numbers)))
(test-assert (equal? '(two . 2) (vbal-assq 'two vbal-numbers)))
(test-assert (equal? '(zero . 0) (vbal-assv 'zero vbal-numbers)))
(test-assert
    (equal?
     '(three . 3)
     (let-values (((k v) (vbal-ref vbal-numbers 3))) (cons k v))
     ))
(test-assert
    (equal?
     '("five" . 5)
     (let ()
       (vbal-set! vbal-numbers 3 "five" 5)
       (let-values (((k v) (vbal-ref vbal-numbers 3))) (cons k v))
       )))
(test-assert
    (equal?
     '("one" . 1)
     (let ()
       (vbal-key-set! vbal-numbers 1 "one")
       (let-values (((k v) (vbal-ref vbal-numbers 1))) (cons k v))
       )))
(test-assert
    (equal?
     '(vbal-value-set! vbal-numbers 3  5)
     ))
(define vbal2 #f)
(test-assert
    (let ()
      (set! vbal2
        (vbal-map
         (lambda (k v) (if (string? k) (values (string->symbol k) v) (values k v)))
         vbal-numbers
         ))
      (and
       (equal? '("one" . 1) (let-values (((k v) (vbal-ref vbal-numbers 1))) (cons k v)))
       (equal? '(one . 1) (let-values (((k v) (vbal-ref vbal2 1))) (cons k v)))
       )))

(test-end "schemacs_ui_vbal")

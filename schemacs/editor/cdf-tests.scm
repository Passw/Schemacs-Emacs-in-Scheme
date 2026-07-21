(import
 (scheme base)
 (schemacs editor cdf)
 (only (srfi 64) test-assert test-equal test-begin test-end)
 )

(test-begin "schemacs_editor_cdf")

(define (find cdf n)
  (let-values (((i val) (cdf-find cdf n)))
    (cons i val)
    ))

(define testcdf (new-cdf 0))

(test-equal '(#f . #f)
  (find testcdf 1)
  )

(test-equal '((0 . 0) (#f . #f))
  (let ()
    (cdf-push testcdf 1)
    (list
      (find testcdf 0)
      (find testcdf 1)
      )))

(test-equal '(1 . 1)
  (let ()
    (cdf-push testcdf 1)
    (find testcdf 1)
    ))

(test-equal '((2 . 2) (3 . 8))
  (let ()
    (cdf-push testcdf 6)
    (cdf-push testcdf 2)
    (list
     (find testcdf 7)
     (find testcdf 8)
     )))

(test-equal '((3 . 8) (4 . 12))
  (let ()
    (cdf-pop testcdf)
    (cdf-push testcdf 4)
    (cdf-push testcdf 4)
    (list
     (find testcdf 11)
     (find testcdf 12)
     )))

(test-equal #f
  (cdf-invalidate! testcdf 6)
  )

(test-equal '(2 4 6 ((0 . 0) (1 . 1) (2 . 2) (2 . 2)))
  (let ()
    (list
     (cdf-invalidate! testcdf 2)
     (cdf-push testcdf 2)
     (cdf-push testcdf 2)
     (map
      (lambda (i) (find testcdf i))
      '(0 1 2 3)
      ))))

(test-equal
  '(( 0 0 .  0) ( 1 0 .  0) ( 2 0 .  0) ( 3 0 .  0) ( 4 0 .  0) ( 5 0 .  0) ( 6 0 .  0)
    ( 7 1 .  7) ( 8 1 .  7) ( 9 1 .  7) (10 1 .  7) (11 1 .  7) (12 1 .  7)
    (13 2 . 13) (14 2 . 13) (15 2 . 13) (16 2 . 13) (17 2 . 13)
    (18 3 . 18) (19 3 . 18) (20 3 . 18) (21 3 . 18)
    (22 4 . 22) (23 4 . 22) (24 4 . 22)
    (25 5 . 25) (26 5 . 25)
    (27 #f . #f)
    )
  (let ()
    (cdf-invalidate! testcdf 0)
    (cdf-fill testcdf (lambda (i accum) (and (< i 6) (- 7 i))))
    (let loop ((i 0))
      (if (< i 28) (cons (cons i (find testcdf i)) (loop (+ 1 i))) '())
      )))

(test-end "schemacs_editor_cdf")

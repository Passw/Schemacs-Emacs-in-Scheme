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

(test-equal '(#f . #f)
  (let ()
    (cdf-push testcdf 1)
    (find testcdf 1)
    ))

(test-equal '(0 . 1)
  (let ()
    (cdf-push testcdf 1)
    (find testcdf 1)
    ))

(test-equal '((3 . 8) (4 . 10))
  (let ()
    (cdf-push testcdf 6)
    (cdf-push testcdf 2)
    (list
     (find testcdf 7)
     (find testcdf 8)
     )))

(test-equal '((4 . 12) (5 . 16))
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

(test-equal '((0 . 1) (1 . 2) (2 . 4) (2 . 4))
  (let ()
    (list
     (cdf-invalidate! testcdf 2)
     (cdf-push testcdf 2)
     (cdf-push testcdf 2)
     (map
      (lambda (i) (find testcdf i))
      '(0 1 2 3)
      ))))

(test-equal '(1 (0 . 7) (1 . 13) (2 . 18) (3 . 22) (4 . 25) (5 . 27))
  (let*((a (cdf-invalidate! testcdf 0)))
    (cdf-fill testcdf
     (lambda (i accum)
      (cond
       ((< i 6) (+ accum (- 7 i)))
       (else #f)
       )))
    (cons a
     (map
      (lambda (i) (cdf-find testcdf i))
      '(6 12 17 21 24 26)
      ))))

(test-end "schemacs_editor_cdf")

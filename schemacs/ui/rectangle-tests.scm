(import
  (scheme base)
  (only (srfi 64) test-assert test-begin test-end)
  (schemacs ui rectangle)
  )

(test-begin "schemacs_ui_rectangle")

(test-assert
    (point2D=? (point2D 1 1)
     (rect2D-opposite-corner
      (rect2D (point2D 0 0) (size2D 1 1))
      )))


(test-assert
    (rect2D=?
     (rect2D (point2D 0 0) (size2D 1 1))
     (rect2D-canonical
      (rect2D (point2D 1 1) (size2D -1 -1))
      )))


(test-assert
    (let*-values
        (((p) (point2D 1 2))
         ((min2D max2D) (point2D-min-max p))
         )
      (and (point2D=? min2D p) (point2D=? max2D p))
      ))


(test-assert
    (let-values
        (((min2D max2D)
          (point2D-min-max
           (point2D 0 1)
           (point2D 1 0)
           (point2D 2 1)
           (point2D 1 2)
           )))
      (and
       (point2D=? min2D (point2D 0 0))
       (point2D=? max2D (point2D 2 2))
       )))


(test-assert
    (rect2D=?
     (rect2D (point2D 0 0) (size2D 3 3))
     (rect2D-enclosing
      (rect2D 3 2 -1 1)
      (rect2D 2 1 -1 1)
      (rect2D 1 0 -1 1)
      ))
    )

(test-end "schemacs_ui_rectangle")

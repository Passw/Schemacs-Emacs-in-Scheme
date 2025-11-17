(define-library (schemacs ui rectangle)
  (import
    (scheme base)
    (scheme case-lambda)
    (only (scheme write) display write)
    (only (schemacs lens)
          record-unit-lens  lens  update
          =>view-only-lens  =>canonical
          )
    )
  (export
   size2D-type?     size2D         size2D=?   point2D->size2D
   size2D-width     size2D-height
   point2D-type?    point2D        point2D=?  size2D->point2D
   point2D-x        point2D-y
   rect2D-type?     rect2D         rect2D=?
   rect2D-point     rect2D-size    rect2D-canonical
   point2D-min-max  rect2D-opposite-corner
   rect2D-enclosing copy-rect2D    copy-point2D
   copy-size2D      copy-2D
   write-rect2D     write-point2D  write-size2D
   )
  (begin

    (define-record-type <size2D-type>
      (make<size2D> width height)
      size2D-type?
      (width   size2D-width)
      (height  size2D-height)
      )

    (define (copy-size2D o)
      (make<size2D> (size2D-width o) (size2D-height o))
      )

    (define size2D
      ;; Apply a width and height value to this procedure to construct
      ;; a 2D size value. If only a single argument is applied, it is
      ;; treated as both the width and height, defining a square.
      ;;--------------------------------------------------------------
      (case-lambda
        ((wh) (make<size2D> wh wh))
        ((w h) (make<size2D> w h))
        ))

    (define write-size2D
      (case-lambda
        ((s) (write-size2D s (current-output-port)))
        ((s port)
         (write-string "(size2D " port)
         (write (size2D-width s) port)
         (write-char #\space port)
         (write (size2D-height s) port)
         (write-char #\) port)
         )))

    (define-record-type <point2D-type>
      (point2D x y)
      point2D-type?
      (x  point2D-x)
      (y  point2D-y)
      )

    (define (copy-point2D o)
      (point2D (point2D-x o) (point2D-y o))
      )

    (define write-point2D
      (case-lambda
        ((p) (write-size2D p (current-output-port)))
        ((p port)
         (write-string "(point2D " port)
         (write (point2D-x p) port)
         (write-char #\space port)
         (write (point2D-y p) port)
         (write-char #\) port)
         )))

    (define-record-type <rect2D-type>
      (make<rect2D> point size)
      rect2D-type?
      (point   rect2D-point)
      (size    rect2D-size)
      )

    (define (copy-rect2D o)
      (make<rect2D>
       (copy-point2D (rect2D-point o))
       (copy-size2D (rect2D-size o))
       ))

    (define (copy-2D o)
      (cond
       ((boolean? o) o)
       ((symbol? o) o)
       ((number? o) o)
       ((rect2D-type?  o) (copy-rect2D  o))
       ((size2D-type?  o) (copy-size2D  o))
       ((point2D-type? o) (copy-point2D o))
       (else (error "cannot copy, not a 2D value" o))
       ))

    (define rect2D
      (case-lambda
        ((point size) (make<rect2D> point size))
        ((a b c)
         (cond
          ((point2D-type? a) (make<rect2D> a (size2D b c)))
          ((size2D-type?  c) (make<rect2D> (point2D a b) c))
          (else (error "rect2D constructor received values of wrong type" a b c))
          ))
        ((x y w h) (make<rect2D> (point2D x y) (size2D w h)))
        ))

    (define (size2D=? a b)
      (and (eq? (size2D-width  a) (size2D-width  b))
           (eq? (size2D-height a) (size2D-height b))
           ))

    (define (point2D=? a b)
      (and (eq? (point2D-x a) (point2D-x b))
           (eq? (point2D-y a) (point2D-y b))
           ))

    (define (rect2D=? a b)
      (and (point2D=? (rect2D-point a) (rect2D-point b))
           (size2D=?  (rect2D-size  a) (rect2D-size  b))
           ))

    (define (point2D-min-max a . more)
      (let loop
          ((more more)
           (min-x (point2D-x a))
           (min-y (point2D-y a))
           (max-x (point2D-x a))
           (max-y (point2D-y a))
           )
        (cond
         ((null? more)
          (values
           (point2D min-x min-y)
           (point2D max-x max-y)
           ))
         (else
          (let ((b (car more)))
            (loop
             (cdr more)
             (min min-x (point2D-x b))
             (min min-y (point2D-y b))
             (max max-x (point2D-x b))
             (max max-y (point2D-y b))
             ))))))

    (define rect2D-opposite-corner
      (case-lambda
        ((rect)
         (rect2D-opposite-corner
          (rect2D-point rect)
          (rect2D-size  rect)
          ))
        ((point size)
         (point2D
          (+ (point2D-x point) (size2D-width  size))
          (+ (point2D-y point) (size2D-height size))
          ))))

    (define (rect2D-canonical rect)
      (let*((a (rect2D-point rect))
            (b (rect2D-opposite-corner rect))
            (min-x (min (point2D-x a) (point2D-x b)))
            (max-x (max (point2D-x a) (point2D-x b)))
            (min-y (min (point2D-y a) (point2D-y b)))
            (max-y (max (point2D-y a) (point2D-y b)))
            )
        (rect2D
         (point2D min-x min-y)
         (size2D
          (- max-x min-x)
          (- max-y min-y)
          ))))

    (define (rect2D-enclosing elem . more)
      ;; Apply one or more values of type `RECT2D` or `POINT2D`,
      ;; construct the smallest possible `RECT2D` that encloses all of
      ;; the rectangles and points.
      ;;--------------------------------------------------------------
      (let*-values
          (((min-point max-point)
            (cond
             ((point2D-type? elem) (values elem elem))
             ((rect2D-type? elem)
              (values
               (rect2D-point elem)
               (rect2D-opposite-corner elem)
               ))))
           ((min-point max-point)
            (let loop
                ((min-point min-point)
                 (max-point max-point)
                 (more      more)
                 )
              (cond
               ((null? more) (values min-point max-point))
               (else
                (let ((elem (car more)))
                  (let-values
                      (((min-point max-point)
                        (cond
                         ((point2D-type? elem)
                          (point2D-min-max elem min-point max-point)
                          )
                         ((rect2D-type? elem)
                          (point2D-min-max
                           min-point max-point
                           (rect2D-point elem)
                           (rect2D-opposite-corner elem)
                           ))
                         (else (error "not a rect2D or point2D type" elem))
                         )))
                    (loop min-point max-point (cdr more))
                    ))))))
           ((min-x) (point2D-x min-point))
           ((min-y) (point2D-y min-point))
           ((max-x) (point2D-x max-point))
           ((max-y) (point2D-y max-point))
           )
        (rect2D
         (point2D min-x min-y)
         (size2D
          (- max-x min-x)
          (- max-y min-y)
          ))))

    (define write-rect2D
      (case-lambda
        ((r) (write-size2D r (current-output-port)))
        ((r port)
         (let ((p (rect2D-point r))
               (s (rect2D-size  r))
               )
           (write-string "(rect2D " port)
           (write (point2D-x p) port)
           (write-char #\space port)
           (write (point2D-y p) port)
           (write (size2D-width s) port)
           (write-char #\space port)
           (write (size2D-height s) port)
           (write-char #\) port)
           ))))

    ;;--------------------------------------------------------------
    ))

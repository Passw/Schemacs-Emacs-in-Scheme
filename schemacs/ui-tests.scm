(import
  (scheme base)
  (scheme lazy)
  (only (schemacs lens) lens-set)
  (only (srfi 64)
        test-assert  test-begin  test-end
        )
  (only (schemacs lens) view)
  (only (schemacs vbal) vbal-type? vbal->alist)
  (only (schemacs ui rectangle)
        rect2D  point2D  rect2D=?  point2D=?
        )
  (schemacs ui)
  )

(test-begin "schemacs_ui")

(define (assert-error proc errmsg irr)
  (let ((result
         (call/cc
          (lambda (halt)
            (with-exception-handler
                (lambda (err) (halt err))
              proc
              )))))
    (and
     (string=? errmsg (error-object-message result))
     (equal? irr (error-object-irritants result))
     )))

(test-assert
    (let*((on-button-push (lambda () (display "OK\n")))
          (example
           (run-div-monad
            #f
            (push-button
             (widget 'example)
             (properties
              'on-button-push: on-button-push
              )))))
      (and
       (vbal-type? (view example =>div-properties*!))
       (eq? (div-view-type example) push-button)
       (equal?
        `((on-button-push: . ,on-button-push))
        (vbal->alist (view example =>div-properties*!))
        )
       (not (div-parent example))
       )))

(test-assert
    (assert-error
     (lambda ()
       (run-div-monad #f
        (div
         (widget 'example)
         (cons 75600 #f)
         )))
     "cannot initialze div with value of type"
     '((75600 . #f))
     ))

(test-assert
    (let*((result
           (call/cc
            (lambda (halt)
              (with-exception-handler (lambda (err) (halt err))
                (lambda ()
                  (run-div-monad
                   #f
                   (div-pack
                    (pack-elem (div (content "correct")))
                    (content "incorrect")
                    ))))))))
      (and
       (error-object? result)
       (string=? (error-object-message result) "initialized content more than once")
       (= 2 (length (error-object-irritants result)))
       (div-pack-type? (car (error-object-irritants result)))
       )))

(test-assert
    (let*((testdiv
           (run-div-monad #f
            (div-pack
             cut-horizontal from-start
             (pack-elem (div (content "example"))))
            ))
          (example (div-content testdiv))
          )
      (and
       (eq? cut-horizontal (div-pack-orientation example))
       (eq? from-start (div-pack-from example))
       (= 1 (vector-length (div-pack-subdivs example)))
       )))

(test-assert
    (let*((sizes '(enclose expand enclose))
          (o (lambda (str) (div (content str))))
          (testdiv
           (run-div-monad #f
            (div-grid
             (x-sizes sizes)
             (y-sizes sizes)
             (row-major
              increasing
              (list   (o "00") (o "01") (o "02"))
              (list 1 (o "10") (o "11") (o "12"))
              (list   (o "20") (o "21") (o "22"))
              ))))
          (example (div-content testdiv))
          )
      (and
       (equal? sizes (vector->list (div-grid-x-sizes example)))
       (equal? sizes (vector->list (div-grid-y-sizes example)))
       (equal?
        '("00" "01" "02" "10" "11" "12" "20" "21" "22")
        (map div-content
             (vector->list (div-grid-subdivs example))
             )))))

(test-assert
    (let*((testdiv
           (run-div-monad #f
            (div-space
             (floater
              1 (rect2D 20 20 32 32)
              (div (content "example"))
              )
             (inner-align (point2D 10 20))
             (outer-align (point2D from-middle from-middle))
             )))
          (example (div-content testdiv))
          (elems (and (div-space-type? example) (div-space-elements example)))
          (flo (and (vector? elems) (< 0 (vector-length elems)) (vector-ref elems 0)))
          (subdiv (and (floater-type? flo) (floater-div flo)))
          (inalgn (and example (div-space-inner-align example)))
          (outalgn (and example (div-space-outer-align example)))
          )
      (and
       example elems flo subdiv
       (div-type? subdiv)
       (string=? "example" (div-content (floater-div flo)))
       (= 1 (floater-z-index flo))
       (rect2D=? (rect2D 20 20 32 32) (floater-rect flo))
       (point2D=? inalgn (point2D 10 20))
       (point2D=? outalgn (point2D from-middle from-middle))
       )))

(define (strdiv str) (div (content str)))

(define (simple-pack-test)
  (div-pack
   cut-horizontal
   (pack-elem 'enclose (strdiv "Hello, world!"))
   (pack-elem 'expand  (strdiv "Scheme is like a ball of snow."))
   ))

(define (check-simple-packed o)
  (let*((packed (div-content o))
        (subdivs (and packed (div-pack-subdivs packed))))
    (and
     packed subdivs
     (eq? cut-horizontal (div-pack-orientation packed))
     (= 2 (vector-length subdivs))
     (string=?
      (div-content (vector-ref subdivs 0))
      "Hello, world!"
      )
     (string=?
      (div-content (vector-ref subdivs 1))
      "Scheme is like a ball of snow."
      ))))

(define (pack-test . elems)
  (div
   (apply
    tiled-windows 'cut-horizontal 'from-start
    (map pack-elem elems)
    )))

(define (simple-grid-test upper-left upper-right lower-left lower-right)
  (div-grid
   (x-sizes '(expand expand))
   (y-sizes '(expand expand))
   (row-major
    (list upper-left upper-right)
    (list lower-left lower-right)
    )))

(define (grid-test)
  (simple-grid-test
   (div "upper-left") (div "upper-right")
   (div "lower-left") (div "lower-right")
   ))

(define (simple-space-test . elems)
  (apply
   div-space
   (let loop ((elems elems) (i 0))
     (cond
      ((null? elems) '())
      (else
       (cons
        (floater (rect2D i i 100 10) (div (content (car elems))))
        (loop (cdr elems) (+ 15 i))
        ))))))

(define (space-test)
  (simple-space-test
   "Hello, world!"
   "Scheme is like a ball of snow!"
   "Testing shows the presence of bugs, never the absence of bugs."
   "Large C programs are likely to contain a buggy, incomplete Lisp interpreter."
   ))

(define (counter-test)
  (let*((number (state-var = 0))
        (button
         (lambda (label action)
           (push-button
            label
            (properties
             'on-button-push: (update-var number action)
             )))))
    (div-pack
     cut-horizontal
     (div-pack
      cut-vertical
      (button "+" (lambda (n) (+ n 1)))
      (button "-" (lambda (n) (- n 1)))
      (use-vars (list number) div)
      )
     (div "")
     )))

(define (split-screen-test)
  (tiled-windows cut-vertical
   (div "Left-side")
   (div "Middle")
   (div "Right-side")
   ))

(define (layout-test)
  (define current-layout (state-var eq? #f))
  (define (button label action)
    (push-button label
     (properties
      'on-button-push: (update-var current-layout action)
      )))
  (define (three-row-button) (button "Three in a row" three-row))
  (define (three-row _)
    (display "; three-row\n")
    (tiled-windows cut-vertical
     (three-row-button)
     (two-above-button)
     (two-right-button)
     ))
  (define (two-above-button) (button "Two up above" two-up-above))
  (define (two-up-above _)
    (display "; two-up-above\n")
    (tiled-windows cut-horizontal
     (tiled-windows cut-vertical
      (three-row-button)
      (two-above-button)
      )
     (two-right-button)
     ))
  (define (two-right-button) (button "Two on the right" two-right))
  (define (two-right _)
    (display "; two-right\n")
    (tiled-windows cut-vertical
     (three-row-button)
     (tiled-windows cut-horizontal
      (two-above-button)
      (two-right-button)
      )))
  (lens-set (three-row #f) current-layout =>state-var-value*!)
  (use-vars (list current-layout) (lambda (o) o))
  )

(test-assert
    (let*((packed (run-div-monad #f (simple-pack-test))))
      (check-simple-packed packed)
      ))

(test-assert
    (let*((o (run-div-monad #f (div (simple-pack-test))))
          (packed (div-content o))
          )
      (check-simple-packed (div-content o))
      ))

(test-end "schemacs_ui")

(display "----------------------------------------------------------------------------------------------------\n")

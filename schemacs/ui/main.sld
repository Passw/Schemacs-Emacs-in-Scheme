(define-library (schamcs ui main)
  ;; This library defines the main user interface for Schemacs in
  ;; terms of the platform-independent `(schemacs ui)` framework.
  ;; This is an Emacs-like user interface.
  ;;------------------------------------------------------------------
  (import
    (scheme base)
    (only (schemacs ui rectangle)
          (rect2D  point2D  size2D)
          )
    (only (schemacs ui)
          run-div-monad  enclose  expand
          state-var  div  div-pack  div-space  floater
          tiled-windows
          )
    (only (schemacs hash-table)
          hash-table->alist
          )
    (only (schemacs lens) view)
    (only (schemacs editor)
          editor-state
          =>editor-buffer-table
          =>editor-winframe-table
          =>winframe-view
          )
    )
  (export
   main
   )
  (begin

    (define (frame->div rect frame-handle frame)
      (let*((layout (view frame =>winframe-layout))
            (layout (or layout ))
            )
        (floater ;; top-level
         rect
         (tiled-windows ;; window container
          editor-content-area-state
          ))))

    (define (main)
      (let*((edst (editor-state))
            (winframe-table (view edst =>editor-winframe-table))
            )
        (run-div-monad
         #f ;; <- no parent, this is the top-level widget
         (apply
          div-space
          (let loop
              ((x 12) (y 6)
               (frames (hash-table->alist winframe-table))
               )
            (cond
             ((pair? frames)
              (let ((named-frame (car frames)))
                (cons
                 (frame->div
                  (rect2D
                   (point2D x y)
                   (size2D enclose enclose)
                   )
                  (car named-frame)
                  (cdr named-frame)
                  )
                 (loop (+ 12 x) (+ 6 y) (cdr frames))
                 )))
             ((null? frames) '())
             (else
              (error "hash-table->alist returned inproper list" frames)
              )))))))

    ;;----------------------------------------------------------------
    ))

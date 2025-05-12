(define-library (gypsum vector)
  ;; TODO: Remove this library some day, when SRFI-133 becomes more
  ;; ubiquitous, or perhaps just when Guile finally supports the
  ;; SRFI-133 API.
  ;;
  ;; Not all of SRFI-133 is used in this project, so only the APIs
  ;; used but not defined more universally across various Scheme
  ;; implementations are defined here.

  (import (scheme base))
  (export
   vector-fold
   )

  (cond-expand
    (guile
     (import
       (rename (srfi 43)
               (vector-fold old-vector-fold)))
     (include "vector.scm")
     )
    (mit
     (import (srfi 133)))
    (else
     ;; We need this second "cond-expand" layer here because Guile
     ;; does not seem to fully conform to the R7RS standard yet -- the
     ;; "cond-expand" implementation used does not understand the
     ;; "(library ...)" clause.
     (cond-expand
       ((library (srfi 133))
        (import
          (only (srfi 133)
                vector-fold))
        )
       ((library (srfi 43))
        (import
          (rename (srfi 43)
                  (vector-fold old-vector-fold)))
        (export
         vector-fold
         )
        (include "vector.scm")
        )
       (else
        (begin
          (define (vector-fold kons knil . veclist)
            ;; This is an implementation of SRFI-133 `VECTOR-FOLD` written using
            ;; only APIs exposed by the `(SCHEME BASE)` library defined by the
            ;; R7RS Scheme standard.
            ;;------------------------------------------------------------------
            (let ((minlen
                   (let loop ((veclist veclist) (minlen #f))
                     (cond
                      ((null? veclist) (if minlen minlen 0))
                      (else
                       (let ((thislen (vector-length (car veclist))))
                         (loop (cdr veclist)
                               (if minlen (min thislen minlen) thislen)))))))
                  )
              (cond
               ((= 0 minlen) knil)
               (else
                (let loop ((i 0) (state knil))
                  (cond
                   ((>= i minlen) state)
                   (else
                    (loop (+ 1 i)
                          (apply kons state
                                 (map (lambda (vec) (vector-ref vec i))
                                      veclist))))))))
              ))
          )))))
  )

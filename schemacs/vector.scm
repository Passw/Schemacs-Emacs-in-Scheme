
(define (vector-fold kons knil . vecs)
  ;; Re-define `VECTOR-FOLD` such that it's API is identical to
  ;; that of SRFI-133. To do this, the `INDEX` argument is no
  ;; longer passed to the `KONS` procedure on each iteration.
  (apply old-vector-fold (lambda (_index accum . elems) (apply kons accum elems)) knil vecs)
  )

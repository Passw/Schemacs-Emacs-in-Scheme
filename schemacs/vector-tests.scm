(import
  (gypsum vector))

(test-begin "gypsum_vector")

(test-assert 6
  (vector-fold + 0 (vector 0 1 2 3)))

(test-assert 36
  (vector-fold + 0
   (vector 6 7 8 400 500 600)
   (vector 3 4 5 200 300)
   (vector 0 1 2 100)
   (vector 0 0 0)
   ))

(test-end "gypsum_vector")

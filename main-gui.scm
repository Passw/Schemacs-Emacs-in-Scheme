(cond-expand
  (guile
   (load "schemacs/platform/guile/main.scm")
   )
  (else (error "There is no GUI support for this Scheme implementation."))
  )

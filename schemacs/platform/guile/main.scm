(import (scheme base))

(cond-expand
  ((library (gi))
   (import
     (scheme base)
     (only (scheme repl) interaction-environment)
     (only (scheme eval) eval)
     (only (schemacs ui platform guile-gi-gtk3) main)
     ))
  ((library (g-golf))
   (error "Guile G-Golf is not yet supported.")
   )
  (else
   (error "The GUI library could not be found for this Guile installation.")
   ))

(define repl-env
  (interaction-environment)
  )

(eval
 '(import
    (only (schemacs ui platform guile-gi-gtk3) main)
    (schemacs eval)
    (schemacs lens)
    (schemacs elisp-eval)
    )
 repl-env
 )
((eval
  '(lambda (repl-env)
     (parameterize
         ((*the-environment-procedure* (lambda () repl-env)))
       (main 'start-repl: #t)
       ))
  repl-env
  )
 repl-env
 )

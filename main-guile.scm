(import (scheme base))

(cond-expand
  ((library (gi))
    (import
      (only (schemacs ui platform guile-gi-gtk3)
            gtk-draw-div
            )
      ))
  ((library (g-golf))
   (error "Guile G-Golf is not yet supported")
   )
  (else
   (error "GUI library not available on this platform")
   ))

(import
  (only (schemacs editor) main)
  )

(gtk-draw-div (main 'start-repl: #t))

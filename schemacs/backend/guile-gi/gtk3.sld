(define-library (gypsum backend guile-gi gtk3)
  (import
    (scheme base)
    (only (gypsum eval) eval interaction-environment)
    (only (gypsum backend guile-gi gtk3-init) launch-gui))
  (export main)
  (include "gtk3.scm"))

(define-library (schemacs backend guile-gi gtk3)
  (import
    (scheme base)
    (only (schemacs eval) eval interaction-environment)
    (only (schemacs backend guile-gi gtk3-init) launch-gui))
  (export main)
  (include "gtk3.scm"))

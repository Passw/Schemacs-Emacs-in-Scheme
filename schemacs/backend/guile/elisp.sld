(define-library (gypsum backend guile elisp)
  (import
    (scheme base)
    (scheme case-lambda)
    (only (system base language) default-environment)
    (only (system base compile) compile)
    (only (ice-9 eval-string) eval-string)
    (only (gypsum elisp-eval spec) elisp)
    (only (gypsum elisp-eval compile-tree-il) compile-tree-il)
   )

  (export
   *the-elisp-environment*
   new-elisp-env
   eval-elisp
   eval-elisp-string
   )

  (include "elisp.scm")
  )



(define-library (gypsum eval)
  (import
    (scheme base)
    (only (scheme case-lambda) case-lambda)
    (only (scheme write) display write)
    (prefix (scheme eval) scheme:)
    (prefix (scheme repl) scheme:)
    (prefix (scheme r5rs) scheme:)
    (prefix (gypsum concurrent) th:))
  (export
   eval eval-string environment
   null-environment
   scheme-report-environment
   interaction-environment
   the-environment
   *eval-procedure* *eval-string-procedure* *environment-procedure*
   *null-environment-procedure*
   *scheme-report-environment-procedure*
   *interaction-environment-procedure*
   *the-environment-procedure*
   )
  (include "eval.scm"))

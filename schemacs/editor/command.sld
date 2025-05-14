(define-library (schemacs editor command)
  (import
    (scheme base)
    (scheme write)
    (scheme case-lambda)
    (only (schemacs lens) record-unit-lens))
  (export
   <command-type> command-type? make<command> new-command
   command-name command-procedure command-doc-string
   run-command apply-command show-command
   =>command-name*!
   =>command-procedure*!
   =>command-doc-string*!
   =>command-source-type*!
   =>command-source-location*!
   =>command-source-code*!
   )
  (include "command.scm")
  )

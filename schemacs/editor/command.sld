(define-library (gypsum editor command)
  (import
    (scheme base)
    (scheme write)
    (scheme case-lambda)
    (only (gypsum lens) record-unit-lens))
  (export
   <command-type> command-type? make<command> new-command
   command-name command-procedure command-doc-string
   run-command apply-command show-command
   =>command-name
   =>command-procedure
   =>command-doc-string
   =>command-source-type
   =>command-source-location
   =>command-source-string)
  (include "command.scm"))

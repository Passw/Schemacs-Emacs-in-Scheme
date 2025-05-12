(define-library (gypsum backend guile-gi gtk3-repl)
  (import
    (scheme base)
    (scheme write)
    (scheme case-lambda)
    (only (guile)
          save-module-excursion set-current-module current-module)
    (prefix (system repl common) guile:)
    (prefix (system repl command) guile:)
    (only (gypsum concurrent)
          new-channel channel-take! channel-put!)
    (prefix (srfi 18) th:)
    (only (srfi 18) thread-start!)
    (prefix (gi) gi:)
    (only (gi util) push-duplicate-handler!)
    (only (gi) <signal>)
    (prefix (gi repository) gi-repo:)
    (prefix (gi types) gi-type:)
    )
  (export
   make-repl-thread
   thread-start!
   repl-welcome-message
   async-apply-in-main-thread
   )
  (include "gtk3-repl.scm")
  )

(define-library (schemacs sim-agent)
  ;; This library is mostly for creating GUI test stubs, but it might
  ;; be more generally useful for running agent simulations.
  (import
    (scheme base)
    (scheme write)
    (srfi srfi-64) ;; test suite
    (prefix (srfi srfi-28) basic:) ; basic format strings
    (only (schemacs lens) record-unit-lens view update! lens-set!)
    (only (schemacs lens vector)
          new-mutable-vector
          mutable-vector-append!
          mutable-vector-for-each)
    )
  (export
   <sim-agent-type>
   make<sim-agent-type>
   is<sim-agent-type>?
   sim-agent-type?
   sim-agent-reset-id-gen
   new-sim-agent
   =>sim-agent-state
   sim-agent-clear-logs
   sim-agent-log
   sim-agent-for-each-log
   sim-agent-print-logs
   sim-agent-logs-to-string
   sim-agent->string
   )
  (include "sim-agent.scm")
  )

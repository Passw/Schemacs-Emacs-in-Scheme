(import (srfi srfi-64))

(test-begin "gypsum_sim-agent")

(define agent0 (new-sim-agent "test-agent" 0))

(test-assert (is<sim-agent-type>? agent0))

(test-assert ((sim-agent-type? "test-agent") agent0))

(let ()
  (sim-agent-log agent0 "ABC")
  (test-assert (equal? (sim-agent-logs-to-string agent0 #f) "ABC"))
  (sim-agent-log agent0 "DEF")
  (test-assert (equal? (sim-agent-logs-to-string agent0 #t) "ABCDEF"))
  (test-assert (equal? (sim-agent-logs-to-string agent0 #f) ""))
  )

(let ()
  (sim-agent-log agent0 "Oh how I wish I were a string!")
  (test-assert
      (equal?
       "(test-agent(0)\n  \"Oh how I wish I were a string!\")\n"
       (sim-agent->string agent0 #t)))
  )

(test-end "gypsum_sim-agent")

(define-library (schemacs concurrent)
  (import
    (scheme base)
    (scheme case-lambda)
    (only (scheme write) display write)
    (prefix (schemacs lens vector) vec:)
    )

  (cond-expand
    (guile-3
     (import
       (prefix (srfi 18) th:))))

  (export
   channel-type?
   new-channel channel-take! channel-put! channel-update!
   channel-ref channel-try-ref
   new-channel/error-handler
   set!channel-error-handler
   channel-default-error-handler

   ;; ----------------------------------------
   log-buffer-type?
   new-log-buffer log! log-clear! log-dump!
   new-unsafe-log-buffer
   log-buffer-iterate
   log-message-type?
   log-message-id log-message-thread log-message-string
   log-message-display

   ;; ----------------------------------------
   worker-type? make<worker>
   recruit-worker recruit-clone
   request-work relieve-worker! fire-worker!
   make-delayed-loop delayed-loop
   make-delayed-counter delayed-counter
   specialist-job ordinary-job
   new-worker-with new-worker make-recruiter
   )

  (include "concurrent.scm")
  )

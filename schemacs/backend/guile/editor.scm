
(define (command-error-default-function data context signal)
  (let*((editor (current-editor))
        (msgbuf (editor-messages editor))
        (insert (*impl/insert-info-buffer*))
        )
    (insert msgbuf
     (call-with-port (open-output-string)
       (lambda (port)
         (pretty port (print data context signal))
         (get-output-string port))))
    ))

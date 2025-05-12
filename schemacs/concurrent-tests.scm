(import
  (only (srfi srfi-64)
        test-begin test-end test-assert))

(test-begin "schemacs_concurrent")

(test-assert
    (equal?
     (string-append
      "thread-A (0 . \"ball\")\n"
      "thread-B (1 . \"ball->A\")\n"
      "thread-A (2 . \"ball->A->B\")\n"
      "thread-B (3 . \"ball->A->B->A\")\n"
      "thread-A (4 . \"ball->A->B->A->B\")\n"
      "thread-B (5 . \"ball->A->B->A->B->A\")\n"
      "thread-A (6 . \"ball->A->B->A->B->A->B\")\n"
      "thread-B (7 . \"ball->A->B->A->B->A->B->A\")\n"
      "thread-A (8 . \"ball->A->B->A->B->A->B->A->B\")\n"
      "thread-B (9 . \"ball->A->B->A->B->A->B->A->B->A\")\n"
      "thread-B 'done\n"
      "thread-A (10 . \"ball->A->B->A->B->A->B->A->B->A->B\")\n"
      "thread-A 'done\n")
     (run-ping-pong-test)))


(define (ping-pong name chan-A chan-B port)
  (th:make-thread
   (lambda ()
     (let loop ()
       (let ((ball (channel-take! chan-A)))
         (display (string-append "thread-" name " ") port)
         (write ball port)
         (newline port)
         (channel-put! chan-B
          (cons
           (+ 1 (car ball))
           (string-append (cdr ball) "->" name)))
         (th:thread-sleep! 0.05)
         (cond
          ((< (car ball) 9) (loop))
          (else
           (display (string-append "thread-" name " 'done") port)
           (newline port)
           (th:thread-sleep! 0.05))))))))

(define (run-ping-pong-test)
  (let ((chan-A (new-channel))
        (chan-B (new-channel)))
    (call-with-port (open-output-string)
      (lambda (port)
        (let ((thread-A (ping-pong "A" chan-A chan-B port))
              (thread-B (ping-pong "B" chan-B chan-A port)))
          (th:thread-start! thread-A)
          (th:thread-start! thread-B)
          (channel-put! chan-A (cons 0 "ball"))
          (th:thread-join! thread-A)
          (th:thread-join! thread-B)
          (get-output-string port))))))

;; -------------------------------------------------------------------------------------------------

(define (simple-log-test)
  (let*((log (new-unsafe-log-buffer)))
    (log! log "message 01")
    (log! log "message 02")
    (log! log "message 03")
    (call-with-port (open-output-string)
      (lambda (port)
        (log-buffer-iterate
         (log-clear! log)
         (lambda (msg)
           (write (log-message-id msg) port)
           (display (string-append " " (log-message-string msg) "\n") port)))
        (get-output-string port)))))

;; -------------------------------------------------------------------------------------------------

(define (log-writer name cycle-time signal log)
  (th:make-thread
   (lambda ()
     (let loop ((i 0))
       (th:thread-sleep! cycle-time)
       (log! log
        (call-with-port (open-output-string)
          (lambda (port)
            (display "thread-" port) (display name port) (display " " port) (write i port)
            (get-output-string port))))
       (cond
        ((channel-ref signal) (loop (+ 1 i)))
        (else #f))))))

(define (run-log-buffer-test)
  (let*((log (new-log-buffer))
        (signal (new-channel #t))
        (worker
         (lambda (name cycle-time)
           (log-writer name (/ cycle-time) signal log)))
        (thread-A (worker "A" 41))
        (thread-B (worker "B" 43))
        (thread-C (worker "C" 47))
        (dump-log
         (lambda ()
           (th:thread-sleep! 0.20)
           (call-with-port (open-output-string)
             (lambda (port)
               (log-dump! log port)
               (get-output-string port)))))
        (_
         (begin
           (th:thread-start! thread-C)
           (th:thread-start! thread-B)
           (th:thread-start! thread-A)))
        (result01 (dump-log))
        (result02 (dump-log)))
    (channel-update! signal (lambda (_) (values #f #f)))
    (th:thread-join! thread-A)
    (th:thread-join! thread-B)
    (th:thread-join! thread-C)
    (cons result01 result02)))

;; -------------------------------------------------------------------------------------------------

(test-assert
    (equal?
     "1 message 01\n2 message 02\n3 message 03\n"
     (simple-log-test)))


(test-assert
    (let*((result (run-log-buffer-test))
          (result-string (string-append (car result) (cdr result))))
      (and
       (equal?
        (substring 
         (string-append
          "1: \"thread-C 0\"\n"
          "2: \"thread-B 0\"\n"
          "3: \"thread-A 0\"\n"
          "4: \"thread-C 1\"\n"
          "5: \"thread-B 1\"\n"
          "6: \"thread-A 1\"\n"
          "7: \"thread-C 2\"\n"
          "8: \"thread-B 2\"\n"
          "9: \"thread-A 2\"\n"
          "10: \"thread-C 3\"\n"
          "11: \"thread-B 3\"\n"
          "12: \"thread-A 3\"\n"
          "13: \"thread-C 4\"\n"
          "14: \"thread-B 4\"\n"
          "15: \"thread-A 4\"\n"
          "16: \"thread-C 5\"\n"
          "17: \"thread-B 5\"\n"
          "18: \"thread-A 5\"\n"
          "19: \"thread-C 6\"\n"
          "20: \"thread-B 6\"\n"
          "21: \"thread-A 6\"\n"
          "22: \"thread-C 7\"\n"
          "23: \"thread-B 7\"\n"
          "24: \"thread-C 8\"\n"
          "25: \"thread-A 7\"\n"
          "26: \"thread-B 8\"\n"
          "27: \"thread-C 9\"\n"
          "28: \"thread-A 8\"\n"
          "29: \"thread-B 9\"\n"
          "30: \"thread-C 10\"\n"
          "31: \"thread-A 9\"\n"
          "32: \"thread-B 10\"\n"
          "33: \"thread-C 11\"\n"
          "34: \"thread-A 10\"\n"
          "35: \"thread-C 12\"\n"
          "36: \"thread-B 11\"\n"
          "37: \"thread-A 11\"\n"
          "38: \"thread-C 13\"\n"
          "39: \"thread-B 12\"\n"
          "40: \"thread-A 12\"\n"
          "41: \"thread-C 14\"\n"
          "42: \"thread-B 13\"\n"
          "43: \"thread-A 13\"\n"
          "44: \"thread-C 15\"\n"
          "45: \"thread-B 14\"\n"
          "46: \"thread-C 16\"\n"
          "47: \"thread-A 14\"\n"
          "48: \"thread-B 15\"\n"
          "49: \"thread-C 17\"\n"
          "50: \"thread-A 15\"\n")
         0 500)
        (substring result-string 0 500))
       (> (string-length result-string) 800))))

;; -------------------------------------------------------------------------------------------------

(test-end "schemacs_concurrent")

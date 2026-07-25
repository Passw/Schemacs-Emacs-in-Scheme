(import
 (scheme base)
 (schemacs editor engine)
 (only (srfi 64) test-assert test-equal test-begin test-end)
 )

;; **Sample text**
;;
;; Welcome to Schemacs, an application platform written in, and
;; programmable in, the Scheme programming language and providing
;; compatibility with GNU Emacs applications. The goals of this
;; project are
;;
;;   - to use the latest R7RS language standard
;;
;;   - to grow the Scheme software ecosystem
;;
;;   - to provide an integrated development environment (IDE)
;;     for Scheme software that is similar to GNU Emacs
;;
;;   - to be able to emulate Emacs applications

(define (strconcat elems)
  (call-with-port (open-output-string)
    (lambda (port)
      (let loop ((elems elems))
        (cond
         ((pair? elems)
          (let ((elem (car elems)))
            (cond
             ((char? elem) (write-char elem port))
             ((string? elem) (write-string elem port))
             (else (write elem port))
             )
            (loop (cdr elems))
            ))
         (else (get-output-string port))
         )))))

(define (find-diff-char str buf)
  ;; Compare a text buffer to a string, return the index of the first
  ;; character where the two strings do not match. Returns three values:
  ;;
  ;;  1. the index where the characters do not match
  ;;  2. the character in the string
  ;;  3. the character in the buffer
  ;;
  ;; If the string and buffer have the same textual content, 1. will
  ;; be the length of the string and buffer (which will be the same)
  ;; and 2. and 3. will be `#f`.
  (let*((strlen (string-length str))
	(buflen (text-editor-char-count buf))
	(len (min strlen buflen))
	)
    ;;(display "strlen=") (write strlen) (display ", buflen=") (write buflen) (newline);;DEBUG
    (let loop ((i 0))
      ;;(display "f[") (write i) (display "]: ");;DEBUG
      (cond
       ((< i len)
	(let ((str-ch (string-ref str i))
	      (buf-ch (text-editor-get-char-index buf i))
	      )
	  ;;(display "s=") (write str-ch) (display ", b=") (write buf-ch) (newline);;DEBUG
	  (cond
	   ((char=? str-ch buf-ch) (loop (+ 1 i)))
	   (else (values i str-ch buf-ch))
	   )))
       ((< i buflen)
	;;(display " end of string\n");;DEBUG
	(values i #f (text-editor-get-char-index buf i))
	)
       ((< i strlen)
	;;(display " end of buffer\n");;DEBUG
	(values i (string-ref str i) #f)
	)
       (else (values i #f #f))
       ))))

;;--------------------------------------------------------------------
;; 0. Constant definitions

(define *sample-text-1*
  '("Welcome to Schemacs," " an application platform written" " in, and" #\newline
    "programmable in," " the Scheme" " programming language" " and"
    " providing\ncompatibility with" " GNU Emacs applications." " The goals of"
    " this\nproject are:" #\newline #\newline
    "  - to use" " the latest" " R7RS" " language standard" #\newline #\newline
    "  - to grow the Scheme software ecosystem" #\newline #\newline
    "  - to provide an integrated" " development environment" " (IDE)" #\newline
    "    for Scheme software that" " is similar to GNU Emacs" #\newline #\newline
    "  - to be able" " to emulate" " Emacs applications\n"
   ))

(define *sample-string-1* (strconcat *sample-text-1*))

;;--------------------------------------------------------------------

(test-begin "schemacs_editor_engine")

;;--------------------------------------------------------------------
;; 1. record and playback: the simplest test
;;
;; Just checks that the most basic functionality works. Inserts a list
;; of strings and characters into the text editor buffer, moves the
;; cursor to the beginning, dumps the buffer back to a stirng, compares
;; the string 

(define ed (new-text-editor))

(define (gb-insert-list ed elems)
  (let loop ((elems elems))
    (cond
     ((pair? elems)
      ;; Write the sample text into the text editor buffer.
      (text-editor-insert ed (car elems))
      (loop (cdr elems))
      )
     (else
      (text-editor-to-string ed)
      ))))

(test-assert
  (let*((ed-str (gb-insert-list ed *sample-text-1*))
	(cat-str *sample-string-1*)
	)
    (cond
     ((string=? ed-str cat-str) #t)
     (else
      (display "test 1 \"record and playback\": strings not equal\n")
      (display "-----------------------\n")
      (display "original string:\n")
      (display "-----------------------\n")
      (display cat-str) (newline)
      (display "-----------------------\n")
      (display "editor string:\n")
      (display "-----------------------\n")
      (display ed-str) (newline)
      (display "-----------------------\n")
      #f))))

;;--------------------------------------------------------------------
;; 2. index-by-index comparison
;;
;; Checks if the character indexing functions are working. This tests
;; a number of features related to tracking the length of lines.

(test-assert
 (let-values
     (((index str-ch buf-ch)
       (find-diff-char *sample-string-1* ed)
       ))
   (not (or str-ch buf-ch))
   ))

;;--------------------------------------------------------------------
(test-end "schemacs_editor_engine")

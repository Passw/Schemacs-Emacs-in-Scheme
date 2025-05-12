(define-library (gypsum pretty)
  ;; A tiny monadic pretty printer. Example usage:
  ;;
  ;; #+BEGIN_SRC scheme
  ;;   (pretty
  ;;    (print
  ;;     "input string: "
  ;;     (lambda ()
  ;;       (let ((prompt-result (read-line)))
  ;;         (print
  ;;          "you entered: " (qstr prompt-result) (line-break)
  ;;          (repeat 80 #\-) (line-break)
  ;;          (indent-by 3
  ;;           (repeat 30 (repeat 8 #\space prompt-result))
  ;;           (line-break)))))))
  ;; #+END_SRC
  ;;
  ;; The `PRETTY` procedure runs the monad. The monad is constructed
  ;; with the `PRINT`, `INDENT`, and `LINE-BREAK` procedures and their
  ;; derivatives: `REPEAT`, `JOIN-BY`, `JOIN-LINES`, `BRACKETED`.
  ;;
  ;; `PRETTY` takes a state data structure which decides how to output
  ;; lines of text. To write to a PORT using the `PRINT-TO-PORT`
  ;; procedure to construct a state that prints to a Scheme
  ;; `OUTPUT-PORT`. Use `PRINT-TO-BUFFER` to print text to a line
  ;; buffer. The `PRETTY` function also follows semantics similar to
  ;; the SRFI-48 `FORMAT` procedure, so can take #t as its first
  ;; argument to output to the Scheme `CURRENT-OUTPUT-PORT`, or #f as
  ;; its first argument to write all text into a single string object.
  ;;
  ;; The `<PP-LINE-BUFFER>` stores lines of text in a
  ;; vector. Construct one with `NEW-LINE-BUFFER`, or with
  ;; `PRINT-TO-BUFFER`. Iterate over a line buffer using
  ;; `PP-FOLD-LINES`. Each line in the buffer is a data structure of
  ;; `<PP-LINE-TYPE>`, which contains an indent level, an indentation
  ;; fill character, and a string of text for that line of text.
  ;;------------------------------------------------------------------
  (import
    (scheme base)
    (scheme write)
    (scheme case-lambda)
    (only (gypsum lens)
          record-unit-lens update)
    (only (gypsum lens vector)
          mutable-vector-type? new-mutable-vector
          mutable-vector-append! mutable-vector-fold/index)
    )
  (export
   pp-type?

   pp-quoted-type? qstr

   print repeat indent-by line-break
   newline-indent join-by join-lines bracketed
   form force-write-indent

   pp-state-type?
   pretty print-to-buffer print-to-port pp-state-line-buffer
   make<pp-state> pp-state-output-port
   pp-state-indent pp-state-indent-char

   pp-line-buffer-type?
   new-line-buffer pp-line-buffer-append! pp-fold-lines
   pp-line-buffer-stats pp-line-buffer-recompute-stats!
   *default-line-buffer-init-size*

   pp-line-type? pp-line  pp-display-line
   pp-line-indent pp-line-indent-char pp-line-string
   empty-line?

    new-line-buffer-stats
   pp-line-buffer-zero-count pp-line-buffer-char-count
   pp-line-buffer-min-len    pp-line-buffer-max-len
   pp-line-buffer-min-indent pp-line-buffer-max-indent
   )
  (include "pretty.scm")
  )

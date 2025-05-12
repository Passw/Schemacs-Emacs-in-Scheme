(define-library (schemacs elisp-eval pretty)
  ;; This library defines an extension to `(schemacs pretty)`
  ;; with rules specific to pretty-printing Emacs Lisp.
  (import
    (scheme base)
    (scheme write)
    (scheme case-lambda)
    (only (chibi match) match)
    (only (schemacs pretty)
          pretty   print   qstr   pp-type?
          indent-by   newline-indent   line-break
          bracketed   form   join-by   join-lines
          )
    )
  (export elisp-pretty elisp-print)
  (include "pretty.scm")
  )

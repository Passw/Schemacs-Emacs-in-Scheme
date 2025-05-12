;; This file contains extensions to Guile's own Emacs Lisp compiler
;; defined in the (language elisp spec) module, which comes included
;; with most all Guile-3 distributions.
;;
;; The Emacs Lisp compiler is extended by binding the values that are
;; defined in this module to various `defun` and `defvar` symbols in
;; the Emacs Lisp environment. It should be loaded automatically when
;; the Emacs Lisp interpreter is initialized by Gypsum.
;;--------------------------------------------------------------------

(define (new-elisp-env) (default-environment elisp))

(define *the-elisp-environment* (make-parameter (new-elisp-env)))

(define eval-elisp-string
  (case-lambda
    ((str)
     (eval-elisp-string str (*the-elisp-environment*))
     )
    ((str env)
     (eval-string str #:lang elisp #:module env)
     )
    ))

(define eval-elisp
  (case-lambda
    ((expr)
     (eval-elisp expr (*the-elisp-environment*))
     )
    ((expr env)
      ;; imported elisp language object from `(GYPSUM ELISP-EVAL SPEC)`, use it here
     (compile expr #:env env #:from elisp #:to 'value))
    ))

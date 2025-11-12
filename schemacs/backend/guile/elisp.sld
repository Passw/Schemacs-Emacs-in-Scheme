(define-library (schemacs backend guile elisp)
  (import
    (scheme base)
    (scheme case-lambda)
    (only (system base language) default-environment)
    (only (system base compile) compile)
    (only (ice-9 eval-string) eval-string)
    (only (schemacs elisp-eval spec) elisp)
    (only (schemacs elisp-eval compile-tree-il) compile-tree-il)
   )

  (export
   *the-elisp-environment*
   new-elisp-env
   eval-elisp
   eval-elisp-string
   )

  (begin
    ;; This file contains extensions to Guile's own Emacs Lisp compiler
    ;; defined in the (language elisp spec) module, which comes included
    ;; with most all Guile-3 distributions.
    ;;
    ;; The Emacs Lisp compiler is extended by binding the values that are
    ;; defined in this module to various `defun` and `defvar` symbols in
    ;; the Emacs Lisp environment. It should be loaded automatically when
    ;; the Emacs Lisp interpreter is initialized by Schemacs.
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
         )))

    (define eval-elisp
      (case-lambda
        ((expr)
         (eval-elisp expr (*the-elisp-environment*))
         )
        ((expr env)
          ;; imported elisp language object from `(SCHEMACS ELISP-EVAL SPEC)`, use it here
         (compile expr #:env env #:from elisp #:to 'value))
        ))

    ;;----------------------------------------------------------------
    ))


